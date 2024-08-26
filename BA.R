library(readr)
library(dplyr)
library(tidyr)
library(caret)

# Einlesen der Daten und Entfernen unnötiger Spalten
pfadData <- "../Testdaten/KiData.csv"
data <- read_delim(pfadData, locale = locale(encoding = "ISO-8859-1"), delim = ";", quote = "\"") %>%
  select(-UPPER_LIMIT, -LOWER_LIMIT, -NOMINAL, -TOLERANCE, -PART_NUMBER, -STATION_NUMBER, -WORKORDER_ID, -STATION_DESC, -WORKORDER_DESC)

# Berechnung der eindeutigen Werte pro Spalte und Ausgabe
unique_counts <- sapply(data, n_distinct)
print(unique_counts)

# Berechnung des Gesamt_MEASURE_FAIL_CODE pro BOOKING_ID
measure_fail_code_per_booking <- data %>%
  group_by(BOOKING_ID) %>%
  summarize(Gesamt_MEASURE_FAIL_CODE = ifelse(any(MEASURE_FAIL_CODE == 1), 1, 0))


# Filtern der MEASURE_NAMEs, die mindestens 1000 Einträge haben
valid_measure_names <- data %>%
  group_by(MEASURE_NAME) %>%
  summarize(Frequency = n()) %>%
  filter(Frequency >= 1000) %>%
  pull(MEASURE_NAME)


# Filterung der Daten auf valide MEASURE_NAMEs
data_filtered <- data %>%
  filter(MEASURE_NAME %in% valid_measure_names)

# Korrektur der Dezimaltrennzeichen und Überprüfung der Konvertierbarkeit zu numerischen Werten
data_with_numeric_check <- data_filtered %>%
  mutate(MEASURE_VALUE = gsub(",", ".", MEASURE_VALUE),
         CanConvertToNumeric = !is.na(as.numeric(MEASURE_VALUE)))

# Identifizierung nicht-numerischer MEASURE_VALUEs und deren Anzahl pro MEASURE_NAME
invalid_measure_values <- data_with_numeric_check %>%
  filter(!CanConvertToNumeric) %>%
  select(MEASURE_NAME, MEASURE_VALUE) %>%
  distinct()

invalid_measure_values_count <- invalid_measure_values %>%
  group_by(MEASURE_NAME) %>%
  summarize(Count = n_distinct(MEASURE_VALUE),  # Anzahl der einzigartigen MEASURE_VALUEs
            Variants = list(unique(MEASURE_VALUE)))  # Varianten als Liste speichern

# Filtern der MEASURE_NAMEs nach Anzahl der Varianten (zwischen 2 und 20)
valid_measure_names <- invalid_measure_values_count %>%
  filter(Count >= 2 & Count <= 20) 
invalid_measure_names <- invalid_measure_values_count %>%
  filter(Count < 2 | Count > 20)

# Filterung der Daten auf valide MEASURE_NAMEs
data_filtered2 <- data_filtered %>%
  filter(!MEASURE_NAME %in% invalid_measure_names$MEASURE_NAME)



# Pivotieren der Daten, Hinzufügen von Zusatzinformationen und Zusammenführen
data_wide <- data_filtered2 %>%
  select(BOOKING_ID, MEASURE_NAME, MEASURE_VALUE) %>%
  pivot_wider(names_from = MEASURE_NAME, values_from = MEASURE_VALUE)

additional_info <- data_filtered2 %>%
  group_by(BOOKING_ID) %>%
  summarize(
    STATION_ID = unique(STATION_ID),
    WORKORDER_NUMBER = unique(WORKORDER_NUMBER),
    SEQUENCE_NUMBER = unique(SEQUENCE_NUMBER),
    BOOK_DATE = unique(BOOK_DATE),
    MEASURE_TYPE = list(unique(MEASURE_TYPE))
  )

final_df <- data_wide %>%
  left_join(measure_fail_code_per_booking, by = "BOOKING_ID") %>%
  left_join(additional_info, by = "BOOKING_ID") 




if ("ComputerName" %in% names(final_df)) {
  print("Die Spalte 'ComputerName' existiert im Dataframe.")
} else {
  print("Die Spalte 'ComputerName' existiert nicht im Dataframe.")
}


formula <- ~ ComputerName + DMM_SN + `Messergebnis DMC`

dummies <- dummyVars(formula, data = final_df)
one_hot_encoded <- predict(dummies, newdata = final_df)
one_hot_encoded_df <- as.data.frame(one_hot_encoded)
one_hot_encoded_df[is.na(one_hot_encoded_df)] <- 0
final_df <- cbind(final_df, one_hot_encoded_df)




final_df2 <- final_df %>%
  mutate(
    D = ifelse(grepl("D", MEASURE_TYPE), 1, 0),
    T = ifelse(grepl("T", MEASURE_TYPE), 1, 0)
  ) %>%
  select(-MEASURE_TYPE)


final_df2 <- final_df2[, !(names(final_df2) %in% c("ComputerName", "DMM_SN", "Messergebnis DMC"))]


final_df2$WORKORDER_NUMBER <- as.numeric(substr(final_df2$WORKORDER_NUMBER, 1, nchar(final_df2$WORKORDER_NUMBER) - 3))

contains_negative_one <- any(final_df2 == -1)
contains_negative_one
count_negative_one <- sum(final_df2 <= 0, na.rm = TRUE)
count_negative_one









rows_to_remove <- apply(final_df2, 1, function(row) any(row == -1, na.rm = TRUE))

# Entfernen der betroffenen Zeilen
final_df2_cleaned <- final_df2[!rows_to_remove, ]

# Ergebnis anzeigen
print(final_df2_cleaned)








count_negative_if_na <- function(column) {
  if (any(is.na(column))) {
    return(sum(column == -1, na.rm = TRUE))
  } else {
    return(0)
  }
}

# Zähler für jede Spalte initialisieren
negative_counts <- sapply(final_df2_cleaned, count_negative_if_na)

# In DataFrame umwandeln
result_df <- data.frame(Spalte = names(negative_counts), Zähler = negative_counts)

# Ergebnis anzeigen
print(result_df)






final_df2_cleaned <- final_df2_cleaned %>%
  mutate(across(everything(), ~ ifelse(is.na(.), -1, .)))




write.csv(final_df2_cleaned, file = "/home/justin.simon/repos/BA/Testdaten/final_df2_cleaned.csv", row.names = FALSE)

# Bestätigung ausgeben
cat("DataFrame wurde in '/home/justin.simon/repos/BA/Testdaten/final_df2_cleaned.csv' gespeichert.\n")





