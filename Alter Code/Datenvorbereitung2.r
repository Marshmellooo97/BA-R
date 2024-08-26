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
print(measure_fail_code_per_booking)

# Filtern der MEASURE_NAMEs, die mindestens 1000 Einträge haben
valid_measure_names <- data %>%
  group_by(MEASURE_NAME) %>%
  summarize(Frequency = n()) %>%
  filter(Frequency >= 1000) %>%
  pull(MEASURE_NAME)
print(valid_measure_names)

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
print(dim(data_filtered2))

# Erstellen des DataFrames mit BOOKING_ID und den neuen Spalten aus Variants
all_columns <- unique(unlist(valid_measure_names$Variants))
new_df <- data.frame(BOOKING_ID = unique(data_filtered2$BOOKING_ID))

for (col in all_columns) {
  new_df[[col]] <- 0
}

# Füllen des DataFrames new_df mit 1 bei Übereinstimmung von BOOKING_ID und MEASURE_VALUE
for (i in 1:nrow(data_filtered2)) {
  booking_id <- data_filtered2$BOOKING_ID[i]
  measure_value <- data_filtered2$MEASURE_VALUE[i]
  
  if (measure_value %in% colnames(new_df)) {
    new_df[new_df$BOOKING_ID == booking_id, measure_value] <- 1
  }
}

# Ausgabe zur Überprüfung
print(new_df)

# Zählen der 1-Werte pro Spalte und insgesamt
ones_per_column <- colSums(new_df[, -1])
print(ones_per_column)
total_ones <- sum(ones_per_column)
cat("Gesamtanzahl der 1-Werte im DataFrame:", total_ones, "\n")

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
  left_join(additional_info, by = "BOOKING_ID") %>%
  left_join(new_df, by = "BOOKING_ID") %>%
  mutate(across(everything(), ~ replace_na(., "-100")))

# Prüfen des Ergebnisses
print(head(final_df))
