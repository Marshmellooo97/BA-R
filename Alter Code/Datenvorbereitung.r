library(readr)
library(dplyr)
library(tidyr)
library(caret)



# Daten einlesen
pfadData <- "../Testdaten/KiData.csv"
data <- read_delim(pfadData, locale = locale(encoding = "ISO-8859-1"), delim = ";", quote = "\"")
data <- data %>%
  select(-UPPER_LIMIT, -LOWER_LIMIT, -NOMINAL, -TOLERANCE, -PART_NUMBER, -STATION_NUMBER, -WORKORDER_ID, -STATION_DESC, -WORKORDER_DESC)

# Zusätzliche Berechnungen (wenn erforderlich)
unique_counts <- sapply(data, n_distinct)  # Zählen der eindeutigen Werte in jeder Spalte des Datensatzes
print(unique_counts)  # Ausgabe der Anzahl eindeutiger Werte

# Gesammt MEASURE_FAIL_CODE pro BOOKING_ID
measure_fail_code_per_booking <- data %>%
  group_by(BOOKING_ID) %>%
  summarize(Gesamt_MEASURE_FAIL_CODE = ifelse(any(MEASURE_FAIL_CODE == 1), 1, 0))  # Wenn ein MEASURE_FAIL_CODE von 1 vorhanden ist, setze Gesamt_MEASURE_FAIL_CODE auf 1, sonst auf 0
print(measure_fail_code_per_booking)


measure_name_frequency <- data %>%
  group_by(MEASURE_NAME) %>%
  summarize(Frequency = n())  # Gruppieren nach MEASURE_NAME und Zählen der Einträge

# Filtern der MEASURE_NAME, die mindestens 1000 Einträge haben
valid_measure_names <- measure_name_frequency %>%
  filter(Frequency >= 1000) %>%
  pull(MEASURE_NAME)

print(valid_measure_names)

# Daten filtern: nur Einträge behalten, deren MEASURE_NAME in valid_measure_names ist
data_filtered <- data %>%
  filter(MEASURE_NAME %in% valid_measure_names)



# Korrektur der Dezimaltrennzeichen (Komma zu Punkt)
data_filled <- data_filtered %>%
  mutate(MEASURE_VALUE = gsub(",", ".", MEASURE_VALUE))

# Prüfen, ob MEASURE_VALUE in eine Zahl umgewandelt werden kann
data_with_numeric_check <- data_filled %>%
  mutate(CanConvertToNumeric = !is.na(as.numeric(MEASURE_VALUE)))

# Finden der MEASURE_NAME, bei denen mindestens ein MEASURE_VALUE nicht in eine Zahl umgewandelt werden konnte
invalid_measure_values <- data_with_numeric_check %>%
  filter(!CanConvertToNumeric) %>%
  select(MEASURE_NAME, MEASURE_VALUE) %>%
  distinct()

invalid_measure_values_count <- invalid_measure_values %>%
  group_by(MEASURE_NAME) %>%
  summarize(
    Count = n_distinct(MEASURE_VALUE),  # Anzahl der einzigartigen MEASURE_VALUE
    Variants = list(unique(MEASURE_VALUE))  # Speichert die Varianten als Liste von Strings
  )

# Herausziehen der MEASURE_NAME, die nur einen unterschiedlichen nicht-numerischen String haben
invalid_measure_names <- invalid_measure_values_count %>%
  filter(Count < 2 | Count > 20)

valid_measure_names <- invalid_measure_values_count %>%
  filter(Count >= 2 & Count <= 20) 


# Filtere den DataFrame data_filtered
data_filtered2 <- data_filtered %>%
  filter(!MEASURE_NAME %in% invalid_measure_names$MEASURE_NAME)
print(dim(data_filtered2))




# 1. Extrahiere alle Strings aus den Listen in der Variants-Spalte und erstelle eine Liste aller eindeutigen Spaltennamen
all_columns <- unique(unlist(valid_measure_names$Variants))

# 2. Erstelle ein neues DataFrame mit BOOKING_ID und den neuen Spalten aus Variants
new_df <- data.frame(BOOKING_ID = unique(data_filtered2$BOOKING_ID))

for (col in all_columns) {
  new_df[[col]] <- 0
}

# Schritt 1: Durchlaufe jede Zeile von data_filtered2
for (i in 1:nrow(data_filtered2)) {
  booking_id <- data_filtered2$BOOKING_ID[i]
  measure_value <- data_filtered2$MEASURE_VALUE[i]
  
  # Schritt 2: Prüfe, ob measure_value eine Spalte in new_df ist
  if (measure_value %in% colnames(new_df)) {
    # Schritt 3: Setze den Wert auf 1 für die entsprechende BOOKING_ID und Spalte
    new_df[new_df$BOOKING_ID == booking_id, measure_value] <- 1
  }
}



# Ausgabe des neuen DataFrames zur Überprüfung
print(new_df)



# Schritt 1: Zählen der 1-Werte pro Spalte
ones_per_column <- colSums(new_df[, -1])  # Ignoriere die erste Spalte (BOOKING_ID)

# Ausgabe der Anzahl der 1-Werte pro Spalte
print(ones_per_column)

# Schritt 2: Gesamtanzahl der 1-Werte im gesamten DataFrame
total_ones <- sum(ones_per_column)

# Ausgabe der Gesamtanzahl
cat("Gesamtanzahl der 1-Werte im DataFrame:", total_ones, "\n")





# Pivoting der Daten: BOOKING_ID als Zeilen, MEASURE_NAME als Spalten, MEASURE_VALUE als Werte
data_wide <- data_filtered2 %>%
  select(BOOKING_ID, MEASURE_NAME, MEASURE_VALUE) %>%
  pivot_wider(names_from = MEASURE_NAME, values_from = MEASURE_VALUE)  # Pivotieren der Daten, sodass MEASURE_NAME zu Spalten wird und MEASURE_VALUE deren Werte enthält

# Zusätzliche Informationen pro BOOKING_ID extrahieren und als Liste zusammenfassen
additional_info <- data_filtered2 %>%
  group_by(BOOKING_ID) %>%
  summarize(
    STATION_ID = unique(STATION_ID),  # Eindeutige Werte von STATION_ID pro BOOKING_ID
    WORKORDER_NUMBER = unique(WORKORDER_NUMBER),  # Eindeutige Werte von WORKORDER_NUMBER pro BOOKING_ID
    SEQUENCE_NUMBER = unique(SEQUENCE_NUMBER),  # Eindeutige Werte von SEQUENCE_NUMBER pro BOOKING_ID
    BOOK_DATE = unique(BOOK_DATE),  # Eindeutige Werte von BOOK_DATE pro BOOKING_ID
    MEASURE_TYPE = list(unique(MEASURE_TYPE))  # Eindeutige Werte von MEASURE_TYPE pro BOOKING_ID als Liste
  )

# Hinzufügen des Gesamt_MEASURE_FAIL_CODE und der zusätzlichen Informationen zur gepivoteten Tabelle
final_df <- data_wide %>%
  left_join(measure_fail_code_per_booking, by = "BOOKING_ID") %>%
  left_join(additional_info, by = "BOOKING_ID") %>%
  left_join(new_df, by = "BOOKING_ID")
  
 

# Leere Felder mit -100 auffüllen
final_df <- final_df %>%
  mutate(across(everything(), ~ replace_na(., "-100")))

# Prüfe das Ergebnis
print(head(final_df))





