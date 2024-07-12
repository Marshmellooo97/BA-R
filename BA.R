# readr package
library(readr)
library(dplyr)
library(tidyr)  # Paket laden, das die Funktion unnest bereitstellt

print("Hallo")

# Daten einlesen
pfadData <- "../Testdaten/KiData.csv"
data <- read_delim(pfadData, locale = locale(encoding = "ISO-8859-1"), delim = ";", quote = "\"")
data <- data %>%
  select(-UPPER_LIMIT, -LOWER_LIMIT, -NOMINAL, -TOLERANCE, -PART_NUMBER, -STATION_NUMBER, -WORKORDER_ID, -STATION_DESC, -WORKORDER_DESC)

unique_counts <- sapply(data, n_distinct)
print(unique_counts)



# Alle MEASURE_STEP_NUMBER pro BOOKING_ID herausfinden
measure_steps_per_booking <- data %>%
  group_by(BOOKING_ID) %>%
  summarize(MEASURE_STEP_NUMBERS = list(unique(MEASURE_STEP_NUMBER))) %>%
  unnest(cols = MEASURE_STEP_NUMBERS)

print(measure_steps_per_booking)

# Prüfen, ob es doppelte MEASURE_STEP_NUMBER pro BOOKING_ID gibt
duplicates <- data %>%
  group_by(BOOKING_ID, MEASURE_STEP_NUMBER) %>%
  summarize(count = n(), .groups = 'drop') %>%
  filter(count > -1)

print(duplicates)



measure_names_per_step <- data %>%
  group_by(MEASURE_STEP_NUMBER) %>%
  summarize(MEASURE_NAMES = list(unique(MEASURE_NAME))) %>%
  unnest(cols = MEASURE_NAMES)

print(measure_names_per_step)

measure_name_step_count <- data %>%
  group_by(MEASURE_NAME) %>%
  summarize(Distinct_Measure_Step_Count = n_distinct(MEASURE_STEP_NUMBER))

print(measure_name_step_count)

measure_steps_per_booking <- data %>%
  group_by(BOOKING_ID) %>%
  summarize(
    Measure_Step_Count = n_distinct(MEASURE_STEP_NUMBER),
    Measure_Step_Numbers = list(unique(MEASURE_STEP_NUMBER))
  ) %>%
  unnest(cols = Measure_Step_Numbers)

print(measure_steps_per_booking)


measure_step_count_per_booking <- data %>%
  group_by(BOOKING_ID) %>%
  summarize(Measure_Step_Count = n_distinct(MEASURE_STEP_NUMBER))


print(measure_step_count_per_booking)

measure_step_count_frequency <- measure_step_count_per_booking %>%
  group_by(Measure_Step_Count) %>%
  summarize(Frequency = n())

print(measure_step_count_frequency)


