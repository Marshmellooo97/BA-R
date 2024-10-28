library(pryr)       # Für Speicherverbrauch
library(tictoc)     # Für Zeitmessung (reale Zeit)
library(data.table) # Für die Datenspeicherung als Tabelle
library(readr)
library(dplyr)
library(tidyr)
library(caret)
library(rpart)
library(profvis)



# Beispiel für deine Funktionen:
TimeDatenvorbereitung <- function() {
	print("TimeDatenvorbereitung")
	rm(list = ls())
	# Einlesen der Daten und Entfernen unnötiger Spalten
	pfadData <- "../Testdaten/KiData.csv"
	data <- read_delim(pfadData, locale = locale(encoding = "ISO-8859-1"), delim = ";", quote = "\"") %>%
		select(-UPPER_LIMIT, -LOWER_LIMIT, -NOMINAL, -TOLERANCE, -PART_NUMBER, -STATION_NUMBER, -WORKORDER_ID, -STATION_DESC, -WORKORDER_DESC)
	
	# Berechnung der eindeutigen Werte pro Spalte und Ausgabe
	unique_counts <- sapply(data, n_distinct)
	
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
	
	# Wandelt den Inhalt in Zahlen um Letzten drei Ziffern / Buchstaben werden abgeschnitten
	final_df2$WORKORDER_NUMBER <- as.numeric(substr(final_df2$WORKORDER_NUMBER, 1, nchar(final_df2$WORKORDER_NUMBER) - 3))
	
	final_df2_cleaned <- final_df2
	
	final_df2_cleaned <- final_df2_cleaned[, !names(final_df2_cleaned) %in% c("BOOK_DATE", "BOOKING_ID")]
	
	convert_to_numeric <- function(df) {
		df <- as.data.frame(lapply(df, function(x) {
			# Ersetze Kommas durch Punkte
			x <- gsub(",", ".", x)
			
			# Wandle den Text in numerische Werte um
			num_x <- as.numeric(x)
			
			# Überprüfe, ob die Umwandlung fehlgeschlagen ist und versuche es als numerische Umwandlung
			if (any(is.na(num_x)) && !all(is.na(x))) {
				num_x <- as.numeric(as.character(x))
			}
			
			return(num_x)
		}))
		return(df)
	}
	
	# Wandle alle Spalten in numerische Werte um
	final_df2_cleaned <- convert_to_numeric(final_df2_cleaned)
	
	write.csv(final_df2_cleaned, file = "/home/justin.simon/repos/BA/Testdaten/final_df2_cleaned.csv", row.names = FALSE)
	cat("DataFrame wurde in '/home/justin.simon/repos/BA/Testdaten/final_df2_cleaned.csv' gespeichert.\n")
}



# Globale Variablen definieren
xTrain <<- NULL
xTest <<- NULL
yTrain <<- NULL
yTest <<- NULL
modelBaum <<- NULL

BaumDatenvorbereitung <- function() {
	print("BaumDatenvorbereitung")
	rm(list = ls())
	file_path <- "/home/justin.simon/repos/BA/Testdaten/final_df2_cleaned.csv"
	
	# CSV-Datei als DataFrame laden
	final_df2_loaded <- read.csv(file_path, stringsAsFactors = FALSE)
	
	convert_to_numeric <- function(df) {
		df <- as.data.frame(lapply(df, function(x) {
			# Ersetze Kommas durch Punkte
			x <- gsub(",", ".", x)
			
			# Wandle den Text in numerische Werte um
			num_x <- as.numeric(x)
			
			# Überprüfe, ob die Umwandlung fehlgeschlagen ist und versuche es als numerische Umwandlung
			if (any(is.na(num_x)) && !all(is.na(x))) {
				num_x <- as.numeric(as.character(x))
			}
			
			return(num_x)
		}))
		return(df)
	}
	
	# Wandle alle Spalten in numerische Werte um
	final_df2_loaded <- convert_to_numeric(final_df2_loaded)
	
	x <<- final_df2_loaded[, !names(final_df2_loaded) %in% "Gesamt_MEASURE_FAIL_CODE"]
	y <<- final_df2_loaded$Gesamt_MEASURE_FAIL_CODE
	
	set.seed(123)
	trainIndex <- caret::createDataPartition(y, p = 0.7, list = FALSE)
	xTrain <<- x[trainIndex,]
	xTest <<- x[-trainIndex,]
	yTrain <<- y[trainIndex]
	yTest <<- y[-trainIndex]
}

BaumModellTrainieren <- function() {
	print("BaumModellTrainieren")
	# Modell erstellen und trainieren
	set.seed(101)
	modelBaum <<- rpart(yTrain ~ ., data = xTrain, method = "class")
}

BaumVorhersagen <- function() {
	print("BaumVorhersagen")
	y_pred <- predict(modelBaum, xTest, type = "class")
	return(y_pred)
}






# Globale Variablen definieren
xTrain <<- NULL
xTest <<- NULL
yTrain <<- NULL
yTest <<- NULL
model <<- NULL  # Modell für das MLP

MLPDatenvorbereitung <- function() {
	print("MLPDatenvorbereitung")
	rm(list = ls())
	file_path <- "/home/justin.simon/repos/BA/Testdaten/final_df2_cleaned.csv"
	
	# CSV-Datei als DataFrame laden
	final_df2_loaded <- read.csv(file_path, stringsAsFactors = FALSE)
	
	final_df2_loaded <- final_df2_loaded %>%
		mutate(across(everything(), ~ ifelse(is.na(.), -1, .)))
	
	convert_to_numeric <- function(df) {
		df <- as.data.frame(lapply(df, function(x) {
			# Ersetze Kommas durch Punkte
			x <- gsub(",", ".", x)
			
			# Wandle den Text in numerische Werte um
			num_x <- as.numeric(x)
			
			# Überprüfe, ob die Umwandlung fehlgeschlagen ist und versuche es als numerische Umwandlung
			if (any(is.na(num_x)) && !all(is.na(x))) {
				num_x <- as.numeric(as.character(x))
			}
			
			return(num_x)
		}))
		return(df)
	}
	
	# Wandle alle Spalten in numerische Werte um
	final_df2_loaded <- convert_to_numeric(final_df2_loaded)
	
	x <<- final_df2_loaded[, !names(final_df2_loaded) %in% "Gesamt_MEASURE_FAIL_CODE"]
	y <<- final_df2_loaded$Gesamt_MEASURE_FAIL_CODE
	
	set.seed(123)
	trainIndex <- caret::createDataPartition(y, p = 0.7, list = FALSE)
	xTrain <<- x[trainIndex,]
	xTest <<- x[-trainIndex,]
	yTrain <<- y[trainIndex]
	yTest <<- y[-trainIndex]
	
	newScaler <- caret::preProcess(xTrain, method = 'range')
	xTrain <<- predict(newScaler, xTrain)
	xTest <<- predict(newScaler, xTest)
	
	xTrain <<- as.data.frame(lapply(xTrain, as.numeric))
	yTrain <<- as.factor(yTrain)
	
	colnames(xTrain) <<- 1:ncol(xTrain)
	colnames(xTest) <<- 1:ncol(xTest)
}

MLPModellTrainieren <- function() {
	print("MLPModellTrainieren")
	model <<- train(
		x = xTrain, 
		y = yTrain,
		method = 'mlp',
		trControl = trainControl(
			method = 'none' #, #'cv'
			# number = 2, 
			# verboseIter = TRUE 
		),
		tuneGrid = expand.grid(
			size = c(389)
		),
		verbose = TRUE
	)
}

MLPVorhersagen <- function() {
	print("MLPVorhersagen")
	predictions <- predict(model, newdata = xTest, type = "raw")
	y_pred <<- as.numeric(as.character(predictions))
	return(y_pred)
}






library(microbenchmark)
library(bench)


# Liste der Funktionen, die ausgeführt und gemessen werden sollen
#functions_to_run <- list(
#list(func = TimeDatenvorbereitung, name = "TimeDatenvorbereitung"),
#list(func = BaumDatenvorbereitung, name = "BaumDatenvorbereitung"),
#list(func = BaumModell, name = "BaumModell"),
#list(func = BaumModellTrainieren, name = "BaumModellTrainieren"),
#list(func = BaumVorhersagen, name = "BaumVorhersagen"),
#list(func = MLPDatenvorbereitung, name = "MLPDatenvorbereitung")
#list(func = MLPModell, name = "MLPModell"),
#list(func = MLPModellTrainieren, name = "MLPModellTrainieren"),
#list(func = MLPVorhersagen, name = "MLPVorhersagen")
#)





measure_performance <- function(func, func_name) {
	gc()
	# Messen mit bench
	bench_result <- bench::mark(
		func(),
		iterations = 1
	)
	
	# Messen mit microbenchmark für die CPU-Zeit
	microbenchmark_result <- microbenchmark(func(), times = 1)
	
	print(bench_result)
	print(microbenchmark_result)
	# Extrahiere CPU-Zeit, reale Zeit und Speicherverbrauch
	cpu_time <- as.numeric(microbenchmark_result$time) / 1e6 / 1000 # CPU-Zeit in Sekunden (nanoseconds -> seconds)
	real_time <- as.numeric(bench_result$total_time)    # Reale Zeit in Sekunden
	memory <- as.numeric(bench_result$mem_alloc) / (1024^3)    # Speicherverbrauch in GB
	
	# Rückgabe eines Dataframes mit den Messergebnissen
	return(data.frame(
		Function = func_name,
		CPU_Time = format(cpu_time, scientific = FALSE),  # Formatierung ohne wissenschaftliche Notation
		Time = format(real_time, scientific = FALSE),     # Formatierung ohne wissenschaftliche Notation
		RAM = format(memory, scientific = FALSE)          # Formatierung ohne wissenschaftliche Notation
	))
}


# Liste aller Funktionen und deren Namen
functions <- list(
	"TimeDatenvorbereitung" = TimeDatenvorbereitung,
	"BaumDatenvorbereitung" = BaumDatenvorbereitung,
	"BaumModellTrainieren" = BaumModellTrainieren,
	"BaumVorhersagen" = BaumVorhersagen,
	"MLPDatenvorbereitung" = MLPDatenvorbereitung,
	"MLPModellTrainieren" = MLPModellTrainieren,
	"MLPVorhersagen" = MLPVorhersagen
)

# Initialisiere einen leeren Dataframe für die Ergebnisse
results <- data.frame()

# Für jede Funktion die Performance messen und die Ergebnisse zum Dataframe hinzufügen
for (func_name in names(functions)) {
	func <- functions[[func_name]]
	result <- measure_performance(func, func_name)
	results <- rbind(results, result)
}

# Ergebnisse anzeigen
print(results)

# DataFrame aus den Ergebnissen erstellen
results_df <- data.frame(results)

# Ausgabe des DataFrames
print(results_df)

# Optional: Ergebnisse in eine CSV-Datei speichern
write.csv(results_df, "performance_results.csv", row.names = FALSE)

