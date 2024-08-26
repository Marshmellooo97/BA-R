library(readr)
library(dplyr)
library(tidyr)
library(caret)

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

# Überprüfe die Struktur des neuen DataFrames
str(final_df2_loaded)

# Überprüfe, ob noch Nicht-Numerische Werte vorhanden sind
any(is.na(final_df2_loaded))





str(final_df2_numeric)

x <- final_df2_loaded[, !names(final_df2_loaded) %in% "Gesamt_MEASURE_FAIL_CODE"]
y <- final_df2_loaded$Gesamt_MEASURE_FAIL_CODE




set.seed(101)
trainIndex <- caret::createDataPartition(y, p = 0.7, list = FALSE)
xTrain <- x[trainIndex,]
xTest <- x[-trainIndex,]
yTrain <- y[trainIndex]
yTest <- y[-trainIndex]


newScaler <- caret::preProcess(xTrain, method = 'range')
xTrain <- predict(newScaler, xTrain)
xTest <- predict(newScaler, xTest)


# Labels in Datenrahmen umwandeln und Spaltennamen setzen
yTrain <- as.data.frame(yTrain)
yTestScale <- as.data.frame(yTest)
colnames(yTrain) <- 'Gesamt_MEASURE_FAIL_CODE'
colnames(yTestScale) <- 'Gesamt_MEASURE_FAIL_CODE'


# Skalierungsobjekt für Labels erstellen und skalieren
scalerLabels <- caret::preProcess(yTrain, method = 'range')
yTrain <- predict(scalerLabels, yTrain)
yTestScale <- predict(scalerLabels, yTestScale)

yTrain <- unlist(yTrain)
yTestScale <- unlist(yTestScale)


model <- train(
	x = xTrain, 
	y = yTrain,
	method = 'mlp',
	trControl = trainControl(
		method = 'cv', 
		number = 10, 
		verboseIter = TRUE  # Zeigt Fortschrittsmeldungen in der Konsole an
	),
	tuneGrid = expand.grid(
		size = c(100, 200, 400, 600, 800, 1000)
	),
	verbose = TRUE
)
