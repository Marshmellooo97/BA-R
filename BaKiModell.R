library(readr)
library(dplyr)
library(tidyr)
library(caret)

file_path <- "/home/justin.simon/repos/BA/Testdaten/final_df2_cleaned.csv"

# CSV-Datei als DataFrame laden
final_df2_loaded <- read.csv(file_path, stringsAsFactors = FALSE)


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
