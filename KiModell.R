library(readr)
library(dplyr)
library(tidyr)
library(caret)

library(rpart)  # Für Entscheidungsbäume

library(ggplot2)  # Für die Visualisierung
library(e1071)  # Wird von 'caret' benötigt

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



x <- final_df2_loaded[, !names(final_df2_loaded) %in% "Gesamt_MEASURE_FAIL_CODE"]
y <- final_df2_loaded$Gesamt_MEASURE_FAIL_CODE




set.seed(123)
trainIndex <- caret::createDataPartition(y, p = 0.7, list = FALSE)
xTrain <- x[trainIndex,]
xTest <- x[-trainIndex,]
yTrain <- y[trainIndex]
yTest <- y[-trainIndex]








newScaler <- caret::preProcess(xTrain, method = 'range')
xTrain <- predict(newScaler, xTrain)
xTest <- predict(newScaler, xTest)


write.csv(xTrain, file = "/home/justin.simon/repos/BA/Testdaten/xTrain.csv", row.names = FALSE)
write.csv(xTest, file = "/home/justin.simon/repos/BA/Testdaten/xTest.csv", row.names = FALSE)
write.csv(yTrain, file = "/home/justin.simon/repos/BA/Testdaten/yTrain.csv", row.names = FALSE)
write.csv(yTest, file = "/home/justin.simon/repos/BA/Testdaten/yTest.csv", row.names = FALSE)


xTrain <- as.data.frame(lapply(xTrain, as.numeric))
yTrain <- as.factor(yTrain)

model <- train(
	x = xTrain, 
	y = yTrain,
	method = 'mlp',
	trControl = trainControl(
		method = 'cv', 
		number = 2, 
		verboseIter = TRUE  # Zeigt Fortschrittsmeldungen in der Konsole an
	),
	tuneGrid = expand.grid(
		size = c(20, 50)
	),
	verbose = TRUE
)



model_rf <- train(
	x = xTrain, 
	y = yTrain,
	method = 'rf',
	trControl = trainControl(
		method = 'cv', 
		number = 5, 
		verboseIter = TRUE
	),
	tuneGrid = expand.grid(
		mtry = c(10,15,20,25)  # Anpassen basierend auf der Anzahl der Features
	),
	verbose = TRUE
)

model_rpart <- train(
	x = xTrain, 
	y = yTrain,
	method = 'rpart',  # Methode für Decision Tree
	trControl = trainControl(
		method = 'cv', 
		number = 5,  # Hier die Anzahl der Folds festlegen
		verboseIter = TRUE  # Zeigt Fortschrittsmeldungen in der Konsole an
	),
	tuneGrid = expand.grid(
		cp = c(0.001, 0.01, 0.05, 0.1)  # Tuning-Parameter für den Decision Tree (Komplexität)
	)
)



model_gbm <- train(
	x = xTrain, 
	y = yTrain,
	method = 'gbm',
	trControl = trainControl(
		method = 'cv', 
		number = 10, 
		verboseIter = TRUE
	),
	tuneGrid = expand.grid(
		n.trees = c(100, 200, 300),
		interaction.depth = c(1, 3, 5),
		shrinkage = c(0.01, 0.1),
		n.minobsinnode = c(10, 20)
	),
	verbose = TRUE
)

vorhersage_rf <- predict(model_rf, xTest, type = "raw")
vorhersage_rf <- ifelse(vorhersage_rf >= 0.5, 1, 0)

yTest <- factor(yTest, levels = c(0, 1))
vorhersage_rf <- factor(vorhersage_rf, levels = c(0, 1))

# Erstellen der Verwirrungsmatrix
conf_matrix <- confusionMatrix(vorhersage_rf, yTest)
# Anzeige der Verwirrungsmatrix
print(conf_matrix)



vorhersage_rp <- predict(model_rpart, xTest, type = "raw")
vorhersage_rp <- ifelse(vorhersage_rp >= 0.5, 1, 0)

yTest <- factor(yTest, levels = c(0, 1))
vorhersage_rp <- factor(vorhersage_rp, levels = c(0, 1))

# Erstellen der Verwirrungsmatrix
conf_matrix <- confusionMatrix(vorhersage_rp, yTest)
# Anzeige der Verwirrungsmatrix
print(conf_matrix)




vorhersage_mlp <- predict(model, xTest, type = "raw")
vorhersage_mlp <- ifelse(vorhersage_rp >= 0.5, 1, 0)

yTest <- factor(yTest, levels = c(0, 1))
vorhersage_mlp <- factor(vorhersage_mlp, levels = c(0, 1))

# Erstellen der Verwirrungsmatrix
conf_matrix <- confusionMatrix(vorhersage_mlp, yTest)
# Anzeige der Verwirrungsmatrix
print(conf_matrix)

print(vorhersage_mlp)

x_train = xTrain
x_test = xTest
y_train = yTrain
y_test = yTest


# Modell erstellen und trainieren
set.seed(101)
modelBaum <- rpart(y_train ~ ., data = x_train, method = "class")

# Vorhersagen treffen
y_pred <- predict(modelBaum, x_test, type = "class")

# Modell bewerten: Accuracy
accuracy <- mean(y_pred == y_test)
cat("Accuracy:", accuracy, "\n")

# Classification Report (Precision, Recall, F1)
report <- confusionMatrix(factor(y_pred), factor(y_test))
cat("Classification Report:\n")
print(report)

# Confusion Matrix
conf_matrix <- report$table
cat("Confusion Matrix:\n")
print(conf_matrix)

# Confusion Matrix visualisieren
ggplot(data = as.data.frame(conf_matrix), aes(x = Prediction, y = Reference)) +
	geom_tile(aes(fill = Freq), color = "white") +
	scale_fill_gradient(low = "white", high = "blue") +
	geom_text(aes(label = Freq), vjust = 1) +
	labs(x = "Predicted Label", y = "True Label", title = "Confusion Matrix") +
	theme_minimal()
