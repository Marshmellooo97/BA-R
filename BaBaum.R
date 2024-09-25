library(readr)
library(dplyr)
library(tidyr)
library(caret)
library(rpart)  
library(rpart.plot)  # Importiere das rpart.plot Paket
library(ggplot2) 

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

x <- final_df2_loaded[, !names(final_df2_loaded) %in% "Gesamt_MEASURE_FAIL_CODE"]
y <- final_df2_loaded$Gesamt_MEASURE_FAIL_CODE

set.seed(123)
trainIndex <- caret::createDataPartition(y, p = 0.7, list = FALSE)
xTrain <- x[trainIndex,]
xTest <- x[-trainIndex,]
yTrain <- y[trainIndex]
yTest <- y[-trainIndex]

# Modell erstellen und trainieren
set.seed(101)
modelBaum <- rpart(yTrain ~ ., data = xTrain, method = "class")

# Vorhersagen treffen
y_pred <- predict(modelBaum, xTest, type = "class")

# Modell bewerten: Accuracy
accuracy <- mean(y_pred == yTest)
cat("Accuracy:", accuracy, "\n")

# Classification Report (Precision, Recall, F1)
report <- confusionMatrix(factor(y_pred), factor(yTest))
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

rpart.plot(modelBaum, 
					 main = "Entscheidungsbaum", 
					 extra = 104,       # Zeigt die Klasse und die Wahrscheinlichkeit an
					 box.palette = "RdBu",  # Farbpalette für die Kästchen
					 shadow.col = "gray",   # Schatten für die Kästchen
					 nn = TRUE)           
