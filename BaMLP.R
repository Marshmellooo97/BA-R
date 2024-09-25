library(readr)
library(dplyr)
library(tidyr)
library(caret)
library(ggplot2)
library(reshape2)


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

xTrain <- as.data.frame(lapply(xTrain, as.numeric))
yTrain <- as.factor(yTrain)

colnames(xTrain) <- 1:ncol(xTrain)
colnames(xTest) <- 1:ncol(xTest)

model <- train(
	x = xTrain, 
	y = yTrain,
	method = 'mlp',
	trControl = trainControl(
		method = 'none'#, #'cv'
		#number = 2, 
		#verboseIter = TRUE 
	),
	tuneGrid = expand.grid(
		size = c(389)
	),
	verbose = TRUE
)


print(model)

model_details <- model$finalModel
print(model_details)

# Vorhersagen auf den Testdaten erstellen
predictions <- predict(model, newdata = xTest, type = "raw")
y_pred <- as.numeric(as.character(predictions))
# Umwandlung der Wahrscheinlichkeiten in Klassen (bei binärer Klassifikation)
#y_pred <- ifelse(predictions > 0.5, 1, 0)

# Modell bewerten
accuracy <- sum(y_pred == yTest) / length(yTest)
print(paste("Accuracy:", accuracy))

# Klassifikationsbericht erstellen
report <- confusionMatrix(as.factor(y_pred), as.factor(yTest))
print(report)

# Verwirrungsmatrix berechnen
conf_matrix <- table(Predicted = y_pred, Actual = yTest)
print("Confusion Matrix:")
print(conf_matrix)

conf_matrix_melted <- melt(conf_matrix)
ggplot(data = conf_matrix_melted, aes(x = Predicted, y = Actual)) +
	geom_tile(aes(fill = value), color = "white") +
	scale_fill_gradient(low = "lightblue", high = "blue") +
	geom_text(aes(label = value), color = "white", size = 6) +
	labs(title = "Confusion Matrix", x = "Predicted Label", y = "True Label") +
	theme_minimal()

