library(pryr)  # Für die Speicherverbrauchsmessung
library(tictoc)  # Für die Zeitmessung
library(dplyr)  # Für die Datenmanipulation

# Funktion zur Messung von Zeit und Speicherverbrauch
measure_performance <- function(func) {
	# Zeitmessung starten
	tic()
	
	# Speicherverbrauch vor der Funktion
	mem_before <- mem_used()
	
	# Funktion ausführen
	func()
	
	# Speicherverbrauch nach der Funktion
	mem_after <- mem_used()
	
	# Zeitmessung stoppen
	exec_time <- toc(quiet = TRUE)
	
	# Gesamtlaufzeit in Sekunden
	total_time <- exec_time$toc - exec_time$tic
	
	# Speicherverbrauch (Differenz) in MB berechnen
	mem_usage <- (mem_after - mem_before) / (1024 * 1024)
	
	return(list(total_time = total_time, mem_usage = mem_usage))
}

# Funktionen, die ausgeführt werden sollen (Platzhalterfunktionen)
TimeDatenvorbereitung <- function() {
	Sys.sleep(0.5)  # Simulation einer Funktion (z.B. Datenvorbereitung)
}

BaumDatenvorbereitung <- function() {
	Sys.sleep(0.3)
}

BaumModell <- function() {
	Sys.sleep(0.7)
}

BaumModellTrainieren <- function() {
	Sys.sleep(1)
}

BaumVorhersagen <- function() {
	Sys.sleep(0.2)
}

MLPDatenvorbereitung <- function() {
	Sys.sleep(0.4)
}

MLPModell <- function() {
	Sys.sleep(0.6)
}

MLPModellTrainieren <- function() {
	Sys.sleep(1.1)
}

MLPVorhersagen <- function() {
	Sys.sleep(0.3)
}

# Liste der Funktionen, die ausgeführt und gemessen werden sollen
functions_to_run <- list(
	list(func = TimeDatenvorbereitung, name = "TimeDatenvorbereitung"),
	list(func = BaumDatenvorbereitung, name = "BaumDatenvorbereitung"),
	list(func = BaumModell, name = "BaumModell"),
	list(func = BaumModellTrainieren, name = "BaumModellTrainieren"),
	list(func = BaumVorhersagen, name = "BaumVorhersagen"),
	list(func = MLPDatenvorbereitung, name = "MLPDatenvorbereitung"),
	list(func = MLPModell, name = "MLPModell"),
	list(func = MLPModellTrainieren, name = "MLPModellTrainieren"),
	list(func = MLPVorhersagen, name = "MLPVorhersagen")
)

# DataFrame zur Speicherung der Ergebnisse
results <- data.frame()

# Schleife über die Funktionen und deren Performance messen
for (func_info in functions_to_run) {
	result <- measure_performance(func_info$func)
	
	results <- rbind(results, data.frame(
		Funktionsname = func_info$name,
		CPU_Zeit_Sekunden = result$total_time,
		Speicherverbrauch_MB = result$mem_usage
	))
}

# Ausgabe des DataFrames
print(results)

# Optional: Ergebnisse in eine CSV-Datei speichern
write.csv(results, file = "performance_results.csv", row.names = FALSE)
