library(rdwd)
library(tidyverse)
library(httr)
library(utils)
library(data.table)  # Added for faster CSV reading
library(readr)  # Added for faster CSV reading with read_csv

## Prints minimal and maximal date of the forecasts
print_info <- function(directory) {
  all_dirs <- list.dirs(directory, full.names = TRUE, recursive = FALSE)
  all_dates <- sapply(all_dirs, function(dir) {
    time_stamp <- as.POSIXct(sub(".*(\\d{4}-\\d{2}-\\d{2}T\\d{2}-\\d{2}-\\d{2}).*", "\\1", dir), 
                             format = "%Y-%m-%dT%H-%M-%S", tz = "UTC")
    return(time_stamp)
  })
  # Convert dates to current timezone
  all_dates <- as.POSIXct(all_dates, origin = "1960-01-01", tz = "GMT")
  # Print the minimal and maximal date in current timezone
  cat(paste0("Minimal date: ", format(min(all_dates), "%Y-%m-%d %H:%M:%S %Z"), "\n"))
  cat(paste0("Maximal date: ", format(max(all_dates), "%Y-%m-%d %H:%M:%S %Z"), "\n"))
}

# Funktion, um Wetter-Forecast-Daten für die letzten n Stunden zu laden und die Modellnamen sowie nHoursBack hinzuzufügen
load_forecast_data <- function(directory, hours_back = 7) {
  # Alle Verzeichnisse im Hauptverzeichnis auflisten
  all_dirs <- list.dirs(directory, full.names = TRUE, recursive = FALSE)
  
  # Aktuelles Datum und Zeit
  current_time <- Sys.time()
  
  # Filtere Verzeichnisse nach den letzten n Stunden
  selected_dirs <- all_dirs[sapply(all_dirs, function(dir) {
    # Extrahiere Zeitstempel aus dem Verzeichnisnamen
    time_stamp <- as.POSIXct(sub(".*(\\d{4}-\\d{2}-\\d{2}T\\d{2}-\\d{2}-\\d{2}).*", "\\1", dir), 
                             format = "%Y-%m-%dT%H-%M-%S")
    # Prüfe, ob der Zeitstempel innerhalb des gewünschten Zeitfensters liegt
    dt = difftime(current_time, time_stamp, units = "hours") 
    dt <= hours_back
  })]
  
  # Preallocate list for forecast data
  forecast_data_list <- vector("list", length(selected_dirs))
  
  # Lade und kombiniere alle CSV-Dateien aus den ausgewählten Verzeichnissen
  for (i in seq_along(selected_dirs)) {
    #i = 1
    dir <- selected_dirs[i]
    # Extrahiere Zeitstempel aus dem Verzeichnisnamen
    time_stamp <- as.POSIXct(sub(".*(\\d{4}-\\d{2}-\\d{2}T\\d{2}-\\d{2}-\\d{2}).*", "\\1", dir), 
                             format = "%Y-%m-%dT%H-%M-%S")
    
    # Berechne nHoursBack
    n_hours_back <- as.numeric(difftime(current_time, time_stamp, units = "hours"))
    
    # Alle CSV-Dateien im Verzeichnis auflisten
    csv_files <- list.files(dir, pattern = "\\.csv$", full.names = TRUE)
    
    # Preallocate list for CSV data
    csv_data_list <- vector("list", length(csv_files))
    
    # CSV-Dateien einlesen und Modellnamen sowie nHoursBack als Spalte hinzufügen
    for (j in seq_along(csv_files)) {
      #j = 1
      file <- csv_files[j]
      data <- suppressMessages(suppressWarnings(
        read_csv(file, progress = FALSE, num_threads = 4)
        ))  # Use read_csv for faster reading
      data$Model <- basename(file)  # Dateiname als Modellname hinzufügen
      data$nHoursBack <- n_hours_back  # nHoursBack als zusätzliche Spalte
      csv_data_list[[j]] <- data
    }
    
    # Combine CSV data for the current directory
    forecast_data_list[[i]] <- bind_rows(csv_data_list)
  }
  
  # Alle Daten in einem Dataframe kombinieren
  forecast_data <- bind_rows(forecast_data_list)
  
  # Force Time
  forecast_data$date <- as.POSIXct(forecast_data$date, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
  return(forecast_data)
}

# Define the table as a data frame
model_table <- data.frame(
  name = c("gfs_seamless", "gfs_global", "meteofrance_seamless", "meteofrance_arpege_world", 
           "meteofrance_arpege_europe", "meteofrance_arome_france", "meteofrance_arome_france_hd", 
           "jma_seamless", "jma_gsm", "gem_seamless", "gem_global", "icon_seamless", "icon_global", 
           "icon_eu", "icon_d2", "ecmwf_ifs04", "cma_grapes_global", "bom_access_global", 
           "arpae_cosmo_seamless", "arpae_cosmo_2i", "arpae_cosmo_5m", "ecmwf_ifs025", 
           "ecmwf_aifs025", "gfs_graphcast025", "knmi_seamless", "knmi_harmonie_arome_europe", 
           "dmi_seamless", "dmi_harmonie_arome_europe", "metno_seamless", 
           "ukmo_global_deterministic_10km", "ukmo_uk_deterministic_2km", "ukmo_seamless"),
  number = c(2, 3, 5, 7, 8, 10, 11, 12, 15, 16, 17, 20, 21, 22, 23, 24, 53, 54, 56, 57, 59, 
             60, 61, 63, 70, 71, 73, 74, 75, 80, 81, 82)
)

# Function to get the model name by 'number.csv' format
get_model_name <- function(file_name) {
  # Remove the '.csv' part and convert to numeric
  number <- as.numeric(sub(".csv", "", file_name))
  
  # Perform the lookup
  result <- model_table$name[model_table$number == number]
  if (length(result) == 0) {
    return(NA)  # Return NA if the number is not found
  }
  return(result)
}





