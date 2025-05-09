library(rdwd)
library(tidyverse)
# Für die Aktuelle Daten
library(httr)
library(utils)


# Define the URL and file paths
# Define the function to download and read DWD weather data
download_and_read_dwd_data <- function(url_now) {
  output_dir <- tempdir(check = TRUE)
  temp_file <- tempfile(fileext = ".zip")
  # remove all files in the output directory
  download.file(url_now, temp_file, mode = "wb")
  unzip(temp_file, exdir = output_dir)
  extracted_files <- list.files(output_dir, full.names = TRUE)
  data_file <- extracted_files[grep(".txt$", extracted_files)]
  weather_data <- read.csv(data_file, sep = ";", header = TRUE)
  return(weather_data)
}


#### Reading "historic" data 
id = findID('Konstanz')
print(id)

### Determine the available data on the server, this is an interactive process
if (FALSE){
  KN_pos = selectDWD(id=id, res='hourly',  per='recent')
  KN_pos
  save(KN_pos, file='KN_pos.RData')
} else {
  load('KN_pos.RData')
}
# Downloading or Reading the cached data
KN_recent = dataDWD(KN_pos, read = TRUE, varnames=TRUE)
# str(KN_recent)
library(dplyr)
# Liste aller DataFrames in KN_recent (außer "STATIONS_ID" und "eor")
df_list <- lapply(KN_recent, function(df) {
  df %>% select(-STATIONS_ID, -eor)
})
# Initialisiere den kombinierten DataFrame mit dem ersten DataFrame in der Liste
KN_combined <- df_list[[1]]
# Mergen aller DataFrames in der Liste basierend auf MESS_DATUM
for (i in 2:length(df_list)) {
  KN_combined <- KN_combined %>%
    full_join(df_list[[i]], by = "MESS_DATUM")
}

# Schritt 1: Neue Spalte "Nebel" hinzufügen, die Nebelmeldungen (WW.Beobachtung 100–129) kennzeichnet
# Neue Spalte "Nebel" hinzufügen, basierend auf den relevanten Texten in WW_Text
KN_combined <- KN_combined %>%
  mutate(Nebel = ifelse(grepl("Nebel|Feuchter Dunst|NEBEL", WW_Text), 1, 0))

# Schritt 2: Extrahiere das Datum (ohne Uhrzeit) aus MESS_DATUM für die Gruppierung
KN_combined <- KN_combined %>%
  mutate(Datum = as.Date(MESS_DATUM))

# Extrahiere die Uhrzeit aus MESS_DATUM und filtere nur Ereignisse zwischen 06:00 und 18:00 Uhr
KN_combined_daytime <- KN_combined %>%
  mutate(Uhrzeit = hour(MESS_DATUM)) %>%  # Extrahiere die Stunde
  filter(Uhrzeit >= 9 & Uhrzeit <= 16)    # Filtere nur Meldungen tagsüber

# Schritt 3: Berechne den Anteil der Nebelmeldungen pro Tag
# Wir gruppieren nach Datum und berechnen den Anteil der Nebelmeldungen
nebel_pro_tag <- KN_combined_daytime %>%
  filter(WW.Beobachtung > -1) %>% # Filtere alle Zeilen mit WW.Beobachtung -1 (keine Wettermeldung)
  group_by(Datum) %>%
  summarise(
    total_meldungen = n(),           # Gesamtanzahl der Wettermeldungen
    nebel_meldungen = sum(Nebel),    # Anzahl der Nebelmeldungen
    anteil_nebel = nebel_meldungen / total_meldungen, # Anteil der Nebelmeldungen in 
    min_temp = min(TT_TU.Lufttemperatur, na.rm = TRUE),      # Minimale Temperatur (wenn vorhanden)
    max_temp = max(TT_TU.Lufttemperatur, na.rm = TRUE),      # Maximale Temperatur
    min_feuchte = min(RF_TU.Relative_Feuchte, na.rm = TRUE), # Minimale relative Feuchte
    max_feuchte = max(RF_TU.Relative_Feuchte, na.rm = TRUE), # Maximale relative Feuchte
    min_sichtweite = min(V_VV.Sichtweite, na.rm = TRUE),     # Minimale Sichtweite
    max_sichtweite = max(V_VV.Sichtweite, na.rm = TRUE)      # Maximale Sichtweite
  )


# Ausgabe der ersten Zeilen der Berechnung
tail(nebel_pro_tag)

ggplot(nebel_pro_tag, aes(x=Datum, y=anteil_nebel)) + geom_point() 







### Reading the recent / currect data
#https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/10_minutes/air_temperature/now/https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/10_minutes/air_temperature/now/10minutenwerte_TU_02712_now.zip
  # Load required libraries
url_now <- "https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/10_minutes/air_temperature/now/10minutenwerte_TU_02712_now.zip"
KN_now <- download_and_read_dwd_data(url_)
tail(KN_now)

url_now <- "https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/1_minute/precipitation/now/1minutenwerte_nieder_02712_now.zip"
KN_now <- download_and_read_dwd_data(url_now)
tail(KN_now)

url_now <- "https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/10_minutes/solar/now/10minutenwerte_SOLAR_02712_now.zip"
KN_now <- download_and_read_dwd_data(url_now,10)
tail(KN_now)







