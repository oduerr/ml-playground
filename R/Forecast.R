# Compare forecast data with measured data
# rsync -avz --progress --ignore-existing weatherForecast@141.37.176.3:~/cronjob/Cronjob-Service-main/data/ ~/Dropbox/Public/data/kn_forecasts # nolint
source("weather_utils.R")
HOURS_BACK = 10


plot_forecast_variable <- function(data, variable, models, title = "Forecast Plot", days_ahead = 1, additional_data = NULL, additional_variable = NULL) {
  # Filter the data by the specified models
  data_filtered <- data[data$Model %in% models, ]
  # Removes the rows with NA values in the specified variable
  data_filtered <- data_filtered[!is.na(data_filtered[[variable]]), ]
  # Ensure date is in POSIXct format
  data_filtered$date <- as.POSIXct(data_filtered$date, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
  # Set x-axis limits
  max_date <- max(data_filtered$date, na.rm = TRUE)
  min_date <- min(data_filtered$date, na.rm = TRUE)
  # x_limits <- c(min_date, min_date + days_ahead * 24 * 60 * 60)
  x_limits <- c(min_date, max_date)

  # Initialize the plot with alpha set by forecast age (nHoursBack) for the main data only
  plot <- ggplot(data_filtered, aes_string(x = "date", y = variable, color = "nHoursBack", shape = "Model", alpha = "nHoursBack")) +
    geom_point(size = 2) +
    labs(title = title, x = "Datum", y = variable, color = "Forecast Alter (Stunden)", shape = "Modell") +
    theme_minimal() +
    scale_color_gradient(low = "green", high = "blue") +
    scale_alpha_continuous(range = c(0.1, 1), trans = "reverse") +  # Older forecasts are more transparent
    #scale_shape_manual(values = c(16, 17, 18, 19, 15)) +
    scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "twodash"))
  
  # Add additional data without alpha if provided
  if (!is.null(additional_data) && !is.null(additional_variable)) {
    additional_data$date <- as.POSIXct(additional_data$date, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
    plot <- plot +
      geom_line(data = additional_data, aes_string(x = "date", y = additional_variable), color = "red", size = 2, alpha=1, linetype = "solid")
  }
  
  # Set x-axis limits
  plot <- plot + scale_x_datetime(limits = x_limits)
  return(plot)
}

calculate_daily_means <- function(data, variable) {
  data %>%
    mutate(date = as.Date(date)) %>%
    group_by(date) %>%
    summarise(daily_mean = mean(get(variable), na.rm = TRUE))
}

######### Loading Forecast Data #########
print_info("~/Dropbox/Public/data/kn_forecasts/")
forecast_data <- load_forecast_data(directory = "~/Dropbox/Public/data/kn_forecasts/", hours_back = HOURS_BACK) 
#plot_forecast_variable(forecast_data, variable = "cloud_cover", models = c('8.csv'))
plot_forecast_variable(forecast_data, variable = "temperature_2m", models = c('8.csv'))

######### Loading Meassured Data #########
id = findID('Konstanz')
print(id)
if (FALSE){
  KN_pos = selectDWD(id=id, res='hourly',  per='recent') #recent ist bis gestern muß manuell auswählen
  saveRDS(KN_pos, file = "KN_pos.rds")
} else {
  KN_pos = readRDS("KN_pos.rds")
  KN_pos
}
KN_recent = dataDWD(KN_pos, read = TRUE, varnames=TRUE)
d = KN_recent$hourly_cloudiness_recent_stundenwerte_N_02712_akt$MESS_DATUM
tail(d)

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

#rename MESS_DATUM to date
names(KN_combined)[names(KN_combined) == "MESS_DATUM"] <- "date"
KN_combined[1:5,1:7]
KN_combined$Model = 'observed' 
forecast_data[1:5,1:7]

#forecast_data %>% filter(Model == '11.csv') %>% tail() %>% View()

###### Optional: only use the lastest forecast data ####
# Filter the data to keep only rows with the smallest nHoursBack for each date
forecast_data_min_nHours <- forecast_data %>%
  group_by(date, Model) %>%
  filter(nHoursBack == min(nHoursBack, na.rm = TRUE)) %>%
  #filter(nHoursBack == max(nHoursBack, na.rm = TRUE)) %>%
  ungroup()

KN_combined$cloud_cover_low = KN_combined$V_N.Bedeckungsgrad.x/8*100 #KN_combined$V_S1_NS.Bedeckungsgrad_Schicht1/8*100
# Calculate the RMSE between the forecast and the measured data for KN_combined and forecast_data_min_nHours for each model
library(Metrics)  # for rmse function
# Merge forecast and measured data on date
data_merged <- forecast_data_min_nHours %>%
  inner_join(KN_combined, by = "date", suffix = c("_forecast", "_measured"))

# Calculate RMSE for each model
rmse_results <- data_merged %>%
  group_by(Model_forecast) %>%
  summarize(
    rmse_temperature_2m = rmse(temperature_2m, TT.Lufttemperatur),
    rmse_cloud_cover = rmse(cloud_cover_low_measured, cloud_cover_low_forecast),
    # Add additional variables here as needed
    .groups = "drop"
  )

# View the RMSE results
rmse_results <- rmse_results %>%
  mutate(Model_Name = sapply(Model_forecast, get_model_name)) %>% 
  column_to_rownames(var = "Model_Name") 

# Drop the Model column if no longer needed
rmse_results <- rmse_results %>%
  arrange(rmse_temperature_2m)  # Sort by cloud cover RMSE

# View the sorted RMSE results
print(rmse_results)


get_model_name(rmse_results$Model_forecast)
print(rmse_results)

models = c('23.csv','81.csv', '11.csv')
plot_forecast_variable(data = forecast_data_min_nHours, variable = "temperature_2m", 
                       models=models, additional_data = KN_combined, additional_variable = "TT.Lufttemperatur") +
  geom_vline(xintercept = as.numeric(Sys.time()), color = "red", linetype = "dashed")


#models = c('23.csv')
plot_forecast_variable(data = forecast_data_min_nHours, variable = "cloud_cover_low", 
                       models=models, additional_data = KN_combined, additional_variable = "cloud_cover_low") +
  geom_vline(xintercept = as.numeric(Sys.time()), color = "red", linetype = "dashed")

plots = list()
models = forecast_data$Model %>% unique()
#models = c('11.csv','23.csv', '81.csv', '74.csv')
######### Cloud Cover #########
for (i in 1:length(models)){
  #i = 1
  print(i)
  model = models[i]
  # Try to create the plot and save it
  tryCatch({
    # Generate plot
    p <- plot_forecast_variable(
      #data = forecast_data, 
      data = forecast_data_min_nHours,
      variable = "cloud_cover_low", 
      models = c(model), 
      additional_data = KN_combined, 
      additional_variable = "cloud_cover_low"
    ) +
      geom_vline(xintercept = as.numeric(Sys.time()), color = "red", linetype = "dashed") + 
      labs(
        title = paste0("Forecast Plot ", Sys.time(), " model ", get_model_name(model)),
        x = "Date", 
        y = "Cloud Cover (%)"
      )
    
    # Store plot in the list
    plots[[i]] <- p
    
    # Save plot as PNG
    ggsave(filename = paste0("forecast_plot_", get_model_name(model), ".png"), plot = p)
    
  }, error = function(e) {
    # Handle errors: print a message and skip to the next iteration
    message(paste("Error in processing model:", model))
    message("Error message:", e$message)
    # Optionally, you could log errors or store them in a list for later inspection
  })
}
plots


plot_forecast_variable(data = forecast_data, variable = "cloud_cover_low", 
                       models = c('81.csv'), additional_data = KN_combined, additional_variable = "cloud_cover_low") +
  geom_vline(xintercept = as.numeric(Sys.time()), color = "red", linetype = "dashed") + 
  labs(title = paste0("Forecast Plot ", Sys.time()), x = "Date", y = "Cloud Cover (%)")


# Plot daily means
ggplot() +
  geom_line(data = forecast_daily_means, aes(x = date, y = daily_mean, color = "Forecast")) +
  geom_line(data = measured_daily_means, aes(x = date, y = daily_mean, color = "Measured")) +
  labs(title = "Daily Mean Temperature", x = "Date", y = "Temperature (°C)", color = "Data Source") +
  theme_minimal()

### Tagesmittelwerte
#https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/10_minutes/solar/now/
#https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/10_minutes/precipitation/now/



