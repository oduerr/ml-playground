library(httr)
library(jsonlite)

# Define the API base URL
API_BASE_URL <- "https://dwd.api.proxy.bund.dev/v30"

# Function to fetch data from the API
fetch_weather_data <- function(station_id) {
  response <- GET(
    url = paste0(API_BASE_URL, "/stationOverviewExtended"),
    query = list(stationIds = station_id),
    add_headers(Accept = "application/json")
  )
  
  print(response)  # Print the response object)
  # Check if the request was successful
  if (status_code(response) == 200) {
    data <- content(response, "text", encoding = "UTF-8")
    json_data <- fromJSON(data)
    
    print("API Response:")
    print(json_data)  # Print full JSON response
    
    return(json_data)
  } else {
    print(paste("Error: HTTP", status_code(response)))
    return(NULL)
  }
}

# Replace with an actual station ID
station_id <- "10929" #2712"

# Call the function and inspect the output
weather_data <- fetch_weather_data(station_id)
weather_data
d = weather_data[[station_id]]

forecast <- weather_data[[station_id]]$forecast2
forecast$temperature / 10

library(ggplot2)
library(lubridate)

library(ggplot2)
library(lubridate)
library(dplyr)

library(ggplot2)
library(lubridate)
library(dplyr)

# Extract data function with length alignment
extract_temperature_data <- function(weather_data) {
  station_id <- names(weather_data)[1]  # Assuming only one station is returned
  forecast <- weather_data[[station_id]]$forecast1
  
  # Convert timestamps (milliseconds to seconds)
  start_time <- as.POSIXct(forecast$start / 1000, origin = "1970-01-01", tz = "UTC")
  time_step <- forecast$timeStep / 1000  # Convert from milliseconds to seconds
  
  # Create time sequence
  timestamps <- seq(from = start_time, by = time_step, length.out = length(forecast$temperature))
  
  # Ensure all vectors have the same length as `temperature`
  len <- length(forecast$temperature)
  
  temperatures <- forecast$temperature / 10  # Convert to Celsius
  sunshine <- ifelse(length(forecast$sunshine) == len, forecast$sunshine, rep(NA, len))  # Extend with NA
  is_day <- ifelse(length(forecast$isDay) == len, forecast$isDay, rep(NA, len))  # Extend with NA
  
  # Create data frame
  df <- data.frame(Time = timestamps, 
                   Temperature = temperatures, 
                   Sunshine = sunshine, 
                   isDay = is_day)
  
  return(df)
}

# Extract temperature data
temp_data <- extract_temperature_data(weather_data)

# Get the current time
current_time <- Sys.time()

# Plot with enhancements
ggplot(temp_data, aes(x = Time)) +
  # Temperature line
  geom_line(aes(y = Temperature), color = "blue", size = 1) +
  
  # Sunshine bar chart (scaled to fit)
  geom_bar(aes(y = Sunshine / max(Sunshine, na.rm = TRUE) * max(Temperature, na.rm = TRUE) * 0.5), 
           stat = "identity", fill = "yellow", alpha = 0.5, na.rm = TRUE) +
  
  # Night shading
  geom_rect(data = temp_data %>% filter(isDay == FALSE),
            aes(xmin = Time, xmax = lead(Time, default = last(Time)), 
                ymin = -Inf, ymax = Inf),
            fill = "gray", alpha = 0.3) +
  
  # Vertical line for current time
  geom_vline(xintercept = as.numeric(current_time), linetype = "dashed", color = "red", size = 1) +
  
  # Labels & Styling
  labs(title = "Temperature & Sunshine Forecast", x = "Time", 
       y = "Temperature (°C)", fill = "Sunshine (scaled)") +
  scale_x_datetime(date_labels = "%Y-%m-%d %H:%M", date_breaks = "12 hours") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
