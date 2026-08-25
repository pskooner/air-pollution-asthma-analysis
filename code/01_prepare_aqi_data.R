# 01_prepare_aqi_data.R
# Load libraries
library(readr)
library(dplyr)
library(writexl)

# File path
file_path <- "data/raw/Annual_AQI_By_County_2023.csv"

# Read data
data <- read_csv(file_path, show_col_types = FALSE)

# 1. Annual Weighted AQI
data <- data %>%
  mutate(
    Annual_Weighted_AQI = (
      `Good Days` * 25 +
        `Moderate Days` * 75 +
        `Unhealthy for Sensitive Groups Days` * 125 +
        `Unhealthy Days` * 175 +
        `Very Unhealthy Days` * 250 +
        `Hazardous Days` * 400
    ) / `Days with AQI`
  )

# 2. Pollutant Category
data <- data %>%
  rowwise() %>%
  mutate(
    max_pollutant_days = max(c(`Days CO`,
                               `Days NO2`,
                               `Days Ozone`,
                               `Days PM2.5`,
                               `Days PM10`), na.rm = TRUE),
    
    Pollutant_Category = case_when(
      sum(c(`Days CO`,
            `Days NO2`,
            `Days Ozone`,
            `Days PM2.5`,
            `Days PM10`) == max_pollutant_days, na.rm = TRUE) > 1 ~ "Mixed",
      
      max_pollutant_days == `Days CO` ~ "CO",
      max_pollutant_days == `Days NO2` ~ "NO2",
      max_pollutant_days == `Days Ozone` ~ "Ozone",
      max_pollutant_days == `Days PM2.5` ~ "PM2.5",
      max_pollutant_days == `Days PM10` ~ "PM10",
      
      TRUE ~ NA_character_
    )
  ) %>%
  ungroup() %>%
  select(-max_pollutant_days)

# 3. Proportion of unhealthy days
data <- data %>%
  mutate(
    prop_unhealthy = (
      `Unhealthy for Sensitive Groups Days` +
        `Unhealthy Days` +
        `Very Unhealthy Days` +
        `Hazardous Days`
    ) / `Days with AQI`
  )

# 4. AQI Category (Quantile-based)
cuts <- quantile(data$prop_unhealthy,
                 probs = c(0, 0.33, 0.66, 1),
                 na.rm = TRUE)

cuts <- unique(cuts)  # prevent duplicate breakpoints

data <- data %>%
  mutate(
    AQI_Category = cut(
      prop_unhealthy,
      breaks = cuts,
      include.lowest = TRUE,
      labels = c("Low", "Moderate", "High")[1:(length(cuts)-1)]
    )
  )

# Convert to ordered factor
data$AQI_Category <- factor(
  data$AQI_Category,
  levels = c("Low", "Moderate", "High"),
  ordered = TRUE
)

# Convert pollutant to factor
data$Pollutant_Category <- as.factor(data$Pollutant_Category)

# Output path
output_path <- "data/processed/Annual_AQI_By_County_2023_Cleaned.xlsx"

# Export
write_xlsx(data, output_path)

# Confirmation
print("New Excel file created successfully!")
