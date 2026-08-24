# 01_prepare_aqi_data.R

# Air Pollution and Asthma Burden in the United States

# Purpose:
# Clean the county-level 2023 EPA Air Quality Index (AQI) dataset and construct
# the air-pollution exposure variables used in the analysis.

# Input:
#   data/raw/Annual_AQI_By_County_2023.csv

# Output:
#   data/processed/Annual_AQI_By_County_2023_Cleaned.csv

# Variables created:
#   - Annual_Weighted_AQI
#   - Pollutant_Category
#   - prop_unhealthy
#   - AQI_Category

# Author:
#   Parminder S. Kooner


# 1. LOAD REQUIRED PACKAGES

library(readr)
library(dplyr)

# 2. DEFINE FILE PATHS

# Relative paths are used so the script can be run from the root of the
# GitHub repository without requiring computer-specific working directories.

input_file <- "data/raw/Annual_AQI_By_County_2023.csv"

output_file <- "data/processed/Annual_AQI_By_County_2023_Cleaned.csv"

# 3. IMPORT RAW AQI DATA

aqi <- read_csv(
  input_file,
  show_col_types = FALSE
)


# Inspect structure
glimpse(aqi)

# 4. CONSTRUCT ANNUAL WEIGHTED AQI

# Annual_Weighted_AQI summarizes annual air-pollution exposure by weighting
# the number of days in each EPA AQI category by a representative midpoint.
#
# Midpoints used:
#
# Good                               = 25
# Moderate                           = 75
# Unhealthy for Sensitive Groups    = 125
# Unhealthy                          = 175
# Very Unhealthy                     = 250
# Hazardous                          = 400

aqi <- aqi %>%
  mutate(
    Annual_Weighted_AQI = (
      (`Good Days` * 25) +
      (`Moderate Days` * 75) +
      (`Unhealthy for Sensitive Groups Days` * 125) +
      (`Unhealthy Days` * 175) +
      (`Very Unhealthy Days` * 250) +
      (`Hazardous Days` * 400)
    ) / `Days with AQI`
  )

# 5. CONSTRUCT DOMINANT POLLUTANT CATEGORY

# Pollutant_Category identifies the pollutant responsible for the greatest
# number of AQI days within each county.
# If multiple pollutants are tied for the maximum number of days,
# the observation is classified as "Mixed".

aqi <- aqi %>%
  rowwise() %>%
  mutate(
    max_pollutant_days = max(
      c(
        `Days CO`,
        `Days NO2`,
        `Days Ozone`,
        `Days PM2.5`,
        `Days PM10`
      ),
      na.rm = TRUE
    ),

    Pollutant_Category = case_when(

      sum(
        c(
          `Days CO`,
          `Days NO2`,
          `Days Ozone`,
          `Days PM2.5`,
          `Days PM10`
        ) == max_pollutant_days,
        na.rm = TRUE
      ) > 1 ~ "Mixed",

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

# 6. CONSTRUCT PROPORTION OF UNHEALTHY AQI DAYS

# prop_unhealthy represents the proportion of monitored AQI days classified
# as Unhealthy for Sensitive Groups or worse.

aqi <- aqi %>%
  mutate(
    prop_unhealthy = (
      `Unhealthy for Sensitive Groups Days` +
      `Unhealthy Days` +
      `Very Unhealthy Days` +
      `Hazardous Days`
    ) / `Days with AQI`
  )

# 7. CONSTRUCT AQI EXPOSURE CATEGORY

# AQI_Category divides counties into approximately three equally sized groups
# based on the empirical distribution of prop_unhealthy.

# Categories:
#   Low
#   Moderate
#   High

# Cut points correspond approximately to the 33rd and 66th percentiles.

aqi_cuts <- quantile(
  aqi$prop_unhealthy,
  probs = c(0, 0.33, 0.66, 1),
  na.rm = TRUE
)

# Remove duplicate breakpoints if they occur
aqi_cuts <- unique(aqi_cuts)

aqi <- aqi %>%
  mutate(
    AQI_Category = cut(
      prop_unhealthy,
      breaks = aqi_cuts,
      include.lowest = TRUE,
      labels = c(
        "Low",
        "Moderate",
        "High"
      )[1:(length(aqi_cuts) - 1)]
    )
  )

# Convert AQI category to an ordered factor
aqi$AQI_Category <- factor(
  aqi$AQI_Category,
  levels = c(
    "Low",
    "Moderate",
    "High"
  ),
  ordered = TRUE
)

# Convert dominant pollutant to factor
aqi$Pollutant_Category <- factor(
  aqi$Pollutant_Category
)

# 8. DATA QUALITY CHECKS

# Check missingness
colSums(is.na(aqi))

# Check newly constructed variables
summary(aqi$Annual_Weighted_AQI)

summary(aqi$prop_unhealthy)

table(
  aqi$AQI_Category,
  useNA = "ifany"
)

table(
  aqi$Pollutant_Category,
  useNA = "ifany"
)

# 9. EXPORT CLEANED AQI DATA

write_csv(
  aqi,
  output_file
)

# 10. CONFIRM OUTPUT

message(
  "AQI data preparation complete."
)

message(
  paste(
    "Cleaned dataset saved to:",
    output_file
  )
)
