# 03_merge_datasets.R
# Load libraries
library(dplyr)
library(readxl)
library(writexl)
library(stringr)

# File paths
aqi_path <- "data/processed/Annual_AQI_By_County_2023_Cleaned.xlsx"
asthma_path <- "data/processed/Annual_Asthama_ED_Visit_Data_2023_Cleaned.xlsx"

# Read datasets
aqi <- read_excel(aqi_path)
asthma <- read_excel(asthma_path)

# Standardize names for matching
aqi <- aqi %>%
  mutate(
    State = trimws(toupper(State)),
    County = trimws(toupper(County))
  )

asthma <- asthma %>%
  mutate(
    State = trimws(toupper(State)),
    County = trimws(toupper(County))
  )

# Merge datasets (State + County + Year)
merged_data <- inner_join(aqi, asthma, by = c("State", "County", "Year"))

# Convert names back to proper case
merged_data <- merged_data %>%
  mutate(
    State = str_to_title(tolower(State)),
    County = str_to_title(tolower(County))
  )

# Output path
output_path <- "data/processed/Combined_AQI_Asthma_2023_Cleaned.xlsx"

# Export merged dataset
write_xlsx(merged_data, output_path)

# Confirmation
print("Datasets merged successfully with updated file paths.")
