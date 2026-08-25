# 03_merge_datasets.R

# Air Pollution and Asthma Burden in the United States

# Purpose:
# Merge the cleaned county-level EPA AQI dataset with the cleaned asthma
# emergency department dataset to create the final analytical dataset.

# Inputs:
#   data/processed/Annual_AQI_By_County_2023_Cleaned.csv
#   data/processed/Annual_Asthama_ED_Visit_Data_2023_Cleaned.csv

# Output:
#   data/processed/Combined_AQI_Asthma_2023_Cleaned.csv

# Merge keys:
#   - State
#   - County
#   - Year

# Author:
#   Parminder S. Kooner

# 1. LOAD REQUIRED PACKAGES

library(readr)
library(dplyr)
library(stringr)

# 2. DEFINE FILE PATHS

aqi_file <-
  "data/processed/Annual_AQI_By_County_2023_Cleaned.csv"

asthma_file <-
  "data/processed/Annual_Asthama_ED_Visit_Data_2023_Cleaned.csv"

output_file <-
  "data/processed/Combined_AQI_Asthma_2023_Cleaned.csv"

# 3. IMPORT CLEANED DATASETS

aqi <- read_csv(
  aqi_file,
  show_col_types = FALSE
)

asthma <- read_csv(
  asthma_file,
  show_col_types = FALSE
)


# Inspect structures
glimpse(aqi)
glimpse(asthma)

# 4. STANDARDIZE GEOGRAPHIC IDENTIFIERS

# Geographic names are standardized before merging to reduce mismatches caused
# by capitalization or leading/trailing whitespace.

aqi <- aqi %>%
  mutate(
    State = str_trim(str_to_upper(State)),
    County = str_trim(str_to_upper(County))
  )

asthma <- asthma %>%
  mutate(
    State = str_trim(str_to_upper(State)),
    County = str_trim(str_to_upper(County))
  )

# 5. CHECK MERGE KEYS
# Confirm that State, County, and Year are present in both datasets.

required_keys <- c(
  "State",
  "County",
  "Year"
)

stopifnot(
  all(required_keys %in% names(aqi))
)

stopifnot(
  all(required_keys %in% names(asthma))
)

# 6. MERGE AQI AND ASTHMA DATA

# An inner join retains only observations with matching State, County, and Year
# values in both source datasets.

combined_data <- inner_join(
  aqi,
  asthma,
  by = c(
    "State",
    "County",
    "Year"
  )
)

# 7. RESTORE READABLE GEOGRAPHIC LABELS

combined_data <- combined_data %>%
  mutate(
    State = str_to_title(str_to_lower(State)),
    County = str_to_title(str_to_lower(County))
  )

# 8. VERIFY MERGED DATASET

# Number of observations
nrow(combined_data)


# Number of states
n_distinct(combined_data$State)


# Number of unique counties
combined_data %>%
  distinct(State, County) %>%
  nrow()


# Number of sex categories
n_distinct(combined_data$Sex)


# Years included
sort(
  unique(combined_data$Year)
)


# Review structure
glimpse(combined_data)

# 9. CHECK FOR MISSINGNESS
colSums(
  is.na(combined_data)
)

# 10. CHECK KEY ANALYTICAL VARIABLES

# AQI exposure category
table(
  combined_data$AQI_Category,
  useNA = "ifany"
)


# Dominant pollutant
table(
  combined_data$Pollutant_Category,
  useNA = "ifany"
)


# Sex
table(
  combined_data$Sex,
  useNA = "ifany"
)


# Asthma ED status
table(
  combined_data$Asthma_ED_Status,
  useNA = "ifany"
)

# 11. CHECK FOR DUPLICATE COUNTY-BY-SEX OBSERVATIONS

# The analytical dataset is expected to have county-by-sex observations.
# This check identifies duplicate combinations of State, County, Year, and Sex.

duplicate_check <- combined_data %>%
  count(
    State,
    County,
    Year,
    Sex
  ) %>%
  filter(n > 1)

duplicate_check

# 12. EXPORT FINAL ANALYTICAL DATASET

write_csv(
  combined_data,
  output_file
)

# 13. CONFIRM OUTPUT

message(
  "AQI and asthma datasets merged successfully."
)

message(
  paste(
    "Final analytical dataset saved to:",
    output_file
  )
)
