# 02_prepare_asthma_data.R

# Air Pollution and Asthma Burden in the United States

# Purpose:
# Clean the county-level 2023 asthma emergency department dataset and construct
# the binary asthma burden outcome used in the analysis.

# Input:
#   data/raw/Annual_Asthama_ED_Visit_Data_2023.csv

# Output:
#   data/processed/Annual_Asthama_ED_Visit_Data_2023_Cleaned.csv

# Variable created:
#   - Asthma_ED_Status

# Author:
#   Parminder S. Kooner

# 1. LOAD REQUIRED PACKAGES

library(readr)
library(dplyr)

# 2. DEFINE FILE PATHS

# Relative paths are used here to make sure that the script can be run from the root of the
# GitHub repository without requiring computer-specific working directories.

input_file <- "data/raw/Annual_Asthama_ED_Visit_Data_2023.csv"

output_file <- "data/processed/Annual_Asthama_ED_Visit_Data_2023_Cleaned.csv"

# 3. IMPORT RAW ASTHMA ED DATA

asthma <- read_csv(
  input_file,
  show_col_types = FALSE
)


# Inspect structure
glimpse(asthma)

# 4. REMOVE UNUSED OR EMPTY VARIABLES

# Remove columns that contain only missing values.

# StateFIPS and CountyFIPS are removed because geographic matching in this
# project is performed using State, County, and Year.

# "Data Comment" is removed if present.

asthma_clean <- asthma %>%

  # Remove columns containing only missing values
  select(
    where(~ !all(is.na(.)))
  ) %>%

  # Remove unused identifiers/comments
  select(
    -any_of(
      c(
        "StateFIPS",
        "CountyFIPS",
        "Data Comment"
      )
    )
  )

# 5. CONSTRUCT ELEVATED ASTHMA ED STATUS

# Asthma_ED_Status is the primary binary outcome for the analysis.

# Sex-specific national benchmarks:

#   Male:
#     Elevated if Asthma_ED_Rate > 27.1 per 10,000 population

#   Female:
#     Elevated if Asthma_ED_Rate > 32.3 per 10,000 population

# Coding:

#   Not Elevated = rate at or below the sex-specific benchmark
#   Elevated     = rate above the sex-specific benchmark

asthma_clean <- asthma_clean %>%
  mutate(

    Asthma_ED_Status = if_else(

      (Sex == "Male" & Asthma_ED_Rate > 27.1) |
        (Sex == "Female" & Asthma_ED_Rate > 32.3),

      "Elevated",
      "Not Elevated"
    ),

    Asthma_ED_Status = factor(
      Asthma_ED_Status,
      levels = c(
        "Not Elevated",
        "Elevated"
      )
    )
  )

# 6. DATA QUALITY CHECKS

# Review missingness
colSums(
  is.na(asthma_clean)
)


# Review sex distribution
table(
  asthma_clean$Sex,
  useNA = "ifany"
)


# Review asthma ED rate
summary(
  asthma_clean$Asthma_ED_Rate
)


# Review derived binary outcome
table(
  asthma_clean$Asthma_ED_Status,
  useNA = "ifany"
)


# Review outcome percentages
prop.table(
  table(asthma_clean$Asthma_ED_Status)
)

# 7. VERIFY OUTCOME CONSTRUCTION

# Optional validation check:
# Confirm that observations classified as Elevated satisfy the appropriate
# sex-specific threshold.

validation <- asthma_clean %>%
  mutate(
    expected_status = case_when(

      Sex == "Male" &
        Asthma_ED_Rate > 27.1 ~ "Elevated",

      Sex == "Male" &
        Asthma_ED_Rate <= 27.1 ~ "Not Elevated",

      Sex == "Female" &
        Asthma_ED_Rate > 32.3 ~ "Elevated",

      Sex == "Female" &
        Asthma_ED_Rate <= 32.3 ~ "Not Elevated",

      TRUE ~ NA_character_
    )
  )


# Count any mismatches
sum(
  as.character(validation$Asthma_ED_Status) !=
    validation$expected_status,
  na.rm = TRUE
)

# 8. EXPORT CLEANED ASTHMA DATA

write_csv(
  asthma_clean,
  output_file
)

# 9. CONFIRM OUTPUT

message(
  "Asthma ED data preparation complete."
)

message(
  paste(
    "Cleaned dataset saved to:",
    output_file
  )
)

