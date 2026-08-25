# 06_stratified_analysis.R

# Air Pollution and Asthma Burden in the United States

# Purpose:
# Evaluate the association between AQI category and elevated asthma emergency
# department (ED) burden after stratification by sex and state.

# Statistical methods:
#   - Stratified contingency tables
#   - Row percentages
#   - Cochran-Mantel-Haenszel (CMH) tests

# Input:
#   data/processed/Combined_AQI_Asthma_2023_Cleaned.csv

# Outputs:
#   results/tables/aqi_asthma_by_sex.csv
#   results/tables/cmh_adjusted_for_sex.csv
#   results/tables/aqi_asthma_by_state.csv
#   results/tables/cmh_adjusted_for_state.csv

# Author:
#   Parminder S. Kooner

# 1. LOAD REQUIRED PACKAGES

library(readr)
library(dplyr)
library(tidyr)
library(broom)

# 2. DEFINE FILE PATHS

input_file <-
  "data/processed/Combined_AQI_Asthma_2023_Cleaned.csv"

sex_table_file <-
  "results/tables/aqi_asthma_by_sex.csv"

cmh_sex_file <-
  "results/tables/cmh_adjusted_for_sex.csv"

state_table_file <-
  "results/tables/aqi_asthma_by_state.csv"

cmh_state_file <-
  "results/tables/cmh_adjusted_for_state.csv"


# 3. IMPORT ANALYTICAL DATASET

data <- read_csv(
  input_file,
  show_col_types = FALSE
)

# 4. FORMAT ANALYTICAL VARIABLES

data <- data %>%
  mutate(

    AQI_Category = factor(
      AQI_Category,
      levels = c(
        "Low",
        "Moderate",
        "High"
      ),
      ordered = TRUE
    ),

    Asthma_ED_Status = factor(
      Asthma_ED_Status,
      levels = c(
        "Not Elevated",
        "Elevated"
      )
    ),

    Sex = factor(
      Sex
    ),

    State = factor(
      State
    )
  )

# PART A — STRATIFIED ANALYSIS BY SEX

# 5. CREATE AQI × ASTHMA × SEX CONTINGENCY TABLE

aqi_asthma_sex_table <- xtabs(
  ~ AQI_Category + Asthma_ED_Status + Sex,
  data = data
)

aqi_asthma_sex_table


# 6. CREATE TIDY SEX-STRATIFIED TABLE

aqi_asthma_by_sex <- as.data.frame(
  aqi_asthma_sex_table
) %>%

  rename(
    Count = Freq
  ) %>%

  group_by(
    Sex,
    AQI_Category
  ) %>%

  mutate(
    Row_Percent = 100 * Count / sum(Count)
  ) %>%

  ungroup() %>%

  mutate(
    Row_Percent = round(
      Row_Percent,
      1
    )
  )


aqi_asthma_by_sex


# Export Table 10 data.

write_csv(
  aqi_asthma_by_sex,
  sex_table_file
)


# 7. DISPLAY SEX-SPECIFIC ROW PROPORTIONS

# Male observations

male_table <- table(
  data$AQI_Category[
    data$Sex == "Male"
  ],
  data$Asthma_ED_Status[
    data$Sex == "Male"
  ]
)

male_row_proportions <- prop.table(
  male_table,
  margin = 1
)

male_row_proportions


# Female observations

female_table <- table(
  data$AQI_Category[
    data$Sex == "Female"
  ],
  data$Asthma_ED_Status[
    data$Sex == "Female"
  ]
)

female_row_proportions <- prop.table(
  female_table,
  margin = 1
)

female_row_proportions


# 8. COCHRAN-MANTEL-HAENSZEL TEST ADJUSTED FOR SEX

# The CMH test evaluates whether the association between AQI category and
# asthma ED status persists after stratifying by sex.

cmh_sex <- mantelhaen.test(
  aqi_asthma_sex_table
)

cmh_sex


# Extract results.

cmh_sex_result <- tibble(

  Adjustment =
    "Sex",

  Statistic =
    as.numeric(
      cmh_sex$statistic
    ),

  Degrees_of_Freedom =
    as.numeric(
      cmh_sex$parameter
    ),

  P_Value =
    cmh_sex$p.value
) %>%

  mutate(

    Statistic = round(
      Statistic,
      3
    ),

    Degrees_of_Freedom = round(
      Degrees_of_Freedom,
      0
    ),

    P_Value = round(
      P_Value,
      6
    )
  )


cmh_sex_result


# Export Table 11 result.

write_csv(
  cmh_sex_result,
  cmh_sex_file
)

# PART B — STRATIFIED ANALYSIS BY STATE

# 9. CREATE AQI × ASTHMA × STATE CONTINGENCY TABLE

aqi_asthma_state_table <- xtabs(
  ~ AQI_Category + Asthma_ED_Status + State,
  data = data
)

aqi_asthma_state_table


# 10. CREATE TIDY STATE-STRATIFIED TABLE

aqi_asthma_by_state <- as.data.frame(
  aqi_asthma_state_table
) %>%

  rename(
    Count = Freq
  ) %>%

  group_by(
    State,
    AQI_Category
  ) %>%

  mutate(
    Row_Percent = ifelse(
      sum(Count) > 0,
      100 * Count / sum(Count),
      NA_real_
    )
  ) %>%

  ungroup() %>%

  mutate(
    Row_Percent = round(
      Row_Percent,
      1
    )
  )


aqi_asthma_by_state


write_csv(
  aqi_asthma_by_state,
  state_table_file
)


# 11. COCHRAN-MANTEL-HAENSZEL TEST ADJUSTED FOR STATE

# State is treated as the stratification variable to evaluate whether the
# observed AQI-asthma association persists after accounting for geographic
# differences across states.

cmh_state <- mantelhaen.test(
  aqi_asthma_state_table
)

cmh_state


# Extract results.

cmh_state_result <- tibble(

  Adjustment =
    "State",

  Statistic =
    as.numeric(
      cmh_state$statistic
    ),

  Degrees_of_Freedom =
    as.numeric(
      cmh_state$parameter
    ),

  P_Value =
    cmh_state$p.value
) %>%

  mutate(

    Statistic = round(
      Statistic,
      3
    ),

    Degrees_of_Freedom = round(
      Degrees_of_Freedom,
      0
    ),

    P_Value = round(
      P_Value,
      6
    )
  )


cmh_state_result


# Export Table 12 result.

write_csv(
  cmh_state_result,
  cmh_state_file
)


# 12. COMPARE SEX- AND STATE-ADJUSTED RESULTS

cmh_comparison <- bind_rows(
  cmh_sex_result,
  cmh_state_result
)

cmh_comparison


# 13. FINAL SUMMARY

message(
  "Stratified analysis complete."
)

message(
  paste(
    "Sex-stratified contingency table saved to:",
    sex_table_file
  )
)

message(
  paste(
    "Sex-adjusted CMH results saved to:",
    cmh_sex_file
  )
)

message(
  paste(
    "State-stratified contingency table saved to:",
    state_table_file
  )
)

message(
  paste(
    "State-adjusted CMH results saved to:",
    cmh_state_file
  )
)

