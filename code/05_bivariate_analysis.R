# 05_bivariate_analysis.R

# Air Pollution and Asthma Burden in the United States

# Purpose:
# Evaluate unadjusted associations between air-pollution exposure measures
# and elevated asthma emergency department (ED) burden using contingency-table
# methods.

# Primary analyses:
#   1. AQI Category × Asthma ED Status
#   2. Pollutant Category × Asthma ED Status

# Statistical methods:
#   - Pearson chi-square test
#   - Likelihood-ratio (G-test)
#   - Cochran-Armitage trend test for ordinal AQI category
#   - Row proportions
#   - Odds ratios
#   - Relative risks
#   - Pearson residuals
#   - Mosaic plots

# Input:
#   data/processed/Combined_AQI_Asthma_2023_Cleaned.csv

# Outputs:
#   results/tables/aqi_asthma_contingency.csv
#   results/tables/aqi_bivariate_tests.csv
#   results/tables/aqi_relative_risks.csv
#   results/tables/aqi_pearson_residuals.csv

#   results/tables/pollutant_asthma_contingency.csv
#   results/tables/pollutant_bivariate_tests.csv
#   results/tables/pollutant_pearson_residuals.csv

#   figures/aqi_asthma_mosaic.png
#   figures/pollutant_asthma_mosaic.png

# Author:
#   Parminder S. Kooner

# 1. LOAD REQUIRED PACKAGES

library(readr)
library(dplyr)
library(tidyr)
library(broom)
library(DescTools)
library(epitools)
library(vcd)

# 2. DEFINE FILE PATHS

input_file <-
  "data/processed/Combined_AQI_Asthma_2023_Cleaned.csv"

# AQI results

aqi_contingency_file <-
  "results/tables/aqi_asthma_contingency.csv"

aqi_tests_file <-
  "results/tables/aqi_bivariate_tests.csv"

aqi_rr_file <-
  "results/tables/aqi_relative_risks.csv"

aqi_residuals_file <-
  "results/tables/aqi_pearson_residuals.csv"

aqi_mosaic_file <-
  "figures/aqi_asthma_mosaic.png"


# Pollutant results

pollutant_contingency_file <-
  "results/tables/pollutant_asthma_contingency.csv"

pollutant_tests_file <-
  "results/tables/pollutant_bivariate_tests.csv"

pollutant_residuals_file <-
  "results/tables/pollutant_pearson_residuals.csv"

pollutant_mosaic_file <-
  "figures/pollutant_asthma_mosaic.png"

# 3. IMPORT ANALYTICAL DATASET

data <- read_csv(
  input_file,
  show_col_types = FALSE
)

# 4. FORMAT ANALYTICAL VARIABLES

# Re-establish factor ordering after CSV import.

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

    Pollutant_Category = factor(
      Pollutant_Category
    ),

    Asthma_ED_Status = factor(
      Asthma_ED_Status,
      levels = c(
        "Not Elevated",
        "Elevated"
      )
    )
  )


# Confirm reference/event coding.

levels(data$Asthma_ED_Status)

# Reference category: Not Elevated
# Event category:     Elevated


# PART A — AQI CATEGORY AND ASTHMA ED STATUS

# 5. AQI CATEGORY × ASTHMA ED STATUS CONTINGENCY TABLE

table_aqi_asthma <- table(
  data$AQI_Category,
  data$Asthma_ED_Status
)

table_aqi_asthma


# Row proportions

aqi_row_proportions <- prop.table(
  table_aqi_asthma,
  margin = 1
)

aqi_row_proportions


# Convert contingency table to a tidy data frame.

aqi_contingency <- as.data.frame(
  table_aqi_asthma
) %>%

  rename(
    AQI_Category = Var1,
    Asthma_ED_Status = Var2,
    Count = Freq
  ) %>%

  group_by(
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


aqi_contingency


# Export Table 4 data.

write_csv(
  aqi_contingency,
  aqi_contingency_file
)


# 6. PEARSON CHI-SQUARE TEST

chi_aqi <- chisq.test(
  table_aqi_asthma
)

chi_aqi


# Extract results.

chi_aqi_result <- tibble(

  Test =
    "Pearson Chi-Square",

  Statistic =
    as.numeric(chi_aqi$statistic),

  Degrees_of_Freedom =
    as.numeric(chi_aqi$parameter),

  P_Value =
    chi_aqi$p.value
)


# 7. LIKELIHOOD-RATIO TEST (G-TEST)

g_aqi <- GTest(
  table_aqi_asthma
)

g_aqi


# Extract results.

g_aqi_result <- tibble(

  Test =
    "Likelihood Ratio (G-Test)",

  Statistic =
    as.numeric(g_aqi$statistic),

  Degrees_of_Freedom =
    as.numeric(g_aqi$parameter),

  P_Value =
    g_aqi$p.value
)


# 8. COCHRAN-ARMITAGE TREND TEST

# AQI_Category is ordinal:

#   Low < Moderate < High

# Therefore a Cochran-Armitage trend test is used to evaluate whether the
# probability of elevated asthma ED status changes monotonically across
# increasing AQI exposure categories.

trend_aqi <- CochranArmitageTest(
  table_aqi_asthma
)

trend_aqi


# Extract results.

trend_aqi_result <- tibble(

  Test =
    "Cochran-Armitage Trend",

  Statistic =
    as.numeric(trend_aqi$statistic),

  Degrees_of_Freedom =
    NA_real_,

  P_Value =
    trend_aqi$p.value
)

# 9. COMBINE AQI BIVARIATE TEST RESULTS

aqi_bivariate_tests <- bind_rows(

  chi_aqi_result,

  g_aqi_result,

  trend_aqi_result

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


aqi_bivariate_tests

# Export Tables 5–7 statistical results.

write_csv(
  aqi_bivariate_tests,
  aqi_tests_file
)

# 10. EXPECTED CELL COUNTS

# Review expected counts to assess the chi-square approximation.

chi_aqi$expected

# Minimum expected count

min(
  chi_aqi$expected
)

# 11. PEARSON RESIDUALS

# Pearson residuals indicate which cells contribute most strongly to the
# overall chi-square statistic.

aqi_residual_matrix <- chi_aqi$residuals

aqi_residual_matrix

# Convert residual matrix to tidy format.

aqi_residuals <- as.data.frame(
  as.table(aqi_residual_matrix)
) %>%

  rename(
    AQI_Category = Var1,
    Asthma_ED_Status = Var2,
    Pearson_Residual = Freq
  ) %>%

  mutate(
    Pearson_Residual = round(
      Pearson_Residual,
      3
    )
  )

aqi_residuals

write_csv(
  aqi_residuals,
  aqi_residuals_file
)

# 12. ODDS-RATIO ANALYSIS

# Display odds-ratio estimates from the contingency table.

# This is retained from the original analysis as a descriptive effect-size
# assessment.

aqi_odds_ratio <- oddsratio(
  table_aqi_asthma
)

aqi_odds_ratio

# 13. RELATIVE RISKS ACROSS AQI CATEGORIES

# Calculate the probability of elevated asthma ED status within each AQI
# category.

risk_low <-
  table_aqi_asthma[
    "Low",
    "Elevated"
  ] /
  sum(
    table_aqi_asthma[
      "Low",
    ]
  )


risk_moderate <-
  table_aqi_asthma[
    "Moderate",
    "Elevated"
  ] /
  sum(
    table_aqi_asthma[
      "Moderate",
    ]
  )


risk_high <-
  table_aqi_asthma[
    "High",
    "Elevated"
  ] /
  sum(
    table_aqi_asthma[
      "High",
    ]
  )


# Pairwise relative risks.

rr_low_vs_high <-
  risk_low / risk_high

rr_low_vs_moderate <-
  risk_low / risk_moderate

rr_moderate_vs_high <-
  risk_moderate / risk_high


# Store results.

aqi_relative_risks <- tibble(

  Comparison = c(
    "Low vs High",
    "Low vs Moderate",
    "Moderate vs High"
  ),

  Relative_Risk = c(
    rr_low_vs_high,
    rr_low_vs_moderate,
    rr_moderate_vs_high
  )
) %>%

  mutate(
    Relative_Risk = round(
      Relative_Risk,
      3
    )
  )


aqi_relative_risks


write_csv(
  aqi_relative_risks,
  aqi_rr_file
)

# 14. FIGURE 3 — AQI CATEGORY × ASTHMA ED STATUS MOSAIC PLOT

png(
  filename = aqi_mosaic_file,
  width = 2400,
  height = 1800,
  res = 300
)


mosaic(
  ~ AQI_Category + Asthma_ED_Status,
  data = data,
  shade = TRUE,
  legend = TRUE,
  main =
    "Mosaic Plot of AQI Category and Asthma ED Status",
  xlab =
    "AQI Category",
  ylab =
    "Asthma ED Status"
)


dev.off()

# PART B — POLLUTANT CATEGORY AND ASTHMA ED STATUS

# 15. POLLUTANT CATEGORY × ASTHMA ED STATUS CONTINGENCY TABLE

table_pollutant_asthma <- table(
  data$Pollutant_Category,
  data$Asthma_ED_Status
)

table_pollutant_asthma


# Row proportions.

pollutant_row_proportions <- prop.table(
  table_pollutant_asthma,
  margin = 1
)

pollutant_row_proportions


# Convert to tidy data frame.

pollutant_contingency <- as.data.frame(
  table_pollutant_asthma
) %>%

  rename(
    Pollutant_Category = Var1,
    Asthma_ED_Status = Var2,
    Count = Freq
  ) %>%

  group_by(
    Pollutant_Category
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


pollutant_contingency


# Export Table 8 data.

write_csv(
  pollutant_contingency,
  pollutant_contingency_file
)

# 16. POLLUTANT CATEGORY PEARSON CHI-SQUARE TEST

chi_pollutant <- chisq.test(
  table_pollutant_asthma
)

chi_pollutant


chi_pollutant_result <- tibble(

  Test =
    "Pearson Chi-Square",

  Statistic =
    as.numeric(chi_pollutant$statistic),

  Degrees_of_Freedom =
    as.numeric(chi_pollutant$parameter),

  P_Value =
    chi_pollutant$p.value
)


# 17. POLLUTANT CATEGORY LIKELIHOOD-RATIO TEST

g_pollutant <- GTest(
  table_pollutant_asthma
)

g_pollutant


g_pollutant_result <- tibble(

  Test =
    "Likelihood Ratio (G-Test)",

  Statistic =
    as.numeric(g_pollutant$statistic),

  Degrees_of_Freedom =
    as.numeric(g_pollutant$parameter),

  P_Value =
    g_pollutant$p.value
)

# 18. COMBINE POLLUTANT TEST RESULTS

# Pollutant_Category is nominal rather than ordinal.

# Therefore a Cochran-Armitage trend test is not performed for this exposure.

pollutant_bivariate_tests <- bind_rows(

  chi_pollutant_result,

  g_pollutant_result

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


pollutant_bivariate_tests

# Export Table 9 and supporting likelihood-ratio results.

write_csv(
  pollutant_bivariate_tests,
  pollutant_tests_file
)

# 19. EXPECTED CELL COUNTS — POLLUTANT ANALYSIS

chi_pollutant$expected


min(
  chi_pollutant$expected
)

# 20. PEARSON RESIDUALS — POLLUTANT ANALYSIS

pollutant_residual_matrix <-
  chi_pollutant$residuals

pollutant_residual_matrix


pollutant_residuals <- as.data.frame(
  as.table(
    pollutant_residual_matrix
  )
) %>%

  rename(
    Pollutant_Category = Var1,
    Asthma_ED_Status = Var2,
    Pearson_Residual = Freq
  ) %>%

  mutate(
    Pearson_Residual = round(
      Pearson_Residual,
      3
    )
  )


pollutant_residuals


write_csv(
  pollutant_residuals,
  pollutant_residuals_file
)

# 21. POLLUTANT CATEGORY ODDS-RATIO ANALYSIS

pollutant_odds_ratio <- oddsratio(
  table_pollutant_asthma
)

pollutant_odds_ratio

# 22. FIGURE 4 — POLLUTANT CATEGORY × ASTHMA ED STATUS MOSAIC PLOT

png(
  filename = pollutant_mosaic_file,
  width = 2400,
  height = 1800,
  res = 300
)


mosaic(
  ~ Pollutant_Category + Asthma_ED_Status,
  data = data,
  shade = TRUE,
  legend = TRUE,
  main =
    "Mosaic Plot of Pollutant Category and Asthma ED Status",
  xlab =
    "Pollutant Category",
  ylab =
    "Asthma ED Status"
)


dev.off()

# 23. FINAL SUMMARY

message(
  "Bivariate analysis complete."
)


message(
  paste(
    "AQI contingency table saved to:",
    aqi_contingency_file
  )
)


message(
  paste(
    "AQI statistical tests saved to:",
    aqi_tests_file
  )
)


message(
  paste(
    "AQI relative risks saved to:",
    aqi_rr_file
  )
)


message(
  paste(
    "AQI mosaic plot saved to:",
    aqi_mosaic_file
  )
)


message(
  paste(
    "Pollutant contingency table saved to:",
    pollutant_contingency_file
  )
)


message(
  paste(
    "Pollutant statistical tests saved to:",
    pollutant_tests_file
  )
)


message(
  paste(
    "Pollutant mosaic plot saved to:",
    pollutant_mosaic_file
  )
)
