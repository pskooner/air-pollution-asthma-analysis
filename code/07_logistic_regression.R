# 07_logistic_regression.R

# Air Pollution and Asthma Burden in the United States

# Purpose:
# Fit crude and multivariable logistic regression models evaluating the
# association between air-pollution exposure and elevated asthma emergency
# department (ED) burden.

# Models:

#   Model 1:
#     Asthma_ED_Status ~ Annual_Weighted_AQI

#   Model 2:
#     Asthma_ED_Status ~ AQI_Category

#   Model 3:
#     Asthma_ED_Status ~ AQI_Category + State

#   Model 4 (Primary/Base Model):
#     Asthma_ED_Status ~ Annual_Weighted_AQI + State

#   Model 5 (Expanded Model):
#     Asthma_ED_Status ~ Annual_Weighted_AQI + State + Pollutant_Category

# Input:
#   data/processed/Combined_AQI_Asthma_2023_Cleaned.csv

# Outputs:
#   results/tables/aqi_category_crude_adjusted.csv
#   results/tables/continuous_aqi_logistic_regression.csv
#   results/tables/categorical_aqi_logistic_regression.csv
#   results/tables/base_logistic_regression.csv
#   results/tables/expanded_logistic_regression.csv
#   results/tables/categorical_aqi_state_model.csv
#   results/tables/logistic_regression_models.csv

# Author:
#   Parminder S. Kooner

# 1. LOAD REQUIRED PACKAGES

library(readr)
library(dplyr)
library(broom)


# 2. DEFINE FILE PATHS

input_file <-
  "data/processed/Combined_AQI_Asthma_2023_Cleaned.csv"


crude_adjusted_file <-
  "results/tables/aqi_category_crude_adjusted.csv"

continuous_model_file <-
  "results/tables/continuous_aqi_logistic_regression.csv"

categorical_model_file <-
  "results/tables/categorical_aqi_logistic_regression.csv"

base_model_file <-
  "results/tables/base_logistic_regression.csv"

expanded_model_file <-
  "results/tables/expanded_logistic_regression.csv"

categorical_state_file <-
  "results/tables/categorical_aqi_state_model.csv"

combined_models_file <-
  "results/tables/logistic_regression_models.csv"

# 3. IMPORT ANALYTICAL DATASET

data <- read_csv(
  input_file,
  show_col_types = FALSE
)

# 4. FORMAT VARIABLES FOR LOGISTIC REGRESSION

# Asthma_ED_Status:

#   Reference = Not Elevated
#   Event     = Elevated

# AQI_Category:

#   Reference = Low

# AQI_Category is intentionally treated as an UNORDERED factor for logistic
# regression so coefficients represent:

#   Moderate vs Low
#   High vs Low

data <- data %>%
  mutate(

    Asthma_ED_Status = factor(
      Asthma_ED_Status,
      levels = c(
        "Not Elevated",
        "Elevated"
      )
    ),

    AQI_Category = factor(
      AQI_Category,
      levels = c(
        "Low",
        "Moderate",
        "High"
      ),
      ordered = FALSE
    ),

    State = factor(
      State
    ),

    Pollutant_Category = factor(
      Pollutant_Category
    )
  )


# Confirm reference levels.

levels(data$Asthma_ED_Status)

levels(data$AQI_Category)

levels(data$State)

levels(data$Pollutant_Category)


# 5. CRUDE CATEGORICAL AQI MODEL

# Evaluates the unadjusted association between AQI category and elevated
# asthma ED status.

model_crude <- glm(
  Asthma_ED_Status ~ AQI_Category,
  data = data,
  family = binomial(link = "logit")
)


summary(model_crude)


# Odds ratios and 95% confidence intervals.

crude_results <- tidy(
  model_crude,
  exponentiate = TRUE,
  conf.int = TRUE
)


crude_results


# 6. STATE-ADJUSTED CATEGORICAL AQI MODEL

# State is included to account for geographic variation and potential
# confounding.

model_adjusted <- glm(
  Asthma_ED_Status ~ AQI_Category + State,
  data = data,
  family = binomial(link = "logit")
)


summary(model_adjusted)


# Odds ratios and 95% confidence intervals.

adjusted_results <- tidy(
  model_adjusted,
  exponentiate = TRUE,
  conf.int = TRUE
)


adjusted_results


# 7. TABLE 13 — CRUDE VS STATE-ADJUSTED AQI ODDS RATIOS

# Retain only the AQI-category coefficients so the crude and adjusted exposure
# estimates can be compared directly.

crude_aqi <- crude_results %>%

  filter(
    term %in% c(
      "AQI_CategoryModerate",
      "AQI_CategoryHigh"
    )
  ) %>%

  transmute(

    Model =
      "Crude",

    Comparison = recode(
      term,

      AQI_CategoryModerate =
        "Moderate vs Low",

      AQI_CategoryHigh =
        "High vs Low"
    ),

    Odds_Ratio =
      estimate,

    CI_Lower =
      conf.low,

    CI_Upper =
      conf.high,

    P_Value =
      p.value
  )


adjusted_aqi <- adjusted_results %>%

  filter(
    term %in% c(
      "AQI_CategoryModerate",
      "AQI_CategoryHigh"
    )
  ) %>%

  transmute(

    Model =
      "Adjusted for State",

    Comparison = recode(
      term,

      AQI_CategoryModerate =
        "Moderate vs Low",

      AQI_CategoryHigh =
        "High vs Low"
    ),

    Odds_Ratio =
      estimate,

    CI_Lower =
      conf.low,

    CI_Upper =
      conf.high,

    P_Value =
      p.value
  )


crude_adjusted_aqi <- bind_rows(
  crude_aqi,
  adjusted_aqi
) %>%

  mutate(

    across(
      c(
        Odds_Ratio,
        CI_Lower,
        CI_Upper
      ),
      ~ round(.x, 3)
    ),

    P_Value = round(
      P_Value,
      6
    )
  )


crude_adjusted_aqi


write_csv(
  crude_adjusted_aqi,
  crude_adjusted_file
)

# 8. MODEL 1 — CONTINUOUS AQI, UNADJUSTED

# This model evaluates the change in odds of elevated asthma ED burden
# associated with a one-unit increase in Annual_Weighted_AQI.

model_continuous <- glm(
  Asthma_ED_Status ~ Annual_Weighted_AQI,
  data = data,
  family = binomial(link = "logit")
)


summary(model_continuous)


# Log-odds coefficients.

continuous_log <- tidy(
  model_continuous,
  conf.int = TRUE,
  exponentiate = FALSE
)


# Odds ratios.

continuous_or <- tidy(
  model_continuous,
  conf.int = TRUE,
  exponentiate = TRUE
)


# Combine coefficient and OR results.

continuous_results <- continuous_log %>%

  transmute(

    Term =
      term,

    Coefficient =
      estimate,

    Std_Error =
      std.error,

    Z_Value =
      statistic,

    P_Value =
      p.value,

    LogOdds_CI_Lower =
      conf.low,

    LogOdds_CI_Upper =
      conf.high,

    Odds_Ratio =
      continuous_or$estimate,

    OR_CI_Lower =
      continuous_or$conf.low,

    OR_CI_Upper =
      continuous_or$conf.high
  ) %>%

  mutate(

    across(
      c(
        Coefficient,
        Std_Error,
        Z_Value,
        LogOdds_CI_Lower,
        LogOdds_CI_Upper,
        Odds_Ratio,
        OR_CI_Lower,
        OR_CI_Upper
      ),
      ~ round(.x, 4)
    ),

    P_Value = round(
      P_Value,
      6
    )
  )


continuous_results


write_csv(
  continuous_results,
  continuous_model_file
)


# 9. MODEL 2 — CATEGORICAL AQI, UNADJUSTED

model_categorical <- glm(
  Asthma_ED_Status ~ AQI_Category,
  data = data,
  family = binomial(link = "logit")
)


summary(model_categorical)


categorical_log <- tidy(
  model_categorical,
  conf.int = TRUE,
  exponentiate = FALSE
)


categorical_or <- tidy(
  model_categorical,
  conf.int = TRUE,
  exponentiate = TRUE
)


categorical_results <- categorical_log %>%

  transmute(

    Term =
      term,

    Coefficient =
      estimate,

    Std_Error =
      std.error,

    Z_Value =
      statistic,

    P_Value =
      p.value,

    LogOdds_CI_Lower =
      conf.low,

    LogOdds_CI_Upper =
      conf.high,

    Odds_Ratio =
      categorical_or$estimate,

    OR_CI_Lower =
      categorical_or$conf.low,

    OR_CI_Upper =
      categorical_or$conf.high
  ) %>%

  mutate(

    across(
      c(
        Coefficient,
        Std_Error,
        Z_Value,
        LogOdds_CI_Lower,
        LogOdds_CI_Upper,
        Odds_Ratio,
        OR_CI_Lower,
        OR_CI_Upper
      ),
      ~ round(.x, 4)
    ),

    P_Value = round(
      P_Value,
      6
    )
  )


categorical_results


write_csv(
  categorical_results,
  categorical_model_file
)


# 10. MODEL 3 — PRIMARY / BASE MULTIVARIABLE MODEL

# Primary model:

#   Asthma_ED_Status ~ Annual_Weighted_AQI + State

# This model estimates the association between continuous annual AQI and
# elevated asthma ED status after accounting for state-level differences.

model_base <- glm(
  Asthma_ED_Status ~ Annual_Weighted_AQI + State,
  data = data,
  family = binomial(link = "logit")
)


summary(model_base)


# Log-odds estimates.

base_log <- tidy(
  model_base,
  conf.int = TRUE,
  exponentiate = FALSE
)


# Odds-ratio estimates.

base_or <- tidy(
  model_base,
  conf.int = TRUE,
  exponentiate = TRUE
)


base_results <- base_log %>%

  transmute(

    Term =
      term,

    Coefficient =
      estimate,

    Std_Error =
      std.error,

    Z_Value =
      statistic,

    P_Value =
      p.value,

    LogOdds_CI_Lower =
      conf.low,

    LogOdds_CI_Upper =
      conf.high,

    Odds_Ratio =
      base_or$estimate,

    OR_CI_Lower =
      base_or$conf.low,

    OR_CI_Upper =
      base_or$conf.high
  ) %>%

  mutate(

    across(
      c(
        Coefficient,
        Std_Error,
        Z_Value,
        LogOdds_CI_Lower,
        LogOdds_CI_Upper,
        Odds_Ratio,
        OR_CI_Lower,
        OR_CI_Upper
      ),
      ~ round(.x, 4)
    ),

    P_Value = round(
      P_Value,
      6
    )
  )


base_results


write_csv(
  base_results,
  base_model_file
)


# 11. MODEL 4 — EXPANDED MODEL WITH POLLUTANT CATEGORY

# This model evaluates whether dominant pollutant category contributes
# additional information beyond annual weighted AQI and state.

model_expanded <- glm(
  Asthma_ED_Status ~
    Annual_Weighted_AQI +
    State +
    Pollutant_Category,
  data = data,
  family = binomial(link = "logit")
)


summary(model_expanded)


expanded_log <- tidy(
  model_expanded,
  conf.int = TRUE,
  exponentiate = FALSE
)


expanded_or <- tidy(
  model_expanded,
  conf.int = TRUE,
  exponentiate = TRUE
)


expanded_results <- expanded_log %>%

  transmute(

    Term =
      term,

    Coefficient =
      estimate,

    Std_Error =
      std.error,

    Z_Value =
      statistic,

    P_Value =
      p.value,

    LogOdds_CI_Lower =
      conf.low,

    LogOdds_CI_Upper =
      conf.high,

    Odds_Ratio =
      expanded_or$estimate,

    OR_CI_Lower =
      expanded_or$conf.low,

    OR_CI_Upper =
      expanded_or$conf.high
  ) %>%

  mutate(

    across(
      c(
        Coefficient,
        Std_Error,
        Z_Value,
        LogOdds_CI_Lower,
        LogOdds_CI_Upper,
        Odds_Ratio,
        OR_CI_Lower,
        OR_CI_Upper
      ),
      ~ round(.x, 4)
    ),

    P_Value = round(
      P_Value,
      6
    )
  )


expanded_results


write_csv(
  expanded_results,
  expanded_model_file
)


# 12. MODEL 5 — ALTERNATIVE CATEGORICAL AQI + STATE MODEL

# This model evaluates AQI as an ordinal exposure represented by categorical
# indicator variables rather than the continuous annual weighted AQI measure.

model_categorical_state <- glm(
  Asthma_ED_Status ~ AQI_Category + State,
  data = data,
  family = binomial(link = "logit")
)


summary(model_categorical_state)


categorical_state_log <- tidy(
  model_categorical_state,
  conf.int = TRUE,
  exponentiate = FALSE
)


categorical_state_or <- tidy(
  model_categorical_state,
  conf.int = TRUE,
  exponentiate = TRUE
)


categorical_state_results <- categorical_state_log %>%

  transmute(

    Term =
      term,

    Coefficient =
      estimate,

    Std_Error =
      std.error,

    Z_Value =
      statistic,

    P_Value =
      p.value,

    LogOdds_CI_Lower =
      conf.low,

    LogOdds_CI_Upper =
      conf.high,

    Odds_Ratio =
      categorical_state_or$estimate,

    OR_CI_Lower =
      categorical_state_or$conf.low,

    OR_CI_Upper =
      categorical_state_or$conf.high
  ) %>%

  mutate(

    across(
      c(
        Coefficient,
        Std_Error,
        Z_Value,
        LogOdds_CI_Lower,
        LogOdds_CI_Upper,
        Odds_Ratio,
        OR_CI_Lower,
        OR_CI_Upper
      ),
      ~ round(.x, 4)
    ),

    P_Value = round(
      P_Value,
      6
    )
  )


categorical_state_results


write_csv(
  categorical_state_results,
  categorical_state_file
)


# 13. CREATE COMBINED LOGISTIC REGRESSION RESULTS FILE

# Combine all fitted models into one machine-readable file for easy comparison
# and use in the GitHub results documentation.

all_regression_results <- bind_rows(

  continuous_results %>%
    mutate(
      Model =
        "Continuous AQI - Crude"
    ),

  categorical_results %>%
    mutate(
      Model =
        "AQI Category - Crude"
    ),

  categorical_state_results %>%
    mutate(
      Model =
        "AQI Category + State"
    ),

  base_results %>%
    mutate(
      Model =
        "Annual Weighted AQI + State"
    ),

  expanded_results %>%
    mutate(
      Model =
        "Annual Weighted AQI + State + Pollutant"
    )
) %>%

  select(
    Model,
    everything()
  )


all_regression_results


write_csv(
  all_regression_results,
  combined_models_file
)


# 14. EXTRACT PRIMARY AQI EFFECT FROM BASE MODEL

# This provides a concise view of the primary exposure estimate from the final
# candidate model prior to formal model comparison and diagnostics.

primary_aqi_effect <- base_results %>%

  filter(
    Term == "Annual_Weighted_AQI"
  )


primary_aqi_effect


# 15. MODEL FIT SUMMARY

# Basic model-fit statistics are displayed here for inspection.

# Formal comparisons between models are performed in:

#   08_model_selection.R

model_fit_summary <- tibble(

  Model = c(
    "Continuous AQI - Crude",
    "AQI Category - Crude",
    "AQI Category + State",
    "Annual Weighted AQI + State",
    "Annual Weighted AQI + State + Pollutant"
  ),

  N = c(
    nobs(model_continuous),
    nobs(model_categorical),
    nobs(model_categorical_state),
    nobs(model_base),
    nobs(model_expanded)
  ),

  AIC = c(
    AIC(model_continuous),
    AIC(model_categorical),
    AIC(model_categorical_state),
    AIC(model_base),
    AIC(model_expanded)
  ),

  Deviance = c(
    deviance(model_continuous),
    deviance(model_categorical),
    deviance(model_categorical_state),
    deviance(model_base),
    deviance(model_expanded)
  )
) %>%

  mutate(

    AIC = round(
      AIC,
      2
    ),

    Deviance = round(
      Deviance,
      2
    )
  )


model_fit_summary

# 16. FINAL SUMMARY

message(
  "Logistic regression analysis complete."
)


message(
  paste(
    "Crude and state-adjusted AQI estimates saved to:",
    crude_adjusted_file
  )
)


message(
  paste(
    "Continuous AQI model saved to:",
    continuous_model_file
  )
)


message(
  paste(
    "Categorical AQI model saved to:",
    categorical_model_file
  )
)


message(
  paste(
    "Primary/base model saved to:",
    base_model_file
  )
)


message(
  paste(
    "Expanded model saved to:",
    expanded_model_file
  )
)


message(
  paste(
    "Categorical AQI + State model saved to:",
    categorical_state_file
  )
)


message(
  paste(
    "Combined logistic regression results saved to:",
    combined_models_file
  )
)

