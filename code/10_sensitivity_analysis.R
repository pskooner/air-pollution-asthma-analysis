# 10_sensitivity_analysis.R

# Air Pollution and Asthma Burden in the United States

# Purpose:
# Assess the robustness of the final logistic regression model to influential
# observations identified using Cook's distance.

# Final model:
#   Asthma_ED_Status ~ Annual_Weighted_AQI + State

# Sensitivity workflow:
#   1. Fit the final model
#   2. Calculate Cook's distance
#   3. Identify observations exceeding the 4/n threshold
#   4. Remove influential observations
#   5. Refit the final model
#   6. Compare AQI coefficients
#   7. Compare model fit
#   8. Compare coefficients graphically
#   9. Recalculate ROC curve and AUC

# Input:
#   data/processed/Combined_AQI_Asthma_2023_Cleaned.csv

# Outputs:
#   results/tables/sensitivity_removed_observations.csv
#   results/tables/sensitivity_model_comparison.csv
#   results/tables/sensitivity_all_coefficients.csv
#   results/tables/sensitivity_auc_comparison.csv

#   figures/cooks_distance_influential.png
#   figures/coefficient_comparison.png
#   figures/roc_sensitivity_model.png

# Author:
#   Parminder S. Kooner

# 1. LOAD REQUIRED PACKAGES

library(readr)
library(dplyr)
library(broom)
library(tibble)
library(pROC)


# 2. DEFINE FILE PATHS

input_file <-
  "data/processed/Combined_AQI_Asthma_2023_Cleaned.csv"


# Results

removed_observations_file <-
  "results/tables/sensitivity_removed_observations.csv"

model_comparison_file <-
  "results/tables/sensitivity_model_comparison.csv"

coefficient_results_file <-
  "results/tables/sensitivity_all_coefficients.csv"

auc_comparison_file <-
  "results/tables/sensitivity_auc_comparison.csv"


# Figures

cooks_figure_file <-
  "figures/cooks_distance_influential.png"

coefficient_figure_file <-
  "figures/coefficient_comparison.png"

roc_clean_figure_file <-
  "figures/roc_sensitivity_model.png"


# 3. IMPORT ANALYTICAL DATASET

data <- read_csv(
  input_file,
  show_col_types = FALSE
)


# 4. FORMAT VARIABLES

data <- data %>%
  mutate(

    Asthma_ED_Status = factor(
      Asthma_ED_Status,
      levels = c(
        "Not Elevated",
        "Elevated"
      )
    ),

    State = factor(
      State
    )
  )


# 5. FIT ORIGINAL FINAL MODEL

model_final <- glm(
  Asthma_ED_Status ~
    Annual_Weighted_AQI +
    State,
  data = data,
  family = binomial(link = "logit")
)


summary(model_final)


# 6. CALCULATE COOK'S DISTANCE

cooks_distance <- cooks.distance(
  model_final
)


cook_threshold <-
  4 / nrow(data)


cook_threshold


# Identify influential observations.

influential_indices <- which(
  cooks_distance >
    cook_threshold
)


influential_indices


# Number of influential observations.

length(
  influential_indices
)


# 7. SAVE INFLUENTIAL OBSERVATIONS

removed_observations <- data %>%

  mutate(

    Observation =
      seq_len(n()),

    Cooks_Distance =
      cooks_distance
  ) %>%

  filter(
    Observation %in%
      influential_indices
  ) %>%

  select(

    Observation,

    State,

    County,

    Sex,

    Asthma_ED_Status,

    Annual_Weighted_AQI,

    Cooks_Distance
  ) %>%

  arrange(
    desc(
      Cooks_Distance
    )
  ) %>%

  mutate(
    Cooks_Distance = round(
      Cooks_Distance,
      5
    )
  )


removed_observations


write_csv(
  removed_observations,
  removed_observations_file
)


# 8. FIGURE 10 — COOK'S DISTANCE WITH INFLUENTIAL POINTS HIGHLIGHTED

png(
  filename = cooks_figure_file,
  width = 2400,
  height = 1800,
  res = 300
)


plot(
  cooks_distance,

  xlab =
    "Observation Index",

  ylab =
    "Cook's Distance",

  main =
    "Cook's Distance: Influential Observations",

  pch = 16
)


abline(
  h = cook_threshold,
  lwd = 2,
  lty = 2
)


points(
  influential_indices,

  cooks_distance[
    influential_indices
  ],

  pch = 16,

  cex = 1.2
)


dev.off()


# 9. CREATE SENSITIVITY-ANALYSIS DATASET

# Remove observations exceeding the Cook's-distance threshold.

data_sensitivity <- data[
  -influential_indices,
]


# Check sample sizes.

n_original <-
  nrow(data)

n_sensitivity <-
  nrow(data_sensitivity)

n_removed <-
  n_original -
  n_sensitivity


n_original
n_sensitivity
n_removed


# 10. REFIT FINAL MODEL AFTER REMOVING INFLUENTIAL OBSERVATIONS

model_sensitivity <- glm(
  Asthma_ED_Status ~
    Annual_Weighted_AQI +
    State,
  data = data_sensitivity,
  family = binomial(link = "logit")
)


summary(model_sensitivity)


# 11. EXTRACT ORIGINAL MODEL RESULTS

original_tidy <- tidy(
  model_final,
  conf.int = TRUE,
  exponentiate = FALSE
)


original_or <- tidy(
  model_final,
  conf.int = TRUE,
  exponentiate = TRUE
)


# 12. EXTRACT SENSITIVITY MODEL RESULTS

sensitivity_tidy <- tidy(
  model_sensitivity,
  conf.int = TRUE,
  exponentiate = FALSE
)


sensitivity_or <- tidy(
  model_sensitivity,
  conf.int = TRUE,
  exponentiate = TRUE
)


# 13. COMPARE PRIMARY AQI EFFECT

original_aqi <- original_tidy %>%

  filter(
    term ==
      "Annual_Weighted_AQI"
  ) %>%

  transmute(

    Model =
      "Original Final Model",

    N =
      nobs(model_final),

    Estimate =
      estimate,

    Std_Error =
      std.error,

    P_Value =
      p.value,

    LogOdds_CI_Lower =
      conf.low,

    LogOdds_CI_Upper =
      conf.high,

    Odds_Ratio =
      original_or$estimate[
        original_or$term ==
          "Annual_Weighted_AQI"
      ],

    OR_CI_Lower =
      original_or$conf.low[
        original_or$term ==
          "Annual_Weighted_AQI"
      ],

    OR_CI_Upper =
      original_or$conf.high[
        original_or$term ==
          "Annual_Weighted_AQI"
      ]
  )


sensitivity_aqi <- sensitivity_tidy %>%

  filter(
    term ==
      "Annual_Weighted_AQI"
  ) %>%

  transmute(

    Model =
      "Sensitivity Model",

    N =
      nobs(model_sensitivity),

    Estimate =
      estimate,

    Std_Error =
      std.error,

    P_Value =
      p.value,

    LogOdds_CI_Lower =
      conf.low,

    LogOdds_CI_Upper =
      conf.high,

    Odds_Ratio =
      sensitivity_or$estimate[
        sensitivity_or$term ==
          "Annual_Weighted_AQI"
      ],

    OR_CI_Lower =
      sensitivity_or$conf.low[
        sensitivity_or$term ==
          "Annual_Weighted_AQI"
      ],

    OR_CI_Upper =
      sensitivity_or$conf.high[
        sensitivity_or$term ==
          "Annual_Weighted_AQI"
      ]
  )


aqi_comparison <- bind_rows(
  original_aqi,
  sensitivity_aqi
)


# 14. ADD MODEL-FIT STATISTICS

model_fit_comparison <- tibble(

  Model = c(
    "Original Final Model",
    "Sensitivity Model"
  ),

  AIC = c(
    AIC(model_final),
    AIC(model_sensitivity)
  ),

  Deviance = c(
    deviance(model_final),
    deviance(model_sensitivity)
  )
)


sensitivity_model_comparison <- aqi_comparison %>%

  left_join(
    model_fit_comparison,
    by = "Model"
  ) %>%

  mutate(

    across(
      c(
        Estimate,
        Std_Error,
        LogOdds_CI_Lower,
        LogOdds_CI_Upper,
        Odds_Ratio,
        OR_CI_Lower,
        OR_CI_Upper
      ),
      ~ round(.x, 4)
    ),

    AIC = round(
      AIC,
      2
    ),

    Deviance = round(
      Deviance,
      2
    ),

    P_Value = round(
      P_Value,
      6
    )
  )


sensitivity_model_comparison


write_csv(
  sensitivity_model_comparison,
  model_comparison_file
)


# 15. COMPARE ALL MODEL COEFFICIENTS

original_coefficients <- original_tidy %>%

  transmute(

    Term =
      term,

    Original_Estimate =
      estimate
  )


sensitivity_coefficients <- sensitivity_tidy %>%

  transmute(

    Term =
      term,

    Sensitivity_Estimate =
      estimate
  )


coefficient_comparison <- full_join(
  original_coefficients,
  sensitivity_coefficients,
  by = "Term"
) %>%

  mutate(

    Original_Estimate = round(
      Original_Estimate,
      4
    ),

    Sensitivity_Estimate = round(
      Sensitivity_Estimate,
      4
    )
  )


coefficient_comparison


write_csv(
  coefficient_comparison,
  coefficient_results_file
)


# 16. FIGURE — COEFFICIENT COMPARISON

# Plot only coefficients available in both models.

coefficient_plot_data <- coefficient_comparison %>%

  filter(
    !is.na(Original_Estimate),
    !is.na(Sensitivity_Estimate)
  )


png(
  filename = coefficient_figure_file,
  width = 2400,
  height = 1800,
  res = 300
)


plot(

  coefficient_plot_data$Original_Estimate,

  coefficient_plot_data$Sensitivity_Estimate,

  xlab =
    "Original Model Coefficients",

  ylab =
    "Sensitivity Model Coefficients",

  main =
    "Coefficient Comparison",

  pch = 19
)


abline(
  0,
  1,
  lwd = 2,
  lty = 2
)


dev.off()


# 17. ROC/AUC — ORIGINAL MODEL

outcome_original <-
  as.numeric(
    data$Asthma_ED_Status
  ) - 1


roc_original <- roc(

  response =
    outcome_original,

  predictor =
    fitted(model_final),

  quiet = TRUE
)


auc_original <- auc(
  roc_original
)


# 18. ROC/AUC — SENSITIVITY MODEL

outcome_sensitivity <-
  as.numeric(
    data_sensitivity$Asthma_ED_Status
  ) - 1


roc_sensitivity <- roc(

  response =
    outcome_sensitivity,

  predictor =
    fitted(model_sensitivity),

  quiet = TRUE
)


auc_sensitivity <- auc(
  roc_sensitivity
)


auc_original
auc_sensitivity


# 19. SAVE AUC COMPARISON

auc_comparison <- tibble(

  Model = c(
    "Original Final Model",
    "Sensitivity Model"
  ),

  N = c(
    nobs(model_final),
    nobs(model_sensitivity)
  ),

  AUC = c(
    as.numeric(
      auc_original
    ),

    as.numeric(
      auc_sensitivity
    )
  )
) %>%

  mutate(
    AUC = round(
      AUC,
      4
    )
  )


auc_comparison


write_csv(
  auc_comparison,
  auc_comparison_file
)


# 20. FIGURE 11 — ROC CURVE FOR SENSITIVITY MODEL

png(
  filename = roc_clean_figure_file,
  width = 2400,
  height = 1800,
  res = 300
)


plot(

  roc_sensitivity,

  lwd = 3,

  main =
    "ROC Curve After Removing Influential Observations",

  xlab =
    "False Positive Rate (1 - Specificity)",

  ylab =
    "True Positive Rate (Sensitivity)",

  legacy.axes = TRUE
)


text(

  x = 0.60,

  y = 0.20,

  labels = paste0(
    "AUC = ",
    round(
      as.numeric(
        auc_sensitivity
      ),
      3
    )
  ),

  cex = 1.2
)


dev.off()

# 21. FINAL SENSITIVITY SUMMARY

message(
  "Sensitivity analysis complete."
)


message(
  paste(
    "Original observations:",
    n_original
  )
)


message(
  paste(
    "Influential observations removed:",
    n_removed
  )
)


message(
  paste(
    "Sensitivity-analysis observations:",
    n_sensitivity
  )
)


message(
  paste(
    "Removed observations saved to:",
    removed_observations_file
  )
)


message(
  paste(
    "Model comparison saved to:",
    model_comparison_file
  )
)


message(
  paste(
    "Coefficient comparison saved to:",
    coefficient_results_file
  )
)


message(
  paste(
    "AUC comparison saved to:",
    auc_comparison_file
  )
)


message(
  paste(
    "Sensitivity ROC curve saved to:",
    roc_clean_figure_file
  )
)

