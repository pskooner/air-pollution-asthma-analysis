# 09_model_diagnostics.R

# Air Pollution and Asthma Burden in the United States

# Purpose:
# Evaluate the adequacy, stability, influence, goodness-of-fit, and
# discrimination of the final logistic regression model.

# Final model:
#   Asthma_ED_Status ~ Annual_Weighted_AQI + State

# Diagnostic procedures:
#   1. Multicollinearity assessment using VIF/GVIF
#   2. Pearson residual assessment
#   3. Deviance residual assessment
#   4. Residuals versus fitted values
#   5. Cook's distance
#   6. Identification of influential observations
#   7. Pearson goodness-of-fit test
#   8. Hosmer-Lemeshow goodness-of-fit test
#   9. Likelihood-ratio test for contribution of AQI
#  10. ROC curve and AUC

# Input:
#   data/processed/Combined_AQI_Asthma_2023_Cleaned.csv

# Outputs:
#   results/tables/vif_results.csv
#   results/tables/influential_observations.csv
#   results/tables/goodness_of_fit.csv
#   results/tables/aqi_contribution_lrt.csv
#   results/tables/roc_auc.csv

#   figures/pearson_residuals.png
#   figures/deviance_residuals.png
#   figures/residuals_vs_fitted.png
#   figures/cooks_distance.png
#   figures/roc_final_model.png

# Author:
#   Parminder S. Kooner


# 1. LOAD REQUIRED PACKAGES

library(readr)
library(dplyr)
library(tibble)
library(broom)
library(car)
library(ResourceSelection)
library(pROC)


# 2. DEFINE FILE PATHS

input_file <-
  "data/processed/Combined_AQI_Asthma_2023_Cleaned.csv"


# Results

vif_file <-
  "results/tables/vif_results.csv"

influence_file <-
  "results/tables/influential_observations.csv"

goodness_of_fit_file <-
  "results/tables/goodness_of_fit.csv"

aqi_lrt_file <-
  "results/tables/aqi_contribution_lrt.csv"

roc_auc_file <-
  "results/tables/roc_auc.csv"


# Figures

pearson_figure_file <-
  "figures/pearson_residuals.png"

deviance_figure_file <-
  "figures/deviance_residuals.png"

residual_fitted_file <-
  "figures/residuals_vs_fitted.png"

cooks_figure_file <-
  "figures/cooks_distance.png"

roc_figure_file <-
  "figures/roc_final_model.png"


# 3. IMPORT ANALYTICAL DATASET

data <- read_csv(
  input_file,
  show_col_types = FALSE
)


# 4. FORMAT ANALYTICAL VARIABLES

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
    ),

    Pollutant_Category = factor(
      Pollutant_Category
    )
  )


# 5. FIT FINAL MODEL

# The final model contains continuous annual weighted AQI and State.

model_final <- glm(
  Asthma_ED_Status ~
    Annual_Weighted_AQI +
    State,
  data = data,
  family = binomial(link = "logit")
)


summary(model_final)


# 6. FIT EXPANDED MODEL FOR MULTICOLLINEARITY COMPARISON

# The expanded model additionally includes dominant pollutant category.

model_expanded <- glm(
  Asthma_ED_Status ~
    Annual_Weighted_AQI +
    State +
    Pollutant_Category,
  data = data,
  family = binomial(link = "logit")
)


# PART A — MULTICOLLINEARITY


# 7. CALCULATE VIF / GVIF

vif_final <- car::vif(
  model_final
)

vif_expanded <- car::vif(
  model_expanded
)


vif_final
vif_expanded


# 8. FORMAT VIF/GVIF OUTPUT

# car::vif() may return either:

#   - ordinary VIF values, or
#   - generalized VIF (GVIF) values when multi-level factors are present.

# For multi-degree-of-freedom terms, GVIF^(1/(2*Df)) provides a scaled measure
# that is more directly comparable across predictors.

format_vif <- function(
  vif_object,
  model_name
) {

  if (is.matrix(vif_object)) {

    result <- as.data.frame(
      vif_object
    ) %>%

      rownames_to_column(
        "Variable"
      )

    if ("GVIF^(1/(2*Df))" %in% names(result)) {

      result <- result %>%
        transmute(

          Model =
            model_name,

          Variable =
            Variable,

          GVIF =
            GVIF,

          Df =
            Df,

          Adjusted_GVIF =
            `GVIF^(1/(2*Df))`
        )

    } else {

      result <- result %>%
        mutate(
          Model = model_name
        )
    }

  } else {

    result <- tibble(

      Model =
        model_name,

      Variable =
        names(vif_object),

      VIF =
        as.numeric(vif_object)
    )
  }

  result
}


vif_final_results <- format_vif(
  vif_final,
  "Final Model"
)


vif_expanded_results <- format_vif(
  vif_expanded,
  "Expanded Model"
)


vif_results <- bind_rows(
  vif_final_results,
  vif_expanded_results
) %>%

  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 3)
    )
  )


vif_results


write_csv(
  vif_results,
  vif_file
)

# PART B — RESIDUAL DIAGNOSTICS

# 9. CALCULATE PEARSON AND DEVIANCE RESIDUALS

pearson_residuals <- residuals(
  model_final,
  type = "pearson"
)


deviance_residuals <- residuals(
  model_final,
  type = "deviance"
)


fitted_probabilities <- fitted(
  model_final
)


# 10. FIGURE 5 — PEARSON RESIDUALS

png(
  filename = pearson_figure_file,
  width = 2400,
  height = 1800,
  res = 300
)


plot(
  pearson_residuals,

  ylab =
    "Pearson Residuals",

  xlab =
    "Observation Index",

  main =
    "Pearson Residuals vs Observation Index",

  pch = 19
)


abline(
  h = 0,
  lwd = 2
)


lines(
  lowess(
    pearson_residuals
  ),
  lwd = 2
)


abline(
  h = c(
    -2,
    2
  ),
  lty = 2
)


grid()


dev.off()


# 11. FIGURE 6 — DEVIANCE RESIDUALS

png(
  filename = deviance_figure_file,
  width = 2400,
  height = 1800,
  res = 300
)


plot(
  deviance_residuals,

  ylab =
    "Deviance Residuals",

  xlab =
    "Observation Index",

  main =
    "Deviance Residuals vs Observation Index",

  pch = 19
)


abline(
  h = 0,
  lwd = 2
)


lines(
  lowess(
    deviance_residuals
  ),
  lwd = 2
)


abline(
  h = c(
    -2,
    2
  ),
  lty = 2
)


grid()


dev.off()


# 12. FIGURE 7 — RESIDUALS VS FITTED VALUES

png(
  filename = residual_fitted_file,
  width = 2400,
  height = 1800,
  res = 300
)


plot(
  fitted_probabilities,
  deviance_residuals,

  xlab =
    "Fitted Probability",

  ylab =
    "Deviance Residuals",

  main =
    "Deviance Residuals vs Fitted Values",

  pch = 19
)


abline(
  h = 0,
  lwd = 2
)


lines(
  lowess(
    fitted_probabilities,
    deviance_residuals
  ),
  lwd = 2
)


abline(
  h = c(
    -2,
    2
  ),
  lty = 2
)


grid()


dev.off()


# PART C — INFLUENTIAL OBSERVATIONS

# 13. CALCULATE COOK'S DISTANCE

cooks_distance <- cooks.distance(
  model_final
)


# Common screening threshold

cook_threshold <-
  4 / nobs(model_final)


cook_threshold


# 14. FIGURE 8 — COOK'S DISTANCE

png(
  filename = cooks_figure_file,
  width = 2400,
  height = 1800,
  res = 300
)


plot(
  cooks_distance,

  type = "h",

  lwd = 2,

  xlab =
    "Observation Index",

  ylab =
    "Cook's Distance",

  main =
    "Cook's Distance"
)


abline(
  h = cook_threshold,
  lwd = 2,
  lty = 2
)


# Highlight observations exceeding 4/n

influential_indices <- which(
  cooks_distance >
    cook_threshold
)


points(
  influential_indices,
  cooks_distance[
    influential_indices
  ],
  pch = 19
)


grid()


dev.off()


# 15. CREATE INFLUENTIAL-OBSERVATION TABLE

# Retain geographic identifiers from the original analytical dataset so that
# influential observations can be identified clearly.

influential_observations <- data %>%

  mutate(

    Observation =
      seq_len(n()),

    Cooks_Distance =
      cooks_distance
  ) %>%

  filter(
    Cooks_Distance >
      cook_threshold
  ) %>%

  arrange(
    desc(
      Cooks_Distance
    )
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

  mutate(
    Cooks_Distance = round(
      Cooks_Distance,
      5
    )
  )


influential_observations


write_csv(
  influential_observations,
  influence_file
)


# Number of influential observations

nrow(
  influential_observations
)

# PART D — MODEL GOODNESS OF FIT

# 16. DEVIANCE STATISTICS

model_deviance <-
  deviance(
    model_final
  )


null_deviance <-
  model_final$null.deviance


deviance_difference <-
  null_deviance -
  model_deviance


model_deviance
null_deviance
deviance_difference


# 17. PEARSON GOODNESS-OF-FIT TEST

pearson_chisq <- sum(
  pearson_residuals^2
)


pearson_df <-
  model_final$df.residual


pearson_p_value <- pchisq(
  pearson_chisq,
  df = pearson_df,
  lower.tail = FALSE
)


pearson_chisq
pearson_df
pearson_p_value


# 18. HOSMER-LEMESHOW GOODNESS-OF-FIT TEST

# Convert the factor outcome to 0/1:
#
#   Not Elevated = 0
#   Elevated     = 1

outcome_numeric <-
  as.numeric(
    data$Asthma_ED_Status
  ) - 1


hl_test <- hoslem.test(
  outcome_numeric,
  fitted_probabilities,
  g = 10
)


hl_test


# 19. CREATE GOODNESS-OF-FIT RESULTS TABLE

goodness_of_fit <- tibble(

  Measure = c(

    "Model Deviance",

    "Null Deviance",

    "Deviance Difference",

    "Pearson Chi-Square",

    "Hosmer-Lemeshow Test"
  ),

  Statistic = c(

    model_deviance,

    null_deviance,

    deviance_difference,

    pearson_chisq,

    as.numeric(
      hl_test$statistic
    )
  ),

  Degrees_of_Freedom = c(

    NA_real_,

    NA_real_,

    model_final$df.null -
      model_final$df.residual,

    pearson_df,

    as.numeric(
      hl_test$parameter
    )
  ),

  P_Value = c(

    NA_real_,

    NA_real_,

    NA_real_,

    pearson_p_value,

    hl_test$p.value
  )
) %>%

  mutate(

    Statistic = round(
      Statistic,
      3
    ),

    P_Value = round(
      P_Value,
      6
    )
  )


goodness_of_fit


write_csv(
  goodness_of_fit,
  goodness_of_fit_file
)


# PART E — CONTRIBUTION OF AQI TO MODEL FIT

# 20. FIT STATE-ONLY MODEL

model_state_only <- glm(
  Asthma_ED_Status ~ State,
  data = data,
  family = binomial(link = "logit")
)


# 21. LIKELIHOOD-RATIO TEST FOR AQI

# This evaluates whether adding Annual_Weighted_AQI significantly improves
# model fit beyond State alone.

aqi_lrt <- anova(
  model_state_only,
  model_final,
  test = "Chisq"
)


aqi_lrt


aqi_contribution <- tidy(
  aqi_lrt
) %>%

  filter(
    !is.na(p.value)
  ) %>%

  transmute(

    Comparison =
      "State only vs State + Annual Weighted AQI",

    Chi_Square =
      deviance,

    Degrees_of_Freedom =
      df,

    P_Value =
      p.value
  ) %>%

  mutate(

    Chi_Square = round(
      Chi_Square,
      3
    ),

    P_Value = round(
      P_Value,
      6
    )
  )


aqi_contribution


write_csv(
  aqi_contribution,
  aqi_lrt_file
)


# PART F — ROC CURVE AND AUC

# 22. CALCULATE ROC CURVE

roc_final <- roc(
  response = outcome_numeric,
  predictor = fitted_probabilities,
  quiet = TRUE
)


auc_final <- auc(
  roc_final
)


auc_final


# 23. SAVE AUC RESULT

roc_auc_results <- tibble(

  Model =
    "Annual Weighted AQI + State",

  AUC =
    as.numeric(
      auc_final
    )
) %>%

  mutate(
    AUC = round(
      AUC,
      4
    )
  )


roc_auc_results


write_csv(
  roc_auc_results,
  roc_auc_file
)


# 24. FIGURE 9 — ROC CURVE

png(
  filename = roc_figure_file,
  width = 2400,
  height = 1800,
  res = 300
)


plot(
  roc_final,

  lwd = 3,

  main =
    "ROC Curve for Final Logistic Regression Model",

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
        auc_final
      ),
      3
    )
  ),

  cex = 1.2
)


dev.off()

# 25. FINAL DIAGNOSTIC SUMMARY

message(
  "Model diagnostics complete."
)


message(
  paste(
    "VIF/GVIF results saved to:",
    vif_file
  )
)


message(
  paste(
    "Influential observations saved to:",
    influence_file
  )
)


message(
  paste(
    "Goodness-of-fit results saved to:",
    goodness_of_fit_file
  )
)


message(
  paste(
    "AQI contribution LRT saved to:",
    aqi_lrt_file
  )
)


message(
  paste(
    "ROC/AUC results saved to:",
    roc_auc_file
  )
)


message(
  paste(
    "Pearson residual figure saved to:",
    pearson_figure_file
  )
)


message(
  paste(
    "Deviance residual figure saved to:",
    deviance_figure_file
  )
)


message(
  paste(
    "Residual-vs-fitted figure saved to:",
    residual_fitted_file
  )
)


message(
  paste(
    "Cook's distance figure saved to:",
    cooks_figure_file
  )
)


message(
  paste(
    "ROC figure saved to:",
    roc_figure_file
  )
)

