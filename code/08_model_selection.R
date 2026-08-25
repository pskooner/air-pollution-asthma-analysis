# 08_model_selection.R

# Air Pollution and Asthma Burden in the United States

# Purpose:
# Compare candidate logistic regression models, evaluate interaction effects,
# and assess variable selection using AIC-based procedures.

# Analyses:
#   1. Likelihood-ratio test for AQI category
#   2. AIC comparison of candidate multivariable models
#   3. Likelihood-ratio comparison of nested models
#   4. Interaction testing
#   5. Forward selection
#   6. Backward elimination
#   7. Bidirectional stepwise selection

# Input:
#   data/processed/Combined_AQI_Asthma_2023_Cleaned.csv

# Outputs:
#   results/tables/aqi_likelihood_ratio_test.csv
#   results/tables/model_comparison.csv
#   results/tables/interaction_tests.csv
#   results/tables/variable_selection.csv
#   results/tables/selected_model_coefficients.csv

# Author:
#   Parminder S. Kooner

# 1. LOAD REQUIRED PACKAGES

library(readr)
library(dplyr)
library(broom)
library(tibble)


# 2. DEFINE FILE PATHS

input_file <-
  "data/processed/Combined_AQI_Asthma_2023_Cleaned.csv"

aqi_lrt_file <-
  "results/tables/aqi_likelihood_ratio_test.csv"

model_comparison_file <-
  "results/tables/model_comparison.csv"

interaction_file <-
  "results/tables/interaction_tests.csv"

variable_selection_file <-
  "results/tables/variable_selection.csv"

selected_model_file <-
  "results/tables/selected_model_coefficients.csv"


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


# PART A — LIKELIHOOD-RATIO TEST FOR AQI CATEGORY

# 5. FIT NULL AND AQI CATEGORY MODELS

# Null model

model_aqi_null <- glm(
  Asthma_ED_Status ~ 1,
  data = data,
  family = binomial(link = "logit")
)


# AQI category model

model_aqi_full <- glm(
  Asthma_ED_Status ~ AQI_Category,
  data = data,
  family = binomial(link = "logit")
)


# 6. LIKELIHOOD-RATIO TEST

aqi_lrt <- anova(
  model_aqi_null,
  model_aqi_full,
  test = "Chisq"
)


aqi_lrt


# Extract the model-comparison row.

aqi_lrt_result <- tidy(
  aqi_lrt
) %>%

  filter(
    !is.na(p.value)
  ) %>%

  transmute(

    Comparison =
      "AQI Category vs Null Model",

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


aqi_lrt_result


write_csv(
  aqi_lrt_result,
  aqi_lrt_file
)


# PART B — CANDIDATE MULTIVARIABLE MODELS

# 7. FIT BASE MODEL

# Primary candidate model:
#
#   Continuous annual weighted AQI + State

model_base <- glm(
  Asthma_ED_Status ~
    Annual_Weighted_AQI +
    State,
  data = data,
  family = binomial(link = "logit")
)


summary(model_base)


# 8. FIT EXPANDED MODEL

# Expanded model adds dominant pollutant category.

model_expanded <- glm(
  Asthma_ED_Status ~
    Annual_Weighted_AQI +
    State +
    Pollutant_Category,
  data = data,
  family = binomial(link = "logit")
)


summary(model_expanded)

# 9. FIT ALTERNATIVE CATEGORICAL AQI MODEL

# Alternative exposure specification:
#
#   AQI category + State

model_alternative <- glm(
  Asthma_ED_Status ~
    AQI_Category +
    State,
  data = data,
  family = binomial(link = "logit")
)


summary(model_alternative)

# 10. COMPARE AIC VALUES

aic_comparison <- AIC(
  model_base,
  model_expanded,
  model_alternative
)


aic_comparison


# 11. LIKELIHOOD-RATIO TEST — BASE VS EXPANDED MODEL

# The expanded model is nested within the base-model framework because it
# adds Pollutant_Category.

lrt_base_expanded <- anova(
  model_base,
  model_expanded,
  test = "Chisq"
)


lrt_base_expanded


lrt_base_expanded_tidy <- tidy(
  lrt_base_expanded
) %>%

  filter(
    !is.na(p.value)
  )


# 12. BASE VS ALTERNATIVE EXPOSURE SPECIFICATION

# The continuous-AQI and categorical-AQI models use different representations
# of the exposure. AIC can be used to compare their relative fit.

# Because these models are not strictly nested, their likelihood-ratio
# comparison should not be interpreted as a formal nested-model LRT.

base_aic <-
  AIC(model_base)

expanded_aic <-
  AIC(model_expanded)

alternative_aic <-
  AIC(model_alternative)


# 13. CREATE MODEL COMPARISON TABLE

model_comparison <- tibble(

  Model = c(

    "Base: Annual Weighted AQI + State",

    "Expanded: Annual Weighted AQI + State + Pollutant",

    "Alternative: AQI Category + State"
  ),

  Number_of_Parameters = c(

    attr(
      logLik(model_base),
      "df"
    ),

    attr(
      logLik(model_expanded),
      "df"
    ),

    attr(
      logLik(model_alternative),
      "df"
    )
  ),

  Log_Likelihood = c(

    as.numeric(
      logLik(model_base)
    ),

    as.numeric(
      logLik(model_expanded)
    ),

    as.numeric(
      logLik(model_alternative)
    )
  ),

  AIC = c(
    base_aic,
    expanded_aic,
    alternative_aic
  )
) %>%

  mutate(

    Log_Likelihood = round(
      Log_Likelihood,
      3
    ),

    AIC = round(
      AIC,
      2
    ),

    Delta_AIC =
      round(
        AIC - min(AIC),
        2
      )
  )


model_comparison


write_csv(
  model_comparison,
  model_comparison_file
)


# 14. DISPLAY FORMAL BASE VS EXPANDED LRT

base_expanded_test <- lrt_base_expanded_tidy %>%

  transmute(

    Comparison =
      "Base vs Expanded",

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


base_expanded_test

# PART C — INTERACTION TESTING

# 15. AQI CATEGORY × STATE INTERACTION

# Main-effects model

model_cat_main <- glm(
  Asthma_ED_Status ~
    AQI_Category +
    State,
  data = data,
  family = binomial(link = "logit")
)


# Interaction model

model_cat_interaction <- glm(
  Asthma_ED_Status ~
    AQI_Category * State,
  data = data,
  family = binomial(link = "logit")
)


# Likelihood-ratio test

lrt_cat_state <- anova(
  model_cat_main,
  model_cat_interaction,
  test = "Chisq"
)


lrt_cat_state


# 16. ANNUAL WEIGHTED AQI × STATE INTERACTION

model_aqi_state_interaction <- glm(
  Asthma_ED_Status ~
    Annual_Weighted_AQI * State,
  data = data,
  family = binomial(link = "logit")
)


lrt_aqi_state <- anova(
  model_base,
  model_aqi_state_interaction,
  test = "Chisq"
)


lrt_aqi_state


# 17. ANNUAL WEIGHTED AQI × POLLUTANT INTERACTION

# Main-effects model

model_pollutant_main <- glm(
  Asthma_ED_Status ~
    Annual_Weighted_AQI +
    Pollutant_Category +
    State,
  data = data,
  family = binomial(link = "logit")
)


# Interaction model

model_pollutant_interaction <- glm(
  Asthma_ED_Status ~
    Annual_Weighted_AQI *
    Pollutant_Category +
    State,
  data = data,
  family = binomial(link = "logit")
)


lrt_aqi_pollutant <- anova(
  model_pollutant_main,
  model_pollutant_interaction,
  test = "Chisq"
)


lrt_aqi_pollutant


# 18. FUNCTION TO EXTRACT INTERACTION LRT RESULTS

extract_lrt <- function(
  lrt_object,
  interaction_label
) {

  tidy(
    lrt_object
  ) %>%

    filter(
      !is.na(p.value)
    ) %>%

    transmute(

      Interaction =
        interaction_label,

      Chi_Square =
        deviance,

      Degrees_of_Freedom =
        df,

      P_Value =
        p.value
    )
}


# 19. COMBINE INTERACTION TEST RESULTS

interaction_results <- bind_rows(

  extract_lrt(
    lrt_cat_state,
    "AQI Category × State"
  ),

  extract_lrt(
    lrt_aqi_state,
    "Annual Weighted AQI × State"
  ),

  extract_lrt(
    lrt_aqi_pollutant,
    "Annual Weighted AQI × Pollutant Category"
  )
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


interaction_results


write_csv(
  interaction_results,
  interaction_file
)


# PART D — VARIABLE SELECTION

# 20. DEFINE FULL AND NULL MODELS

# Candidate predictors:
#
#   Annual_Weighted_AQI
#   State
#   Pollutant_Category

model_full <- glm(
  Asthma_ED_Status ~
    Annual_Weighted_AQI +
    State +
    Pollutant_Category,
  data = data,
  family = binomial(link = "logit")
)


model_null <- glm(
  Asthma_ED_Status ~ 1,
  data = data,
  family = binomial(link = "logit")
)


# 21. FORWARD SELECTION

# Starting with the intercept-only model, predictors are added according to
# AIC.

model_forward <- step(
  model_null,

  scope = list(
    lower = ~ 1,
    upper =
      ~ Annual_Weighted_AQI +
        State +
        Pollutant_Category
  ),

  direction = "forward",

  trace = 0
)


formula(
  model_forward
)


AIC(
  model_forward
)

# 22. BACKWARD ELIMINATION

# Starting with the full model, predictors are removed according to AIC.

model_backward <- step(
  model_full,
  direction = "backward",
  trace = 0
)


formula(
  model_backward
)


AIC(
  model_backward
)

# 23. BIDIRECTIONAL STEPWISE SELECTION

model_stepwise <- step(
  model_full,

  scope = list(
    lower = ~ 1,
    upper =
      ~ Annual_Weighted_AQI +
        State +
        Pollutant_Category
  ),

  direction = "both",

  trace = 0
)


formula(
  model_stepwise
)


AIC(
  model_stepwise
)

# 24. COMPARE VARIABLE-SELECTION RESULTS

variable_selection <- tibble(

  Selection_Method = c(
    "Forward",
    "Backward",
    "Stepwise"
  ),

  Selected_Model = c(

    paste(
      deparse(
        formula(model_forward)
      ),
      collapse = ""
    ),

    paste(
      deparse(
        formula(model_backward)
      ),
      collapse = ""
    ),

    paste(
      deparse(
        formula(model_stepwise)
      ),
      collapse = ""
    )
  ),

  AIC = c(

    AIC(
      model_forward
    ),

    AIC(
      model_backward
    ),

    AIC(
      model_stepwise
    )
  )
) %>%

  mutate(
    AIC = round(
      AIC,
      2
    )
  )


variable_selection


write_csv(
  variable_selection,
  variable_selection_file
)


# 25. IDENTIFY LOWEST-AIC SELECTED MODEL

selected_models <- list(

  Forward =
    model_forward,

  Backward =
    model_backward,

  Stepwise =
    model_stepwise
)


selection_aics <- sapply(
  selected_models,
  AIC
)


best_selection_method <- names(
  which.min(
    selection_aics
  )
)


best_selected_model <-
  selected_models[
    [best_selection_method]
  ]


message(
  paste(
    "Lowest-AIC variable-selection model:",
    best_selection_method
  )
)


message(
  paste(
    "Selected formula:",
    paste(
      deparse(
        formula(
          best_selected_model
        )
      ),
      collapse = ""
    )
  )
)


# 26. EXTRACT SELECTED MODEL COEFFICIENTS

selected_model_results <- tidy(
  best_selected_model,
  conf.int = TRUE,
  exponentiate = TRUE
) %>%

  transmute(

    Term =
      term,

    Odds_Ratio =
      estimate,

    Std_Error =
      std.error,

    Z_Value =
      statistic,

    P_Value =
      p.value,

    OR_CI_Lower =
      conf.low,

    OR_CI_Upper =
      conf.high
  ) %>%

  mutate(

    across(
      c(
        Odds_Ratio,
        Std_Error,
        Z_Value,
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


selected_model_results


write_csv(
  selected_model_results,
  selected_model_file
)


# 27. COMPARE SELECTED MODEL WITH PRIMARY BASE MODEL

selected_vs_base <- tibble(

  Model = c(
    "Primary Base Model",
    paste(
      "Selected Model -",
      best_selection_method
    )
  ),

  Formula = c(

    paste(
      deparse(
        formula(model_base)
      ),
      collapse = ""
    ),

    paste(
      deparse(
        formula(best_selected_model)
      ),
      collapse = ""
    )
  ),

  AIC = c(
    AIC(model_base),
    AIC(best_selected_model)
  )
) %>%

  mutate(
    AIC = round(
      AIC,
      2
    )
  )


selected_vs_base


# 28. FINAL MODEL-SELECTION SUMMARY

message(
  "Model selection and interaction testing complete."
)


message(
  paste(
    "AQI likelihood-ratio test saved to:",
    aqi_lrt_file
  )
)


message(
  paste(
    "Candidate model comparison saved to:",
    model_comparison_file
  )
)


message(
  paste(
    "Interaction tests saved to:",
    interaction_file
  )
)


message(
  paste(
    "Variable-selection results saved to:",
    variable_selection_file
  )
)


message(
  paste(
    "Selected-model coefficients saved to:",
    selected_model_file
  )
)

