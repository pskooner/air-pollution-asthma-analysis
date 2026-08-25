# 11_predictions_visualization.R

# Air Pollution and Asthma Burden in the United States

# Purpose:
# Generate adjusted predicted probabilities of elevated asthma emergency
# department (ED) burden across observed AQI levels and summarize state-level
# effects from the sensitivity-analysis logistic regression model.

# Model:
#   Asthma_ED_Status ~ Annual_Weighted_AQI + State

# The model is fitted after excluding observations identified as influential
# using Cook's distance > 4/n, consistent with the sensitivity analysis.

# Analyses:
#   1. Reproduce the sensitivity-analysis model
#   2. Generate an AQI sequence across the observed exposure range
#   3. Calculate predicted probabilities for representative states
#   4. Visualize adjusted asthma ED risk across AQI levels
#   5. Extract state-level adjusted effects
#   6. Rank state effects relative to the model reference state

# Input:
#   data/processed/Combined_AQI_Asthma_2023_Cleaned.csv

# Outputs:
#   results/tables/predicted_probabilities_by_state.csv
#   results/tables/state_effects_adjusted_aqi.csv

#   figures/predicted_asthma_risk_by_state.png

# Author:
#   Parminder S. Kooner

# 1. LOAD REQUIRED PACKAGES

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)


# 2. DEFINE FILE PATHS

input_file <-
  "data/processed/Combined_AQI_Asthma_2023_Cleaned.csv"


predictions_file <-
  "results/tables/predicted_probabilities_by_state.csv"

state_effects_file <-
  "results/tables/state_effects_adjusted_aqi.csv"

prediction_figure_file <-
  "figures/predicted_asthma_risk_by_state.png"


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
    )
  )


# 5. REPRODUCE ORIGINAL FINAL MODEL

model_final <- glm(
  Asthma_ED_Status ~
    Annual_Weighted_AQI +
    State,
  data = data,
  family = binomial(link = "logit")
)


# 6. IDENTIFY INFLUENTIAL OBSERVATIONS

cooks_distance <- cooks.distance(
  model_final
)


cook_threshold <-
  4 / nrow(data)


influential_indices <- which(
  cooks_distance >
    cook_threshold
)


# 7. CREATE SENSITIVITY DATASET

data_sensitivity <- data[
  -influential_indices,
]


# Preserve the same State factor levels as the original analytical dataset.

data_sensitivity$State <- factor(
  data_sensitivity$State,
  levels = levels(data$State)
)


# 8. FIT SENSITIVITY MODEL

model_sensitivity <- glm(
  Asthma_ED_Status ~
    Annual_Weighted_AQI +
    State,
  data = data_sensitivity,
  family = binomial(link = "logit")
)


summary(
  model_sensitivity
)


# PART A — ADJUSTED PREDICTED PROBABILITIES

# 9. SELECT REPRESENTATIVE STATES

# These states were used in the original project visualization to illustrate
# differences in adjusted asthma ED risk across geographic settings.

states_to_plot <- c(
  "Wisconsin",
  "Iowa",
  "New York",
  "Arizona"
)


# Confirm that all selected states are available in the analytical data.

missing_states <- setdiff(
  states_to_plot,
  levels(data_sensitivity$State)
)


if (
  length(missing_states) > 0
) {

  warning(
    paste(
      "The following states are not available in the data:",
      paste(
        missing_states,
        collapse = ", "
      )
    )
  )
}


states_to_plot <- intersect(
  states_to_plot,
  levels(data_sensitivity$State)
)


# 10. CREATE AQI PREDICTION SEQUENCE

# Generate 100 equally spaced AQI values spanning the observed range in the
# sensitivity-analysis dataset.

aqi_sequence <- seq(

  from = min(
    data_sensitivity$Annual_Weighted_AQI,
    na.rm = TRUE
  ),

  to = max(
    data_sensitivity$Annual_Weighted_AQI,
    na.rm = TRUE
  ),

  length.out = 100
)


# 11. CREATE PREDICTION DATASET

prediction_data <- expand_grid(

  Annual_Weighted_AQI =
    aqi_sequence,

  State =
    states_to_plot
)


# Match the State factor levels used in the fitted model.

prediction_data <- prediction_data %>%

  mutate(

    State = factor(
      State,
      levels = levels(
        data_sensitivity$State
      )
    )
  )


# 12. CALCULATE ADJUSTED PREDICTED PROBABILITIES

prediction_data <- prediction_data %>%

  mutate(

    Predicted_Probability = predict(

      model_sensitivity,

      newdata = prediction_data,

      type = "response"
    )
  )


prediction_data


# 13. EXPORT PREDICTED PROBABILITIES

prediction_output <- prediction_data %>%

  mutate(

    State =
      as.character(State),

    Annual_Weighted_AQI = round(
      Annual_Weighted_AQI,
      3
    ),

    Predicted_Probability = round(
      Predicted_Probability,
      6
    )
  )


write_csv(
  prediction_output,
  predictions_file
)


# 14. FIGURE 12 — PREDICTED ASTHMA ED RISK ACROSS AQI LEVELS

prediction_plot <- ggplot(

  prediction_data,

  aes(

    x =
      Annual_Weighted_AQI,

    y =
      Predicted_Probability,

    group =
      State,

    linetype =
      State
  )
) +

  geom_line(
    linewidth = 1.1
  ) +

  labs(

    title =
      "Adjusted Predicted Probability of Elevated Asthma ED Burden",

    subtitle =
      "Predicted risk across annual weighted AQI levels for selected states",

    x =
      "Annual Weighted AQI",

    y =
      "Predicted Probability of Elevated Asthma ED Status",

    linetype =
      "State"
  ) +

  scale_y_continuous(

    limits = c(
      0,
      1
    ),

    labels =
      scales::percent_format(
        accuracy = 1
      )
  ) +

  theme_minimal() +

  theme(

    plot.title =
      element_text(
        face = "bold",
        hjust = 0.5
      ),

    plot.subtitle =
      element_text(
        hjust = 0.5
      ),

    legend.position =
      "right",

    panel.grid.minor =
      element_blank()
  )


prediction_plot


ggsave(

  filename =
    prediction_figure_file,

  plot =
    prediction_plot,

  width = 9,

  height = 6,

  dpi = 300
)


# PART B — STATE-LEVEL ADJUSTED EFFECTS

# 15. EXTRACT STATE COEFFICIENTS

state_effects <- tidy(
  model_sensitivity,
  conf.int = TRUE,
  exponentiate = FALSE
) %>%

  filter(
    grepl(
      "^State",
      term
    )
  ) %>%

  mutate(

    State =
      sub(
        "^State",
        "",
        term
      )
  )


# 16. CALCULATE STATE ODDS RATIOS

state_or <- tidy(
  model_sensitivity,
  conf.int = TRUE,
  exponentiate = TRUE
) %>%

  filter(
    grepl(
      "^State",
      term
    )
  ) %>%

  select(
    term,
    estimate,
    conf.low,
    conf.high
  ) %>%

  rename(

    Odds_Ratio =
      estimate,

    OR_CI_Lower =
      conf.low,

    OR_CI_Upper =
      conf.high
  )


# 17. COMBINE STATE COEFFICIENTS AND ODDS RATIOS

state_effects_results <- state_effects %>%

  left_join(
    state_or,
    by = "term"
  ) %>%

  transmute(

    State =
      State,

    Log_Odds =
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
      Odds_Ratio,

    OR_CI_Lower =
      OR_CI_Lower,

    OR_CI_Upper =
      OR_CI_Upper
  ) %>%

  arrange(
    Log_Odds
  ) %>%

  mutate(

    Rank =
      row_number(),

    across(
      c(
        Log_Odds,
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
  ) %>%

  select(
    Rank,
    everything()
  )


state_effects_results


# 18. IDENTIFY REFERENCE STATE

# R uses the first State factor level as the reference category.

reference_state <- levels(
  data_sensitivity$State
)[1]


reference_state


message(
  paste(
    "Reference state for adjusted state effects:",
    reference_state
  )
)


# 19. EXPORT STATE EFFECTS

write_csv(
  state_effects_results,
  state_effects_file
)


# 20. DISPLAY LOWEST AND HIGHEST ADJUSTED STATE EFFECTS

lowest_state_effect <- state_effects_results %>%

  slice_min(
    order_by = Log_Odds,
    n = 1,
    with_ties = FALSE
  )


highest_state_effect <- state_effects_results %>%

  slice_max(
    order_by = Log_Odds,
    n = 1,
    with_ties = FALSE
  )


lowest_state_effect

highest_state_effect


# 21. SUMMARY OF PREDICTED PROBABILITIES BY STATE

prediction_summary <- prediction_data %>%

  group_by(
    State
  ) %>%

  summarise(

    Minimum_Predicted_Risk =
      min(
        Predicted_Probability,
        na.rm = TRUE
      ),

    Maximum_Predicted_Risk =
      max(
        Predicted_Probability,
        na.rm = TRUE
      ),

    Mean_Predicted_Risk =
      mean(
        Predicted_Probability,
        na.rm = TRUE
      ),

    .groups =
      "drop"
  ) %>%

  mutate(

    across(
      where(is.numeric),
      ~ round(.x, 4)
    )
  )


prediction_summary


# 22. FINAL SUMMARY

message(
  "Prediction and visualization analysis complete."
)


message(
  paste(
    "Prediction model based on",
    nrow(data_sensitivity),
    "observations after sensitivity exclusions."
  )
)


message(
  paste(
    "Predicted probabilities saved to:",
    predictions_file
  )
)


message(
  paste(
    "State effects saved to:",
    state_effects_file
  )
)


message(
  paste(
    "Predicted-risk figure saved to:",
    prediction_figure_file
  )
)

