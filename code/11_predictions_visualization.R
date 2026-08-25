# 11_predictions_visualization.R
# Testing
# 1. Predicted Probability of Asthma ED Status Across AQI Levels (Holding State Constant)

# Select representative states
states_to_plot <- c("Wisconsin", "Iowa", "New York", "Arizona")

# Create AQI sequence
aqi_seq <- seq(min(data_clean$Annual_Weighted_AQI),
               max(data_clean$Annual_Weighted_AQI),
               length.out = 100)

# Create prediction dataset + predictions
new_data_multi <- expand.grid(
  Annual_Weighted_AQI = aqi_seq,
  State = states_to_plot
)

new_data_multi$State <- factor(new_data_multi$State,
                               levels = levels(data_clean$State))

new_data_multi$pred_prob <- predict(
  model_clean,
  newdata = new_data_multi,
  type = "response"
)

# Plot
ggplot(new_data_multi,
       aes(x = Annual_Weighted_AQI,
           y = pred_prob,
           color = State)) +
  geom_line(size = 1.2) +
  scale_color_grey(start = 0.2, end = 0.7) +
  labs(
    title = "Figure 12: Effect of AQI on Asthma ED Risk Across States",
    x = "Annual Weighted AQI",
    y = "Predicted Probability of Elevated Asthma ED Status",
    color = "State"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    text = element_text(color = "black")
  )

# Finding the state with the lowest adjusted asthma burden relative to the reference state
state_effects <- broom::tidy(model_clean) %>%
  filter(grepl("^State", term)) %>%
  mutate(
    State = gsub("State", "", term)
  ) %>%
  arrange(estimate)

state_effects

# Table
table_states <- state_effects %>%
  dplyr::mutate(
    `Log-Odds` = round(estimate, 3),
    `Std. Error` = round(std.error, 3),
    `p-value` = ifelse(p.value < 0.001, "<0.001", round(p.value, 3))
  ) %>%
  dplyr::select(State, `Log-Odds`, `Std. Error`, `p-value`) %>%
  gt() %>%
  tab_header(
    title = md("**Table 26. State-Level Effects on Asthma ED Status (Adjusted for Annual Weighted AQI)**"),
    subtitle = md("*States ranked by log-odds (lowest to highest relative to reference state)*")
  ) %>%
  cols_align(align = "center")

table_states

# OPTIONAL

# Compare continuous vs categorical AQI models (on original data) - Rerunning to check consistency
model_cont <- glm(Asthma_ED_Status ~ Annual_Weighted_AQI + State,
                  data = data, family = binomial)

model_cat <- glm(Asthma_ED_Status ~ AQI_Category + State,
                 data = data, family = binomial)

summary(model_cont)
summary(model_cat)
