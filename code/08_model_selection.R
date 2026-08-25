# 08_model_selection.R
# STEP 8: MODEL COMPARISON (AIC)
# AIC comparison
aic_tbl <-AIC(model_base, model4, model5)
aic_tbl

# Likelihood ratio test
anova(model_base, model4, test = "Chisq")
anova(model_base, model5, test = "Chisq")

final_table <- tibble(
  Model = c("Base (Annual Weighted AQI + State)",
            "Expanded (+ Pollutant)",
            "Alternative (AQI Category + State)"),
  AIC = aic_tbl$AIC,
  Comparison = c("—", "Base vs Expanded", "Base vs Alternative"),
  ChiSq = c("—",
            round(broom::tidy(anova(model_base, model4, test = "Chisq"))$deviance[2], 2),
            round(broom::tidy(anova(model_base, model5, test = "Chisq"))$deviance[2], 2)),
  df = c("—",
         broom::tidy(anova(model_base, model4, test = "Chisq"))$df[2],
         broom::tidy(anova(model_base, model5, test = "Chisq"))$df[2]),
  p = c("—",
        ifelse(broom::tidy(anova(model_base, model4, test = "Chisq"))$p.value[2] < 0.001, "<0.001",
               round(broom::tidy(anova(model_base, model4, test = "Chisq"))$p.value[2], 3)),
        ifelse(broom::tidy(anova(model_base, model5, test = "Chisq"))$p.value[2] < 0.001, "<0.001",
               round(broom::tidy(anova(model_base, model5, test = "Chisq"))$p.value[2], 3)))
) %>%
  gt() %>%
  tab_header(
    title = md("**Table 18. Model Comparison (AIC and Likelihood Ratio Tests)**")
  ) %>%
  cols_align(align = "center")

final_table

# STEP 9: INTERACTION AND TESTING
# Interaction: AQI Category X State

# Without interaction
model_cat_main <- glm(Asthma_ED_Status ~ AQI_Category + State,
                      family = binomial, data = data)

# With interaction
model_cat_int <- glm(Asthma_ED_Status ~ AQI_Category * State,
                     family = binomial, data = data)

# Likelihood Ratio Test
anova(model_cat_main, model_cat_int, test = "Chisq")

# Interaction: Annual Weighted AQI X State
model_aqi_state_int <- glm(Asthma_ED_Status ~ Annual_Weighted_AQI * State,
                           family = binomial, data = data)

# Likelihood Ratio Test
anova(model_base, model_aqi_state_int, test = "Chisq")

# Without interaction
model_poll_main <- glm(Asthma_ED_Status ~ Annual_Weighted_AQI + Pollutant_Category + State,
                       family = binomial, data = data)

# With interaction
model_poll_int <- glm(Asthma_ED_Status ~ Annual_Weighted_AQI * Pollutant_Category + State,
                      family = binomial, data = data)

# LRT
anova(model_poll_main, model_poll_int, test = "Chisq")

# Function to extract LRT results
extract_lrt <- function(model_main, model_int, label) {
  broom::tidy(anova(model_main, model_int, test = "Chisq")) %>%
    dplyr::filter(!is.na(p.value)) %>%
    dplyr::mutate(
      Interaction = label,
      `Chi-Square` = round(deviance, 2),
      df = df,
      `p-value` = ifelse(p.value < 0.001, "<0.001", round(p.value, 3))
    ) %>%
    dplyr::select(Interaction, `Chi-Square`, df, `p-value`)
}

# Combine all interaction tests
table19 <- bind_rows(
  extract_lrt(model_cat_main, model_cat_int, "AQI Category × State"),
  extract_lrt(model_base, model_aqi_state_int, "Annual Weighted AQI × State"),
  extract_lrt(model_poll_main, model_poll_int, "Annual Weighted AQI × Pollutant Category")
) %>%
  gt() %>%
  tab_header(
    title = md("**Table 19. Likelihood Ratio Tests for Interaction Effects**")
  ) %>%
  cols_align(align = "center")

table19

# STEP 10. VARIABLE SELECTION
# Full model
model_full <- glm(Asthma_ED_Status ~ Annual_Weighted_AQI + State + Pollutant_Category,
                  family = binomial, data = data)

# Null model
model_null <- glm(Asthma_ED_Status ~ 1,
                  family = binomial, data = data)

# Forward Selection
model_forward <- step(model_null,
                      scope = ~ Annual_Weighted_AQI + State + Pollutant_Category,
                      direction = "forward")

# Backward Selection
model_backward <- step(model_full,
                       direction = "backward")

# Stepwise Selection
model_stepwise <- step(model_full,
                       direction = "both")
# Viewing Results
AIC(model_forward, model_backward, model_stepwise)


# Extract AIC values
aic_vals <- AIC(model_forward, model_backward, model_stepwise)

# Create table
table20 <- tibble(
  Method = c("Forward Selection", "Backward Selection", "Stepwise Selection"),
  Model_Selected = c(
    deparse(formula(model_forward)),
    deparse(formula(model_backward)),
    deparse(formula(model_stepwise))
  ),
  AIC = round(aic_vals$AIC, 2)
) %>%
  gt() %>%
  tab_header(
    title = md("**Table 20. Variable Selection Results Using Stepwise Procedures**")
  ) %>%
  cols_label(
    Method = "Selection Method",
    Model_Selected = "Final Model",
    AIC = "AIC"
  ) %>%
  cols_align(align = "center")

table20
