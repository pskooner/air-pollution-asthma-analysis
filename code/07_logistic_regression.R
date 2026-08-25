# 07_logistic_regression.R
# Comparing crude (Unadjusted) OR vs adjusted OR
# Reset variable completely
data$AQI_Category <- as.character(data$AQI_Category)

# Recreate as factor with correct order
data$AQI_Category <- factor(data$AQI_Category, 
                            levels = c("Low", "Moderate", "High"))
# Crude or Unadjusted model
model_crude <- glm(Asthma_ED_Status ~ AQI_Category,
                   family = binomial, data = data)

# Odds ratios
exp(coef(model_crude))

# Confidence intervals
exp(confint(model_crude))

# Adjusted model
model_adj <- glm(Asthma_ED_Status ~ AQI_Category + State,
                 family = binomial, data = data)

# Odds ratios
exp(coef(model_adj))

# Confidence intervals
exp(confint(model_adj))

# Output
crude_results <- tidy(model_crude, exponentiate = TRUE, conf.int = TRUE)
adj_results   <- tidy(model_adj, exponentiate = TRUE, conf.int = TRUE)
crude_results
adj_results

table13 <- dplyr::bind_rows(
  crude_results %>%
    dplyr::filter(term != "(Intercept)") %>%
    dplyr::mutate(
      Term = dplyr::recode(term,
                           "AQI_CategoryModerate" = "Moderate vs Low",
                           "AQI_CategoryHigh"     = "High vs Low"),
      Model = "Crude",
      `OR (95% CI)` = paste0(
        round(estimate, 2), " (",
        round(conf.low, 2), ", ",
        round(conf.high, 2), ")"
      ),
      `p-value` = as.character(ifelse(p.value < 0.001, "<0.001", round(p.value, 3)))
    ) %>%
    dplyr::select(Model, Term, `OR (95% CI)`, `p-value`),
  
  adj_results %>%
    dplyr::filter(term != "(Intercept)") %>%
    dplyr::filter(grepl("AQI_Category", term)) %>%
    dplyr::mutate(
      Term = dplyr::recode(term,
                           "AQI_CategoryModerate" = "Moderate vs Low",
                           "AQI_CategoryHigh"     = "High vs Low"),
      Model = "Adjusted",
      `OR (95% CI)` = paste0(
        round(estimate, 2), " (",
        round(conf.low, 2), ", ",
        round(conf.high, 2), ")"
      ),
      `p-value` = as.character(ifelse(p.value < 0.001, "<0.001", round(p.value, 3)))
    ) %>%
    dplyr::select(Model, Term, `OR (95% CI)`, `p-value`)
) %>%
  dplyr::arrange(Term, Model) %>%
  gt(groupname_col = "Term") %>%
  tab_header(
    title = md("**Table 13. Odds Ratios for Asthma ED Status by AQI Category (Crude and Adjusted for State)**")
  ) %>%
  cols_align(align = "center")

table13

# STEP 6. SIMPLE LOGISTIC REGRESSION
# Continuous AQI
model1 <- glm(Asthma_ED_Status ~ Annual_Weighted_AQI,
              data = data, family = binomial)
summary(model1)
exp(coef(model1))
confint(model1)

# Tidy model (log-odds)
model_log <- broom::tidy(model1, conf.int = TRUE, exponentiate = FALSE)

# Tidy model (odds ratios)
model_or <- broom::tidy(model1, conf.int = TRUE, exponentiate = TRUE)

# Combine both
table_model1 <- model_log %>%
  dplyr::rename(
    Coefficient = estimate,
    `Std. Error` = std.error,
    `z value` = statistic,
    `p-value` = p.value,
    `2.5% (log-odds)` = conf.low,
    `97.5% (log-odds)` = conf.high
  ) %>%
  dplyr::mutate(
    OR = model_or$estimate,
    `2.5% (OR)` = model_or$conf.low,
    `97.5% (OR)` = model_or$conf.high
  ) %>%
  dplyr::mutate(
    across(where(is.numeric), ~ round(., 4)),
    `p-value` = ifelse(`p-value` < 0.001, "<0.001", round(`p-value`, 3))
  ) %>%
  dplyr::rename(Term = term) %>%
  dplyr::select(
    Term, Coefficient, `Std. Error`, `z value`, `p-value`,
    OR, `2.5% (OR)`, `97.5% (OR)`
  ) %>%
  gt() %>%
  tab_header(
    title = md("**Table 14. Logistic Regression Results: Continuous AQI Model**")
  ) %>%
  cols_align(align = "center")

table_model1

# Categorical AQI
model2 <- glm(Asthma_ED_Status ~ AQI_Category,
              data = data, family = binomial)

summary(model2)
exp(coef(model2))
confint(model2)

# Tidy model (log-odds)
model2_log <- broom::tidy(model2, conf.int = TRUE, exponentiate = FALSE)

# Tidy model (odds ratios)
model2_or <- broom::tidy(model2, conf.int = TRUE, exponentiate = TRUE)

# Combine into one table
table_model2 <- model2_log %>%
  dplyr::rename(
    Coefficient = estimate,
    `Std. Error` = std.error,
    `z value` = statistic,
    `p-value` = p.value,
    `2.5% (log-odds)` = conf.low,
    `97.5% (log-odds)` = conf.high
  ) %>%
  dplyr::mutate(
    OR = model2_or$estimate,
    `2.5% (OR)` = model2_or$conf.low,
    `97.5% (OR)` = model2_or$conf.high
  ) %>%
  dplyr::mutate(
    across(where(is.numeric), ~ round(., 4)),
    `p-value` = ifelse(`p-value` < 0.001, "<0.001", round(`p-value`, 3))
  ) %>%
  dplyr::rename(Term = term) %>%
  gt() %>%
  tab_header(
    title = md("**Table 15. Logistic Regression Results: AQI Category (Unadjusted Model)**")
  ) %>%
  cols_align(align = "center")

table_model2

# Inference - Likelihood Ratio Test
model_reduced <- glm(Asthma_ED_Status ~ 1, 
                     family = binomial, data = data)

model_full <- glm(Asthma_ED_Status ~ AQI_Category, 
                  family = binomial, data = data)

# Run LRT
aqi_model_comparison <- anova(model_reduced, model_full, test = "Chisq")

# Tidy + label
table_lrt <- broom::tidy(aqi_model_comparison) %>%
  dplyr::filter(!is.na(p.value)) %>%
  dplyr::mutate(
    Comparison = "AQI Category vs Null Model",
    `Chi-Square` = round(deviance, 2),
    `df` = df,
    `p-value` = ifelse(p.value < 0.001, "<0.001", round(p.value, 3))
  ) %>%
  dplyr::select(Comparison, `Chi-Square`, df, `p-value`) %>%
  gt() %>%
  tab_header(
    title = md("**Table 16. Likelihood Ratio Test: Contribution of AQI Category**")
  ) %>%
  cols_align(align = "center")

table_lrt

# STEP 7. MULTIPLE LOGISTIC REGRESSION
# Main/Base Model
model_base <- glm(Asthma_ED_Status ~ Annual_Weighted_AQI + State,
                  data = data, family = binomial)
summary(model_base)

# Expanded Model
model4 <- glm(Asthma_ED_Status ~ Annual_Weighted_AQI + State + Pollutant_Category,
              data = data, family = binomial)
summary(model4)

# Tidy BOTH models (log-odds scale)
base_log <- broom::tidy(model_base, conf.int = TRUE, exponentiate = FALSE)
exp_log  <- broom::tidy(model4,     conf.int = TRUE, exponentiate = FALSE)

# Tidy BOTH models (OR scale)
base_or <- broom::tidy(model_base, conf.int = TRUE, exponentiate = TRUE)
exp_or  <- broom::tidy(model4,     conf.int = TRUE, exponentiate = TRUE)

# Combine Base model
base_tbl <- base_log %>%
  dplyr::rename(
    Base_Coefficient = estimate,
    Base_SE = std.error,
    Base_z = statistic,
    Base_p = p.value,
    Base_LCI_log = conf.low,
    Base_UCI_log = conf.high
  ) %>%
  dplyr::mutate(
    Base_OR = base_or$estimate,
    Base_LCI_OR = base_or$conf.low,
    Base_UCI_OR = base_or$conf.high
  )

# Combine Expanded model
exp_tbl <- exp_log %>%
  dplyr::rename(
    Expanded_Coefficient = estimate,
    Expanded_SE = std.error,
    Expanded_z = statistic,
    Expanded_p = p.value,
    Expanded_LCI_log = conf.low,
    Expanded_UCI_log = conf.high
  ) %>%
  dplyr::mutate(
    Expanded_OR = exp_or$estimate,
    Expanded_LCI_OR = exp_or$conf.low,
    Expanded_UCI_OR = exp_or$conf.high
  )

# Merge side-by-side
full_table <- base_tbl %>%
  dplyr::rename(Term = term) %>%
  dplyr::full_join(
    exp_tbl %>% dplyr::rename(Term = term),
    by = "Term"
  ) %>%
  dplyr::mutate(
    across(where(is.numeric), ~ round(., 4)),
    Base_p = ifelse(Base_p < 0.001, "<0.001", round(Base_p, 3)),
    Expanded_p = ifelse(Expanded_p < 0.001, "<0.001", round(Expanded_p, 3))
  ) %>%
  gt() %>%
  tab_header(
    title = md("**Table 17. Full Logistic Regression Output: Base vs Expanded Model**")
  ) %>%
  cols_align(align = "center")

full_table

# Alternative Model
model5 <- glm(Asthma_ED_Status ~ AQI_Category + State,
              data = data, family = binomial)
summary(model5)

# Tidy model (log-odds)
model5_log <- broom::tidy(model5, conf.int = TRUE, exponentiate = FALSE)

# Tidy model (odds ratios)
model5_or <- broom::tidy(model5, conf.int = TRUE, exponentiate = TRUE)

# Combine everything
table_model5 <- model5_log %>%
  dplyr::rename(
    Coefficient = estimate,
    `Std. Error` = std.error,
    `z value` = statistic,
    `p-value` = p.value,
    `2.5% (log-odds)` = conf.low,
    `97.5% (log-odds)` = conf.high
  ) %>%
  dplyr::mutate(
    OR = model5_or$estimate,
    `2.5% (OR)` = model5_or$conf.low,
    `97.5% (OR)` = model5_or$conf.high
  ) %>%
  dplyr::mutate(
    across(where(is.numeric), ~ round(., 4)),
    `p-value` = ifelse(`p-value` < 0.001, "<0.001", round(`p-value`, 3))
  ) %>%
  dplyr::rename(Term = term) %>%
  gt() %>%
  tab_header(
    title = md("**Table 17. Logistic Regression Results: AQI Category + State (Alternative Model)**")
  ) %>%
  cols_align(align = "center")

table_model5
