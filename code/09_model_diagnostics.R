# 09_model_diagnostics.R
# STEP 11. MULTI COLLINEARITY
# Final model
model_final <- glm(Asthma_ED_Status ~ Annual_Weighted_AQI + State,
                   family = binomial, data = data)
vif(model_final)
vif(model4)

# Plotting Final model VIF
library(tibble)
vif_final_df <- as.data.frame(vif(model_final)) %>%
  rownames_to_column("Variable") %>%
  rename(`VIF (Final Model)` = 2)

# Expanded model VIF
vif_expanded_df <- as.data.frame(vif(model4)) %>%
  rownames_to_column("Variable") %>%
  rename(`VIF (Expanded Model)` = 2)

# Merge
vif_table <- full_join(vif_final_df, vif_expanded_df, by = "Variable") %>%
  mutate(across(where(is.numeric), ~ round(., 2))) %>%
  gt() %>%
  tab_header(
    title = md("**Table 21. Variance Inflation Factors (VIF) for Final and Expanded Models**")
  ) %>%
  cols_align(align = "center")

vif_table

# STEP 12: MODEL DIAGNOSTICS
# Residuals
pearson_res <- residuals(model_final, type = "pearson")
deviance_res <- residuals(model_final, type = "deviance")

# Plot Pearson residuals
plot(pearson_res,
     ylab = "Pearson Residuals",
     xlab = "Observation Index",
     main = "Figure 5: Pearson Residuals vs Observation Index",
     pch = 19,
     col = "grey30")

# Reference line at 0
abline(h = 0, col = "black", lwd = 2)

# Smoothing line
lines(lowess(pearson_res), col = "grey60", lwd = 2)

# Optional: outlier thresholds
abline(h = c(-2, 2), col = "grey50", lty = 2)

# Grid
grid(col = "grey85")


# Plot Deviance Residuals
plot(deviance_res,
     ylab = "Deviance Residuals",
     xlab = "Observation Index",
     main = "Figure 6: Deviance Residuals vs Observation Index",
     pch = 19,
     col = "grey30")

# Reference line at 0
abline(h = 0, col = "black", lwd = 2)

# Smoothing line
lines(lowess(deviance_res), col = "grey60", lwd = 2)

# Outlier thresholds
abline(h = c(-2, 2), col = "grey50", lty = 2)

# Grid
grid(col = "grey85")


# Residuals vs fitted values
plot(fitted(model_final),
     residuals(model_final, type = "deviance"),
     xlab = "Fitted Values",
     ylab = "Deviance Residuals",
     main = "Figure 7: Residuals vs Fitted Values",
     pch = 19,
     col = "grey30")

# Reference line at 0
abline(h = 0, col = "black", lwd = 2)

# Smoothing line
lines(lowess(fitted(model_final),
             residuals(model_final, type = "deviance")),
      col = "grey60", lwd = 2)

# Outlier thresholds
abline(h = c(-2, 2), col = "grey50", lty = 2)

# Grid
grid(col = "grey85")

# Influential observations
# Cook's distance
cook <- cooks.distance(model_final)

plot(cook,
     type = "h",
     lwd = 2,
     col = "grey40",
     xlab = "Observation Index",
     ylab = "Cook's Distance",
     main = "Figure 8: Cook's Distance")

# Add cutoff line (common rule: 4/n)
cutoff <- 4 / length(cook)
abline(h = cutoff, col = "black", lwd = 2, lty = 2)

# Highlight influential points
points(which(cook > cutoff),
       cook[cook > cutoff],
       col = "black",
       pch = 19)

# Grid
grid(col = "grey85")

# Plot

# Model data
model_data <- model.frame(model_final)

# Cutoff
cutoff <- 4 / length(cook)

# Table
table_influence <- model_data %>%
  dplyr::mutate(
    Observation = as.numeric(rownames(model_data)),
    Cooks_Distance = cook
  ) %>%
  dplyr::filter(Cooks_Distance > cutoff) %>%
  dplyr::arrange(desc(Cooks_Distance)) %>%
  dplyr::mutate(
    Cooks_Distance = round(Cooks_Distance, 4)
  ) %>%
  dplyr::select(
    Observation,
    Cooks_Distance,
    Asthma_ED_Status,
    Annual_Weighted_AQI,
    State
  ) %>%
  gt() %>%
  tab_header(
    title = md("**Table 22. Influential Observations Identified by Cook’s Distance**"),
    subtitle = md("*Observations with Cook’s Distance > 4/n*")
  ) %>%
  cols_align(align = "center")

table_influence

# STEP 13: MODEL FIT
# Deviance
model_final$deviance

# Null deviance (for comparison)
model_final$null.deviance

# Likelihood ratio test statistic
model_final$null.deviance - model_final$deviance

# Pearson chi-square statistic
pearson_chi <- sum(residuals(model_final, type = "pearson")^2)
pearson_chi

# Degrees of freedom
df_resid <- model_final$df.residual

# p-value
pchisq(pearson_chi, df_resid, lower.tail = FALSE)

# Hosmer-Lemeshow test
library(ResourceSelection)
hoslem.test(
  as.numeric(data$Asthma_ED_Status) - 1,
  fitted(model_final),
  g = 10
)

# Extract values (using your existing code results)
deviance_val <- model_final$deviance
null_dev <- model_final$null.deviance
lrt_stat <- null_dev - deviance_val

pearson_chi <- sum(residuals(model_final, type = "pearson")^2)
df_resid <- model_final$df.residual
pearson_p <- pchisq(pearson_chi, df_resid, lower.tail = FALSE)

hl <- hoslem.test(
  as.numeric(data$Asthma_ED_Status) - 1,
  fitted(model_final),
  g = 10
)

# Create table
table_model_fit <- tibble(
  Measure = c(
    "Model Deviance",
    "Null Deviance",
    "Deviance Difference (LRT)",
    "Pearson Chi-Square",
    "Hosmer–Lemeshow Test"
  ),
  Statistic = c(
    round(deviance_val, 2),
    round(null_dev, 2),
    round(lrt_stat, 2),
    round(pearson_chi, 2),
    round(hl$statistic, 2)
  ),
  df = c(
    NA,
    NA,
    NA,
    df_resid,
    hl$parameter
  ),
  `p-value` = c(
    NA,
    NA,
    NA,
    ifelse(pearson_p < 0.001, "<0.001", round(pearson_p, 3)),
    ifelse(hl$p.value < 0.001, "<0.001", round(hl$p.value, 3))
  )
) %>%
  gt() %>%
  tab_header(
    title = md("**Table 23. Model Fit and Goodness-of-Fit Statistics**")
  ) %>%
  cols_align(align = "center")

table_model_fit

# Likelihood Ratio Test - AQI Effect
# Compare with and without AQI
# Model without AQI
model_no_aqi <- glm(Asthma_ED_Status ~ State,
                    family = binomial, data = data)

# Model with AQI (final model)
model_with_aqi <- model_final

# Likelihood ratio test
anova(model_no_aqi, model_with_aqi, test = "Chisq")

# Model comparison: with vs without AQI
aqi_model_comparison <- broom::tidy(
  anova(model_no_aqi, model_with_aqi, test = "Chisq")
)

# Table 23
table24 <- aqi_model_comparison %>%
  dplyr::filter(!is.na(p.value)) %>%
  dplyr::mutate(
    Comparison = "State-only model vs State + Annual Weighted AQI model",
    `Test Statistic (χ²)` = round(deviance, 2),
    df = df,
    `p-value` = ifelse(p.value < 0.001, "<0.001", round(p.value, 3))
  ) %>%
  dplyr::select(Comparison, `Test Statistic (χ²)`, df, `p-value`) %>%
  gt() %>%
  tab_header(
    title = md("**Table 24. Contribution of Annual Weighted AQI to Model Fit (Likelihood Ratio Test)**")
  ) %>%
  cols_align(align = "center")

table24

# STEP 14: ROC/AUC
roc_obj <- roc(data$Asthma_ED_Status, fitted(model_final))
auc_val <- auc(roc_obj)

plot(roc_obj,
     col = "black",
     lwd = 3,
     main = "Figure 9: ROC Curve",
     xlab = "False Positive Rate (1 - Specificity)",
     ylab = "True Positive Rate (Sensitivity)")

# Add AUC label
text(0.6, 0.2,
     labels = paste0("AUC = ", round(auc_val, 3)),
     cex = 1.2)
