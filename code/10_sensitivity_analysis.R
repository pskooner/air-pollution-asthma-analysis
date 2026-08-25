# 10_sensitivity_analysis.R
# STEP 15: SENSITIVITY ANALYSIS

# Cook's distance from final model
cook <- cooks.distance(model_final)
plot(cook,
     main = "Cook's Distance",
     ylab = "Cook's Distance",
     xlab = "Observation Index",
     pch = 16)

abline(h = 4/nrow(data), col = "red", lwd = 2)

#Highlight influential points
plot(cook,
     main = "Figure 10: Cook's Distance (Influential Points Highlighted)",
     ylab = "Cook's Distance",
     xlab = "Observation Index",
     pch = 16)

threshold <- 4/nrow(data)

points(which(cook > threshold),
       cook[cook > threshold],
       col = "red", pch = 16)

abline(h = threshold, col = "blue", lwd = 2)

# Remove influential observations
data_clean <- data[-which(cook > (4/nrow(data))), ]

# Refit model on cleaned data
model_clean <- glm(Asthma_ED_Status ~ Annual_Weighted_AQI + State,
                   data = data_clean, family = binomial)
summary(model_clean)

# Comparing final models with and without influential points
# Tidy outputs
final_tidy <- broom::tidy(model_final)
clean_tidy <- broom::tidy(model_clean)

# Extract AQI row
aqi_final <- final_tidy %>%
  dplyr::filter(term == "Annual_Weighted_AQI") %>%
  dplyr::mutate(
    Model = "Final Model",
    Estimate = round(estimate, 4),
    `Std. Error` = round(std.error, 4),
    p_value = p.value
  )

aqi_clean <- clean_tidy %>%
  dplyr::filter(term == "Annual_Weighted_AQI") %>%
  dplyr::mutate(
    Model = "Cleaned Model",
    Estimate = round(estimate, 4),
    `Std. Error` = round(std.error, 4),
    p_value = p.value
  )

# Combine safely
aqi_compare <- dplyr::bind_rows(aqi_final, aqi_clean) %>%
  dplyr::select(Model, Estimate, `Std. Error`, p_value)

# Model fit
model_fit <- tibble(
  Model = c("Final Model", "Cleaned Model"),
  AIC = c(round(AIC(model_final), 2), round(AIC(model_clean), 2)),
  Deviance = c(round(model_final$deviance, 2), round(model_clean$deviance, 2))
)

# Merge
table25 <- aqi_compare %>%
  dplyr::left_join(model_fit, by = "Model") %>%
  gt() %>%
  tab_header(
    title = md("**Table 25. Comparison of Final Model and Sensitivity Analysis Model (After Removing Influential Observations)**")
  ) %>%
  cols_align(align = "center") %>%
  fmt_number(
    columns = c(Estimate, `Std. Error`, AIC, Deviance),
    decimals = 2
  ) %>%
  fmt(
    columns = p_value,
    fns = function(x) ifelse(x < 0.001, "<0.001", sprintf("%.3f", x))
  )

table25

# Compare coefficients
coef_original <- coef(model_final)
coef_clean <- coef(model_clean)

# Scatterplot
plot(coef_original, coef_clean,
     xlab = "Original Coefficients",
     ylab = "Clean Coefficients",
     main = "Coefficient Comparison",
     pch = 19)

abline(0, 1, col = "red", lwd = 2)

# ROC Curve Model fitted on clean data - ROC CURVE 2
# ROC for cleaned model
roc_clean <- roc(data_clean$Asthma_ED_Status, fitted(model_clean))

# AUC
auc_clean <- auc(roc_clean)

# Plot
plot(roc_clean,
     col = "black",
     lwd = 3,
     main = "Figure 11. ROC Curve for Cleaned Model (After Removing Influential Observations)",
     xlab = "False Positive Rate (1 - Specificity)",
     ylab = "True Positive Rate (Sensitivity)")

# Add AUC label
text(0.6, 0.2,
     labels = paste0("AUC = ", round(auc_clean, 3)),
     cex = 1.2)
