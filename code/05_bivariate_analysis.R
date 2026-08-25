# 05_bivariate_analysis.R
# STEP 4: CONTINGENCY TABLE OR BIVARIATE ANALYSIS
#  1 Contingency table - AQI Category x Asthma ED Status
table_aqi_asthma <- table(data$AQI_Category, data$Asthma_ED_Status)
table_aqi_asthma
# Plot
table4 <- data %>%
  dplyr::select(AQI_Category, Asthma_ED_Status) %>%
  tbl_cross(
    row = AQI_Category,
    col = Asthma_ED_Status,
    percent = "row"
  ) %>%
  modify_header(label = "**AQI Category**") %>%
  bold_labels() %>%
  modify_caption("**Table 4. Association Between AQI Category and Asthma ED Status**")
# Viewing Table
table4

# Chi-square test
chi_res <- chisq.test(table_aqi_asthma)

# Storing results
chi_table <- broom::tidy(chi_res) %>%
  dplyr::select(statistic, parameter, p.value) %>%
  dplyr::mutate(
    statistic = round(statistic, 2),
    p.value = ifelse(p.value < 0.001, "<0.001", round(p.value, 3))
  ) %>%
  dplyr::rename(
    `Chi-Square Statistic` = statistic,
    `Degrees of Freedom` = parameter,
    `p-value` = p.value
  )

# Create publication-style table
table_chi <- chi_table %>%
  gt() %>%
  tab_header(
    title = md("**Table 5. Chi-Square Test of Association Between AQI Category and Asthma ED Status**")
  ) %>%
  cols_align(align = "center") %>%
  tab_options(
    table.font.size = 14,
    heading.title.font.size = 16
  )
# Viewing table
table_chi

# Likelihood ratio test (G²)
g_res <-GTest(table_aqi_asthma)
# Store results
g_table <- broom::tidy(g_res) %>%
  dplyr::select(statistic, parameter, p.value) %>%
  dplyr::mutate(
    statistic = round(statistic, 2),
    p.value = ifelse(p.value < 0.001, "<0.001", round(p.value, 3))
  ) %>%
  dplyr::rename(
    `G Statistic` = statistic,
    `Degrees of Freedom` = parameter,
    `p-value` = p.value
  )

# Create table
table6 <- g_table %>%
  gt() %>%
  tab_header(
    title = md("**Table 6. Likelihood Ratio Test (G-Test) of Association Between AQI Category and Asthma ED Status**")
  ) %>%
  cols_align(align = "center")

table6

# Trend test (ordinal)
trend_res <-CochranArmitageTest(table_aqi_asthma)
# Store results
trend_table <- broom::tidy(trend_res) %>%
  dplyr::select(statistic, p.value) %>%
  dplyr::mutate(
    statistic = round(statistic, 2),
    p.value = ifelse(p.value < 0.001, "<0.001", round(p.value, 3))
  ) %>%
  dplyr::rename(
    `Z Statistic` = statistic,
    `p-value` = p.value
  )

# Create table
table7 <- trend_table %>%
  gt() %>%
  tab_header(
    title = md("**Table 7. Cochran–Armitage Trend Test Across AQI Categories and Asthma ED Status**")
  ) %>%
  cols_align(align = "center")
# View table
table7

# For Report
# Combine all test results into one table
combined_tests <- dplyr::bind_rows(
  chi_table %>%
    dplyr::rename(Statistic = `Chi-Square Statistic`) %>%
    dplyr::mutate(Test = "Chi-square") %>%
    dplyr::select(Test, Statistic, `Degrees of Freedom`, `p-value`),
  
  g_table %>%
    dplyr::rename(Statistic = `G Statistic`) %>%
    dplyr::mutate(Test = "Likelihood Ratio (G²)") %>%
    dplyr::select(Test, Statistic, `Degrees of Freedom`, `p-value`),
  
  trend_table %>%
    dplyr::rename(Statistic = `Z Statistic`) %>%
    dplyr::mutate(Test = "Cochran–Armitage Trend",
                  `Degrees of Freedom` = NA) %>%
    dplyr::select(Test, Statistic, `Degrees of Freedom`, `p-value`)
)

# Create combined publication table
table_combined <- combined_tests %>%
  gt() %>%
  tab_header(
    title = md("**Tests of Association Between AQI Category and Asthma ED Status**")
  ) %>%
  cols_align(align = "center") %>%
  tab_options(
    table.font.size = 14,
    heading.title.font.size = 16
  )

# View combined table
table_combined


levels(data$Asthma_ED_Status)
# Reference or Baseline - Not Elevated
# Event (Outcome modeleed) - Elevated

# Odds ratios
oddsratio(table_aqi_asthma)
# Row percentages
prop.table(table_aqi_asthma, 1)

# Adjusted residuals
chisq.test(table_aqi_asthma)$residuals

# Relative Risk
# Compute risks (probability of Elevated)
risk_low <- table_aqi_asthma["Low", "Elevated"] / sum(table_aqi_asthma["Low", ])
risk_mod <- table_aqi_asthma["Moderate", "Elevated"] / sum(table_aqi_asthma["Moderate", ])
risk_high <- table_aqi_asthma["High", "Elevated"] / sum(table_aqi_asthma["High", ])

# Relative Risks
RR_low_vs_high <- risk_low / risk_high
RR_low_vs_mod  <- risk_low / risk_mod
RR_mod_vs_high <- risk_mod / risk_high

# Print
RR_low_vs_high
RR_low_vs_mod
RR_mod_vs_high

# Visual Representation
mosaic(~ AQI_Category + Asthma_ED_Status, data = data,
       shade = TRUE, legend = TRUE,
       main = "Figure 3. Mosaic Plot of AQI Category and Asthma ED Status",
       xlab = "AQI Category",
       ylab = "Asthma ED Status")

#  2 Contingency table - Pollutant Category x Asthma ED Status
table_pollutant_asthma <- table(data$Pollutant_Category, data$Asthma_ED_Status)
table_pollutant_asthma
table8 <- data %>%
  dplyr::select(Pollutant_Category, Asthma_ED_Status) %>%
  tbl_cross(
    row = Pollutant_Category,
    col = Asthma_ED_Status,
    percent = "row"
  ) %>%
  modify_header(label = "**Pollutant Category**") %>%
  bold_labels() %>%
  modify_caption("**Table 8. Association Between Pollutant Category and Asthma ED Status**")

table8

# Chi-square test
chi_res_pollutant <- chisq.test(table_pollutant_asthma)

# Tidy and format
table9 <- broom::tidy(chi_res_pollutant) %>%
  dplyr::select(statistic, parameter, p.value) %>%
  dplyr::mutate(
    statistic = round(statistic, 2),
    p.value = ifelse(p.value < 0.001, "<0.001", round(p.value, 3))
  ) %>%
  dplyr::rename(
    `Chi-Square Statistic` = statistic,
    `Degrees of Freedom` = parameter,
    `p-value` = p.value
  ) %>%
  gt() %>%
  tab_header(
    title = md("**Table 9. Chi-Square Test of Association Between Pollutant Category and Asthma ED Status**")
  ) %>%
  cols_align(align = "center")

table9

# Likelihood ratio test (G²)
GTest(table_pollutant_asthma)

# Trend test (ordinal)
CochranArmitageTest(table_pollutant_asthma)

levels(data$Asthma_ED_Status)
# Reference or Baseline - Not Elevated
# Event (Outcome modeleed) - Elevated

# Odds ratios
oddsratio(table_pollutant_asthma)
# Row percentages
prop.table(table_pollutant_asthma, 1)

# Adjusted residuals
chisq.test(table_pollutant_asthma)$residuals

# Mosaic Plot
mosaic(~ Pollutant_Category + Asthma_ED_Status, data = data,
       shade = TRUE, legend = TRUE,
       main = "Figure 4. Mosaic Plot of Pollutant Category and Asthma ED Status",
       xlab = "Pollutant Category",
       ylab = "Asthma ED Status")
