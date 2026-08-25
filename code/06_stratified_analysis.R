# 06_stratified_analysis.R
# STEP 5: STRATIFIED ANALYSIS (BY Sex)
# Create 3D contingency table (AQI x Asthma x Sex)
aqi_asthma_sex_table <- xtabs(~ AQI_Category + Asthma_ED_Status + Sex, data = data)
aqi_asthma_sex_table

# Table
df_3d <- as.data.frame(aqi_asthma_sex_table)

table10 <- df_3d %>%
  pivot_wider(names_from = Asthma_ED_Status, values_from = Freq) %>%
  arrange(Sex, AQI_Category) %>%
  gt(groupname_col = "Sex") %>%
  tab_header(
    title = md("**Table 10. AQI Category and Asthma ED Status, Stratified by Sex**")
  )

table10

# Row Percentages
# Males
prop.table(
  table(data$AQI_Category[data$Sex == "Male"],
        data$Asthma_ED_Status[data$Sex == "Male"]), 1)
# Females
prop.table(
  table(data$AQI_Category[data$Sex == "Female"],
        data$Asthma_ED_Status[data$Sex == "Female"]), 1)

# Cochran–Mantel–Haenszel (CMH) test: stratifying variable - sex
cmh_res <- mantelhaen.test(aqi_asthma_sex_table)

# Table
table11 <- broom::tidy(cmh_res) %>%
  dplyr::select(statistic, parameter, p.value) %>%
  dplyr::mutate(
    statistic = round(statistic, 2),
    p.value = ifelse(p.value < 0.001, "<0.001", round(p.value, 3))
  ) %>%
  dplyr::rename(
    `CMH Chi-Square` = statistic,
    `Degrees of Freedom` = parameter,
    `p-value` = p.value
  ) %>%
  gt() %>%
  tab_header(
    title = md("**Table 11. Cochran–Mantel–Haenszel Test of Association Between AQI Category and Asthma ED Status, Adjusted for Sex**")
  ) %>%
  cols_align(align = "center")

table11

#2. STRATIFIED ANALYSIS (BY State)
# Create 3D table: AQI x Asthma x State
aqi_asthma_state_table <- xtabs(~ AQI_Category + Asthma_ED_Status + State, data = data)
aqi_asthma_state_table
# CMH test (adjusting for State)
cmh_state <- mantelhaen.test(aqi_asthma_state_table)

table14 <- broom::tidy(cmh_state) %>%
  dplyr::select(statistic, parameter, p.value) %>%
  dplyr::mutate(
    statistic = round(statistic, 2),
    p.value = ifelse(p.value < 0.001, "<0.001", round(p.value, 3))
  ) %>%
  dplyr::rename(
    `CMH Chi-Square` = statistic,
    `Degrees of Freedom` = parameter,
    `p-value` = p.value
  ) %>%
  gt() %>%
  tab_header(
    title = md("**Table 12. Cochran–Mantel–Haenszel Test Adjusted for State**")
  ) %>%
  cols_align(align = "center")

table14
