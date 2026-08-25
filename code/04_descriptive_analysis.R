# 04_descriptive_analysis.R
# STEP 0: INSTALLATING PACKAGES AND LOADING LIBRARIES
# install.packages(c("tidyverse","epitools","car","ResourceSelection","pROC","DescTools","MASS","brant","gtsummary","gt"))

# Load libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(gtsummary)
library(gt)
library(vcd)
library(tidyr)
library(scales)
library(DescTools)
library(broom)
library(MASS)
library(ResourceSelection)
library(pROC)
library(car)

# Set working directory
setwd("data/processed")

# STEP 1: LOADING DATA
# Read dataset
data <- read_excel("Combined_AQI_Asthma_2023_Cleaned.xlsx")

# Structure and summary
str(data)
summary(data)

# Create summary table
# Load libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(gtsummary)
library(gt)
library(vcd)
library(tidyr)
library(scales)
library(DescTools)
library(broom)
library(MASS)
library(ResourceSelection)
library(pROC)
library(car)

# Set working directory
setwd("data/processed")

# STEP 1: LOADING DATA
# Read dataset
data <- read_excel("Combined_AQI_Asthma_2023_Cleaned.xlsx")

# Structure and summary
str(data)
summary(data)

# Create summary table
table1 <- data %>%
  summarise(
    `Number of Observations (County-Level)` = n(),
    `Number of States` = n_distinct(State),
    `Number of Counties` = n_distinct(State, County),
    `Sex Categories` = n_distinct(Sex),
    `Year` = unique(Year)
  )

# Create publication-style table using gt
table1_gt <- table1 %>%
  gt() %>%
  tab_header(
    title = md("**Table 1. Summary of the Merged County-Level Dataset (United States, 2023)**")
  ) %>%
  cols_align(align = "center") %>%
  tab_options(
    table.font.size = 14,
    heading.title.font.size = 16
  )

# Display table
table1_gt

# Create publication-style table using gt
table1_gt <- table1 %>%
  gt() %>%
  tab_header(
    title = md("**Table 1. Summary of the Merged County-Level Dataset (United States, 2023)**")
  ) %>%
  cols_align(align = "center") %>%
  tab_options(
    table.font.size = 14,
    heading.title.font.size = 16
  )

# Display table
table1_gt

# STEP 2: DATA CLEANING AND CODING
# Check original values
unique(data$Asthma_ED_Status)

# Convert variables
data <- data %>%
  mutate(
    Asthma_ED_Status = ifelse(Asthma_ED_Status == "Elevated", 1, 0),
    AQI_Category = factor(AQI_Category, levels = c("Low","Moderate","High"), ordered = TRUE),
    State = factor(State),
    Pollutant_Category = factor(Pollutant_Category)
  )

# Checking for missing data
colSums(is.na(data))

# SANITY CHECK
# Check outcome distribution
table(data$Asthma_ED_Status)
summary(data$Asthma_ED_Status)

# Check AQI category distribution
table(data$AQI_Category)
levels(data$AQI_Category)

# Pollutant (nominal exposure)
table(data$Pollutant_Category)
levels(data$Pollutant_Category)

# Do not collapse PM 10 
# Rule 1: Collapse, if Count is <5
# In this case, 16 > 5, so we will proceed as is

# STEP 3: DESCRIPTIVE STATISTICS
# Outcome Variable
# Report % elevated asthma and Confidence interval
prop.table(table(data$Asthma_ED_Status))

# One sample proportion test
prop.test(sum(data$Asthma_ED_Status == 1), nrow(data))

# Exposure Variables
table(data$AQI_Category)
prop.table(table(data$AQI_Category))

# Pollutant Category
table(data$Pollutant_Category)
prop.table(table(data$Pollutant_Category))

# Additional Variable (Sex)
table(data$Sex)
prop.table(table(data$Sex))

# Continuous variables
summary(data$Annual_Weighted_AQI)
summary(data$prop_unhealthy)

# Creating a table for categorical variables
data <- data %>%
  mutate(
    AQI_Category = factor(AQI_Category,
                          levels = c("Low", "Moderate", "High"),
                          ordered = TRUE),
    
    Pollutant_Category = factor(Pollutant_Category,
                                levels = c("Ozone", "PM2.5", "PM10")),
    
    Sex = factor(Sex),
    
    Asthma_ED_Status = factor(Asthma_ED_Status,
                              levels = c(0,1),
                              labels = c("Not Elevated", "Elevated"))
  )

# Creating TABLE 2
table2 <- data %>%
  dplyr::select(AQI_Category, Pollutant_Category, Sex, Asthma_ED_Status) %>%
  tbl_summary(
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    label = list(
      AQI_Category ~ "AQI Category",
      Pollutant_Category ~ "Pollutant Category",
      Sex ~ "Sex",
      Asthma_ED_Status ~ "Asthma ED Status"
    )
  ) %>%
  modify_header(label = "**Characteristic**") %>%
  bold_labels() %>%
  modify_caption(paste0("**Table 2. Descriptive Summary of Categorical Variables (N = ", nrow(data), ")**"))

# View table
table2

# DOT PLOT
# Convert to character BEFORE pivot
long_data <- data %>%
  dplyr::select(AQI_Category, Pollutant_Category, Sex, Asthma_ED_Status) %>%
  dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%  
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = "Variable",
    values_to = "Category"
  )

# Clean labels
long_data <- long_data %>%
  dplyr::mutate(
    Variable = dplyr::recode(Variable,
                             AQI_Category = "AQI Category",
                             Pollutant_Category = "Pollutant Category",
                             Sex = "Sex",
                             Asthma_ED_Status = "Asthma ED Status"
    ),
    Category = dplyr::recode(Category,
                             `0` = "Not Elevated",
                             `1` = "Elevated"
    )
  )

# Counts + percentages
plot_data <- long_data %>%
  dplyr::count(Variable, Category) %>%
  dplyr::group_by(Variable) %>%
  dplyr::mutate(percent = n / sum(n)) %>%
  dplyr::ungroup()

# Reapply ordering
plot_data <- plot_data %>%
  mutate(
    Category = case_when(
      Variable == "AQI Category" ~ factor(Category, levels = c("Low", "Moderate", "High")),
      Variable == "Pollutant Category" ~ factor(Category, levels = c("Ozone", "PM2.5", "PM10")),
      Variable == "Asthma ED Status" ~ factor(Category, levels = c("Not Elevated", "Elevated")),
      TRUE ~ factor(Category)
    )
  )

# Plot
ggplot(plot_data, aes(x = Category, y = percent)) +
  geom_point(size = 3) +
  geom_text(aes(label = percent(percent, accuracy = 0.1)),
            hjust = -0.2, size = 3) +
  coord_flip() +
  facet_wrap(~ Variable, scales = "free_y") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Figure 1: Distribution of Categorical Variables",
    x = "",
    y = "Percentage"
  ) +
  theme_classic() +
  theme(
    strip.text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5)
  )

# TABLE 3: Continuous Variables
table3 <- data %>%
  dplyr::select(prop_unhealthy, Asthma_ED_Rate, Annual_Weighted_AQI) %>%
  gtsummary::tbl_summary(
    type = all_continuous() ~ "continuous2",
    statistic = all_continuous() ~ c(
      "{mean} ({sd})",
      "{median}",
      "{min} – {max}"
    ),
    digits = all_continuous() ~ 2,
    label = list(
      prop_unhealthy ~ "Proportion Unhealthy Days",
      Asthma_ED_Rate ~ "Asthma ED Visit Rate",
      Annual_Weighted_AQI ~ "Annual Weighted AQI"
    )
  ) %>%
  gtsummary::modify_header(label = "**Characteristic**") %>%
  gtsummary::bold_labels() %>%
  gtsummary::modify_caption(
    paste0("**Table 3. Summary of Continuous Variables (N = ", nrow(data), ")**")
  )

# Viewing table
table3

# Box Plots for Continuous Variables
# Reshaping the data
long_cont <- data %>%
  dplyr::select(Annual_Weighted_AQI, Asthma_ED_Rate, prop_unhealthy) %>%
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  dplyr::mutate(
    Variable = dplyr::recode(Variable,
                             Annual_Weighted_AQI = "Annual Weighted AQI",
                             Asthma_ED_Rate = "Asthma ED Visit Rate",
                             prop_unhealthy = "Proportion Unhealthy Days"
    )
  )

# Plot
ggplot(long_cont, aes(x = "", y = Value)) +
  geom_boxplot(fill = "gray85", color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3) +
  facet_wrap(~ Variable, scales = "free_y") +
  labs(
    title = "Figure 2. Box Plots of Continuous Variables",
    x = "",
    y = ""
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.border = element_rect(color = "black", fill = NA)
  )
