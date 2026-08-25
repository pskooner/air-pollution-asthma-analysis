# 04_descriptive_analysis.R

# Air Pollution and Asthma Burden in the United States

# Purpose:
# Generate descriptive statistics and exploratory visualizations for the
# combined county-level AQI and asthma emergency department analytical dataset.

# Input:
#   data/processed/Combined_AQI_Asthma_2023_Cleaned.csv

# Outputs:
#   results/tables/dataset_summary.csv
#   results/tables/categorical_summary.csv
#   results/tables/continuous_summary.csv

#   figures/categorical_variable_distributions.png
#   figures/continuous_variable_boxplots.png

# Author:
#   Parminder S. Kooner

# 1. LOAD REQUIRED PACKAGES

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)


# 2. DEFINE FILE PATHS

input_file <-
  "data/processed/Combined_AQI_Asthma_2023_Cleaned.csv"

dataset_summary_file <-
  "results/tables/dataset_summary.csv"

categorical_summary_file <-
  "results/tables/categorical_summary.csv"

continuous_summary_file <-
  "results/tables/continuous_summary.csv"

figure1_file <-
  "figures/categorical_variable_distributions.png"

figure2_file <-
  "figures/continuous_variable_boxplots.png"

# 3. IMPORT ANALYTICAL DATASET

data <- read_csv(
  input_file,
  show_col_types = FALSE
)


# Inspect structure
glimpse(data)

# 4. FORMAT ANALYTICAL VARIABLES

# Re-establish factor ordering after importing the CSV file.

data <- data %>%
  mutate(

    AQI_Category = factor(
      AQI_Category,
      levels = c(
        "Low",
        "Moderate",
        "High"
      ),
      ordered = TRUE
    ),

    Pollutant_Category = factor(
      Pollutant_Category
    ),

    Sex = factor(
      Sex,
      levels = c(
        "Female",
        "Male"
      )
    ),

    Asthma_ED_Status = factor(
      Asthma_ED_Status,
      levels = c(
        "Not Elevated",
        "Elevated"
      )
    )
  )

# 5. DATASET SUMMARY

# Summarize the overall dimensions and geographic structure of the analytical
# dataset.

dataset_summary <- data %>%
  summarise(

    Number_of_Observations = n(),

    Number_of_States = n_distinct(State),

    Number_of_Counties = n_distinct(
      State,
      County
    ),

    Sex_Categories = n_distinct(Sex),

    Year = paste(
      sort(unique(Year)),
      collapse = ", "
    )
  )


# Display
dataset_summary


# Export
write_csv(
  dataset_summary,
  dataset_summary_file
)

# 6. DESCRIPTIVE STATISTICS FOR CATEGORICAL VARIABLES

# Variables summarized:
#
#   - AQI Category
#   - Pollutant Category
#   - Sex
#   - Asthma ED Status


categorical_summary <- bind_rows(

  data %>%
    count(
      AQI_Category,
      name = "Count"
    ) %>%
    mutate(
      Variable = "AQI Category",
      Category = as.character(AQI_Category),
      Percent = 100 * Count / sum(Count)
    ) %>%
    select(
      Variable,
      Category,
      Count,
      Percent
    ),

  data %>%
    count(
      Pollutant_Category,
      name = "Count"
    ) %>%
    mutate(
      Variable = "Pollutant Category",
      Category = as.character(Pollutant_Category),
      Percent = 100 * Count / sum(Count)
    ) %>%
    select(
      Variable,
      Category,
      Count,
      Percent
    ),

  data %>%
    count(
      Sex,
      name = "Count"
    ) %>%
    mutate(
      Variable = "Sex",
      Category = as.character(Sex),
      Percent = 100 * Count / sum(Count)
    ) %>%
    select(
      Variable,
      Category,
      Count,
      Percent
    ),

  data %>%
    count(
      Asthma_ED_Status,
      name = "Count"
    ) %>%
    mutate(
      Variable = "Asthma ED Status",
      Category = as.character(Asthma_ED_Status),
      Percent = 100 * Count / sum(Count)
    ) %>%
    select(
      Variable,
      Category,
      Count,
      Percent
    )
) %>%

  mutate(
    Percent = round(
      Percent,
      1
    )
  )


# Display
categorical_summary


# Export
write_csv(
  categorical_summary,
  categorical_summary_file
)


# 7. FIGURE 1 — DISTRIBUTION OF CATEGORICAL VARIABLES

# Reshape categorical variables into long format.

categorical_long <- data %>%
  select(
    AQI_Category,
    Pollutant_Category,
    Sex,
    Asthma_ED_Status
  ) %>%

  mutate(
    across(
      everything(),
      as.character
    )
  ) %>%

  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Category"
  ) %>%

  mutate(

    Variable = recode(
      Variable,

      AQI_Category =
        "AQI Category",

      Pollutant_Category =
        "Pollutant Category",

      Sex =
        "Sex",

      Asthma_ED_Status =
        "Asthma ED Status"
    )
  )


# Calculate counts and percentages.

categorical_plot_data <- categorical_long %>%

  count(
    Variable,
    Category
  ) %>%

  group_by(
    Variable
  ) %>%

  mutate(
    Percent = n / sum(n)
  ) %>%

  ungroup()


# Establish meaningful category ordering.

categorical_plot_data <- categorical_plot_data %>%

  mutate(

    Category = case_when(

      Variable == "AQI Category" ~
        factor(
          Category,
          levels = c(
            "Low",
            "Moderate",
            "High"
          )
        ),

      Variable == "Pollutant Category" ~
        factor(
          Category,
          levels = c(
            "Ozone",
            "PM2.5",
            "PM10",
            "CO",
            "NO2",
            "Mixed"
          )
        ),

      Variable == "Asthma ED Status" ~
        factor(
          Category,
          levels = c(
            "Not Elevated",
            "Elevated"
          )
        ),

      Variable == "Sex" ~
        factor(
          Category,
          levels = c(
            "Female",
            "Male"
          )
        ),

      TRUE ~
        factor(Category)
    )
  )


# Generate Figure 1.

figure1 <- ggplot(
  categorical_plot_data,
  aes(
    x = Category,
    y = Percent
  )
) +

  geom_point(
    size = 3
  ) +

  geom_text(
    aes(
      label = percent(
        Percent,
        accuracy = 0.1
      )
    ),
    hjust = -0.2,
    size = 3
  ) +

  coord_flip() +

  facet_wrap(
    ~ Variable,
    scales = "free_y"
  ) +

  scale_y_continuous(
    labels = percent_format(),
    expand = expansion(
      mult = c(
        0.02,
        0.18
      )
    )
  ) +

  labs(
    title =
      "Distribution of Categorical Variables",
    x = NULL,
    y = "Percentage"
  ) +

  theme_classic() +

  theme(

    strip.text =
      element_text(
        face = "bold"
      ),

    plot.title =
      element_text(
        hjust = 0.5,
        face = "bold"
      )
  )


# Display
figure1


# Save
ggsave(
  filename = figure1_file,
  plot = figure1,
  width = 10,
  height = 7,
  dpi = 300
)

# 8. DESCRIPTIVE STATISTICS FOR CONTINUOUS VARIABLES

# Continuous variables:
#
#   - Proportion of Unhealthy Days
#   - Asthma ED Visit Rate
#   - Annual Weighted AQI


continuous_summary <- tibble(

  Variable = c(
    "Proportion Unhealthy Days",
    "Asthma ED Visit Rate",
    "Annual Weighted AQI"
  ),

  Mean = c(

    mean(
      data$prop_unhealthy,
      na.rm = TRUE
    ),

    mean(
      data$Asthma_ED_Rate,
      na.rm = TRUE
    ),

    mean(
      data$Annual_Weighted_AQI,
      na.rm = TRUE
    )
  ),

  SD = c(

    sd(
      data$prop_unhealthy,
      na.rm = TRUE
    ),

    sd(
      data$Asthma_ED_Rate,
      na.rm = TRUE
    ),

    sd(
      data$Annual_Weighted_AQI,
      na.rm = TRUE
    )
  ),

  Median = c(

    median(
      data$prop_unhealthy,
      na.rm = TRUE
    ),

    median(
      data$Asthma_ED_Rate,
      na.rm = TRUE
    ),

    median(
      data$Annual_Weighted_AQI,
      na.rm = TRUE
    )
  ),

  Minimum = c(

    min(
      data$prop_unhealthy,
      na.rm = TRUE
    ),

    min(
      data$Asthma_ED_Rate,
      na.rm = TRUE
    ),

    min(
      data$Annual_Weighted_AQI,
      na.rm = TRUE
    )
  ),

  Maximum = c(

    max(
      data$prop_unhealthy,
      na.rm = TRUE
    ),

    max(
      data$Asthma_ED_Rate,
      na.rm = TRUE
    ),

    max(
      data$Annual_Weighted_AQI,
      na.rm = TRUE
    )
  )
) %>%

  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 3)
    )
  )


# Display
continuous_summary


# Export
write_csv(
  continuous_summary,
  continuous_summary_file
)

# 9. FIGURE 2 — BOX PLOTS OF CONTINUOUS VARIABLES

continuous_long <- data %>%

  select(
    Annual_Weighted_AQI,
    Asthma_ED_Rate,
    prop_unhealthy
  ) %>%

  pivot_longer(

    cols = everything(),

    names_to = "Variable",

    values_to = "Value"
  ) %>%

  mutate(

    Variable = recode(

      Variable,

      Annual_Weighted_AQI =
        "Annual Weighted AQI",

      Asthma_ED_Rate =
        "Asthma ED Visit Rate",

      prop_unhealthy =
        "Proportion Unhealthy Days"
    )
  )


# Generate Figure 2.

figure2 <- ggplot(
  continuous_long,
  aes(
    x = "",
    y = Value
  )
) +

  geom_boxplot(
    fill = "gray85",
    color = "black"
  ) +

  stat_summary(
    fun = mean,
    geom = "point",
    shape = 18,
    size = 3
  ) +

  facet_wrap(
    ~ Variable,
    scales = "free_y"
  ) +

  labs(
    title =
      "Box Plots of Continuous Variables",
    x = NULL,
    y = NULL
  ) +

  theme_classic() +

  theme(

    plot.title =
      element_text(
        hjust = 0.5,
        face = "bold"
      ),

    strip.text =
      element_text(
        face = "bold"
      ),

    axis.text.x =
      element_blank(),

    axis.ticks.x =
      element_blank(),

    panel.border =
      element_rect(
        color = "black",
        fill = NA
      )
  )


# Display
figure2


# Save
ggsave(
  filename = figure2_file,
  plot = figure2,
  width = 10,
  height = 7,
  dpi = 300
)

# 10. FINAL CHECK

message(
  "Descriptive analysis complete."
)

message(
  paste(
    "Dataset summary saved to:",
    dataset_summary_file
  )
)

message(
  paste(
    "Categorical summary saved to:",
    categorical_summary_file
  )
)

message(
  paste(
    "Continuous summary saved to:",
    continuous_summary_file
  )
)

message(
  paste(
    "Figure 1 saved to:",
    figure1_file
  )
)

message(
  paste(
    "Figure 2 saved to:",
    figure2_file
  )
)
