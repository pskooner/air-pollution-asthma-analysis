# 02_prepare_asthma_data.R
# Load required libraries
library(readr)
library(dplyr)
library(writexl)

# File path
file_path <- "data/raw/Annual_Asthama_ED_Visit_Data_2023.csv"

# Read data
df <- read_csv(file_path)

# Clean and transform data
df_clean <- df %>%
  
  # Remove columns that are entirely NA
  select(where(~ !all(is.na(.)))) %>%
  
  # Drop unnecessary columns
  select(-StateFIPS, -CountyFIPS, -any_of("Data Comment")) %>%
  
  # Rename variable (capitalized)
  rename(Asthma_ED_Rate = Asthma_ED_Rate) %>%
  
  # Create binary outcome
  mutate(
    Asthma_ED_Status = ifelse(
      (Sex == "Male" & Asthma_ED_Rate > 27.1) |
        (Sex == "Female" & Asthma_ED_Rate > 32.3),
      1, 0
    ),
    
    # Convert to factor with proper labels
    Asthma_ED_Status = factor(
      Asthma_ED_Status,
      levels = c(0, 1),
      labels = c("Not Elevated", "Elevated")
    )
  )

# Output file path
output_path <- "data/processed/Annual_Asthama_ED_Visit_Data_2023_Cleaned.xlsx"

# Write to Excel
write_xlsx(df_clean, output_path)

# Confirmation
print("New Excel file created successfully!")
