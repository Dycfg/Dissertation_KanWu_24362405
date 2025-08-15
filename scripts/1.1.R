# R version: 4.3.1
# Required libraries
library(readxl)
library(dplyr)
library(ggplot2)

# Load and merge datasets
data_v1 <- read_excel("/Users/dycfg/Downloads/Final/Data_LouisBolk_soil-moisture-study_v1_Oct2024.xlsx", sheet = "Data")
data_v2 <- read_excel("/Users/dycfg/Downloads/Final/Data_LouisBolk_soil-moisture-study_v2_Nov2024.xlsx", sheet = "Data_Herbage")

# Filter and merge CUM values from 2019–2023
data_v1_cum <- data_v1 %>% filter(Cut == "CUM", Year >= 2019, Year <= 2023)
data_v2_cum <- data_v2 %>% filter(Cut == "CUM", Year >= 2019, Year <= 2023)
merged_data <- merge(data_v1_cum, data_v2_cum[, c("Block", "Row", "Sward_type", "Management", "Year", "Cut", "DMY")],
                     by = c("Block", "Row", "Sward_type", "Management", "Year", "Cut"),
                     all.x = TRUE, suffixes = c("_v1", "_v2"))
merged_data$DMY <- ifelse(is.na(merged_data$DMY_v1), merged_data$DMY_v2, merged_data$DMY_v1)

# Clean and plot
df_model <- merged_data %>%
  select(Block, Row, Sward_type, Management, Year, DMY) %>%
  filter(!is.na(DMY)) %>%
  mutate(across(c(Block, Row, Sward_type, Management, Year), as.factor))

ggplot(df_model, aes(x = Management, y = DMY, fill = Sward_type)) +
  geom_boxplot() +
  labs(title = "Dry Matter Yield (DMY) by Management and Sward Type (2019–2023)",
       x = "Management Regime", y = "DMY (kg/ha)", fill = "Sward Type") +
  theme_minimal(base_size = 14)

