# Load necessary libraries
library(tidyverse)
library(readxl)

# Step 1: Read the Excel file and select relevant variables
soil_data <- read_excel("/Users/dycfg/Downloads/Final/Data_LouisBolk_soil-moisture-study_v2_Nov2024.xlsx", sheet = "Data_Soil")

# Step 2: Select key physicochemical variables
soil_vars <- c("total_N", "P_PAE", "K", "Mg", "SOM", "org_C", "pH")

# Step 3: Pivot to long format for faceting
soil_long <- soil_data %>%
  select(all_of(soil_vars)) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

# Step 4: Create the boxplots using facet_wrap
ggplot(soil_long, aes(x = Variable, y = Value)) +
  geom_boxplot(fill = "skyblue", alpha = 0.6, outlier.size = 1) +
  facet_wrap(~ Variable, scales = "free", nrow = 1) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.text = element_text(size = 11),
    panel.grid.major.x = element_blank()
  ) +
  labs(title = "Boxplots of Key Soil Physicochemical Properties", y = NULL)
