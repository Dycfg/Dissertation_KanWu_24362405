library(ggplot2)
library(dplyr)
library(readr)

# Load cleaned dataset
data <- read_csv("/Users/dycfg/Downloads/Final/merged_grassland_dmy_2019_2023.csv")

# Standardize column names to lowercase
names(data) <- tolower(names(data))

# Ensure categorical encoding
data$sward_type <- factor(data$sward_type)
data$management <- factor(data$management)

# Plot
ggplot(data, aes(x = sward_type, y = dmy, fill = management)) +
  geom_boxplot(alpha = 0.8, outlier.shape = 21, outlier.size = 1.5) +
  facet_wrap(~year) +
  labs(
    title = "Figure 2.1: Dry Matter Yield (DMY) by Sward Type and Management Regime (2019–2023)",
    x = "Sward Type",
    y = "DMY (kg/ha)",
    fill = "Management"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )