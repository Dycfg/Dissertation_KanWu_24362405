library(ggplot2)
library(reshape2)
library(readr)


worms_data <- read_excel("/Users/dycfg/Downloads/Final/Data_LouisBolk_soil-moisture-study_v2_Nov2024.xlsx", sheet = "Data_Worms")
# Select key indicators
worm_vars <- c("EW_n_tot", "EW_n_adult", "EW_n_juv",
               "EW_T_epigeic", "EW_T_endogeic", "EW_T_anecic",
               "EW_ND", "EW_prop_juv", "EW_ind_biomass", "EW_biomass")

# Check if all fields exist
stopifnot(all(worm_vars %in% colnames(worms_data)))

# Organize data into long format
worms_long <- melt(worms_data[, worm_vars])

p_worms <- ggplot(worms_long, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot(alpha = 0.6, outlier.shape = 1) +
  labs(
    title = "Boxplots of Earthworm Biodiversity Indicators",
    x = "Soil Fauna Indicators",
    y = "Observed Value"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold")
  )


ggsave("earthworm_biodiversity_boxplot.png", p_worms, width = 14, height = 6, dpi = 300, bg = "white")
