# Load libraries
library(tidyverse)
library(readxl)

# 1. Read plant community data
sward_data <- read_excel("/Users/dycfg/Downloads/Final/Data_LouisBolk_soil-moisture-study_v2_Nov2024.xlsx", sheet = "Data_Sward")

# 2. Select key plant diversity variables
sward_vars <- c("Blm_srt_aant", "Blmhfd_aant_tot", 
                "Gew_Hoornbloem", "Rode_klaver", 
                "Grote_weegbree", "Knoopkruid", "Pinksterbloem")

# 3. Convert to long format for faceted plotting
sward_long <- sward_data %>%
  select(all_of(sward_vars)) %>%
  pivot_longer(cols = everything(), names_to = "Species", values_to = "Abundance")

# 4. Drawing: Box plots show the distribution of each indicator.
ggplot(sward_long, aes(x = Species, y = Abundance)) +
  geom_boxplot(fill = "palegreen3", alpha = 0.6, outlier.size = 1) +
  facet_wrap(~ Species, scales = "free", nrow = 1) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.text = element_text(size = 11),
    panel.grid.major.x = element_blank()
  ) +
  labs(title = "Boxplots of Plant Biodiversity Indicators", y = NULL)
