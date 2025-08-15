# Load packages
library(lme4)        # v1.1-34 for fitting LMMs
library(lmerTest)    # v3.1-3 for hypothesis testing
library(tidyverse)   # v2.0.0 for data wrangling
library(ggplot2)      # v3.5.0
library(emmeans)      # v1.8.8

# Read in dataset
model_data <- read.csv("/Users/dycfg/Downloads/Final/merged_grassland_modeldata.csv")

# Convert categorical variables to factors using correct column names
model_data <- model_data %>%
  mutate(across(c(Sward_type, Management, Block, Year), as.factor))

# Fit base model without interaction
model1 <- lmer(DMY ~ Sward_type + Management + Year + (1 | Block), data = model_data)

# Fit model with interaction between sward type and management
model2 <- lmer(DMY ~ Sward_type * Management + Year + (1 | Block), data = model_data)

# Model comparison
anova(model1, model2)

# model2 <- lmer(DMY ~ sward_type * management + year + (1 | block), data = model_data)


emm_year <- emmeans(model2, ~ Year)


emm_year_df <- as.data.frame(emm_year)


ggplot(emm_year_df, aes(x = Year, y = emmean)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.6) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.2) +
  labs(
    title = "Estimated Marginal Means of DMY by Year",
    x = "Year",
    y = "Estimated DMY (t/ha)"
  ) +
  theme_minimal(base_size = 14)

