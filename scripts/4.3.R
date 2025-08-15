# Load packages
library(lme4)        # v1.1-34 for fitting LMMs
library(lmerTest)    # v3.1-3 for hypothesis testing
library(tidyverse)   # v2.0.0 for data wrangling

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

# Extract random intercepts
block_effects <- ranef(model2)$Block
block_effects_df <- data.frame(
  Block = rownames(block_effects),
  Intercept = block_effects[, 1]
)

# Order by value
block_effects_df <- block_effects_df[order(block_effects_df$Intercept), ]

# Plot
ggplot(block_effects_df, aes(x = reorder(Block, Intercept), y = Intercept)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Estimated Random Intercepts for Block (BLUPs)",
    x = "Block",
    y = "Intercept Deviation (kg/ha)"
  ) +
  theme_minimal(base_size = 14)