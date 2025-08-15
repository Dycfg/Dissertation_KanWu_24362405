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

# Load required visualization packages
library(ggplot2)
library(emmeans)

# Estimated marginal means
emm <- emmeans(model2, ~ Sward_type * Management)

# Convert to data frame
emm_df <- as.data.frame(emm)


# Required packages
library(emmeans)    # v1.8.6
library(ggplot2)    # v3.4.4
library(tidyverse)  # v2.0.0

# Compute estimated marginal means for sward type and management
emm_main <- emmeans(model2, ~ Sward_type + Management)

# Convert to dataframe for plotting
emm_df <- as.data.frame(emm_main)

# Plot with confidence intervals
ggplot(emm_df, aes(x = Sward_type, y = emmean, fill = Management)) +
  geom_bar(stat = "identity", position = position_dodge(0.9), color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE),
                position = position_dodge(0.9), width = 0.2) +
  labs(title = "Figure 4.1: Estimated Marginal Means of DMY by Sward Type and Management",
       x = "Sward Type", y = "Estimated DMY (t/ha)", fill = "Management Regime") +
  theme_minimal(base_size = 13)