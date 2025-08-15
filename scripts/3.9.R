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

# ------------------------------------------------------------------------------------------------------------------------
# Libraries
library(lme4)         # v1.1-34
library(lmerTest)     # v3.1-3
library(ggplot2)      # v3.5.1
library(patchwork)    # v1.1.3

# Residuals and fitted values
model2_res <- resid(model2)
model2_fit <- fitted(model2)

# Plot Residuals vs Fitted and Q-Q plot
p1 <- ggplot(data = data.frame(fit = model2_fit, res = model2_res), aes(x = fit, y = res)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fitted Values", y = "Residuals", title = "Residuals vs Fitted")

p2 <- ggplot(data = data.frame(stdres = scale(model2_res)), aes(sample = stdres)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Normal Q-Q Plot")

# Combine plots and save
(p1 | p2)
ggsave("Figure_3_8_residual_diagnostics.png", width = 10, height = 5)

# ------------------------------------------------------------------------------------------------------------------------
