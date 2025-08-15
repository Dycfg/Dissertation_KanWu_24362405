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

# Create interaction plot
ggplot(emm_df, aes(x = Sward_type, y = emmean, fill = Management)) +
  geom_bar(stat = "identity", position = position_dodge(0.8)) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE),
                position = position_dodge(0.8), width = 0.2) +
  labs(
    title = "Interaction between Sward Type and Management on DMY",
    x = "Sward Type",
    y = "Estimated DMY (kg/ha)"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()
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

library(car)  # v3.1-2

# Fit a linear model with the same fixed effects
lm_model <- lm(DMY ~ Sward_type * Management + Year, data = model_data)

# Calculate VIF
vif_values <- vif(lm_model)
print(vif_values)
# ------------------------------------------------------------------------------------------------------------------------

# ------------------------------------------------------------------------------------------------------------------------
# Required libraries
library(lme4)       # v1.1-34
library(ggplot2)    # v3.5.0

# Extract random effects for block and convert to data frame
block_effects <- ranef(model2)$Block  # 注意 "Block" 首字母大写
block_effects_df <- data.frame(
  Block = rownames(block_effects),
  Intercept = as.numeric(block_effects[, 1])
)

# Reorder by intercept
block_effects_df <- block_effects_df %>%
  arrange(Intercept)

# Plot
ggplot(block_effects_df, aes(x = reorder(Block, Intercept), y = Intercept)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Estimated Random Intercepts for Block (BLUPs)",
    x = "Block",
    y = "Intercept Deviation (kg/ha)"
  ) +
  theme_minimal(base_size = 14)
# ---------------------------------------------------------------------------------------------------------
# Required packages
library(lme4)       # v1.1-34
library(ggplot2)    # v3.5.0
library(gridExtra)  # v2.3

# Residual plots
residuals <- resid(model2)
fitted <- fitted(model2)

p1 <- ggplot(data.frame(Fitted = fitted, Residuals = residuals),
             aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals") +
  theme_minimal()

p2 <- ggplot(data.frame(residuals), aes(sample = residuals)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Normal Q-Q Plot", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()

# Combine plots
gridExtra::grid.arrange(p1, p2, ncol = 2)
# ---------------------------------------------------------------------------------------------------------
# Required package
library(MuMIn)   # v1.47.5

# R-squared
r2 <- r.squaredGLMM(model2)
r2
# ---------------------------------------------------------------------------------------------------------
# Required packages
library(lme4)       # v1.1-34
library(r2glmm)     # v0.1.2
library(forcats)
library(ggplot2)
library(dplyr)

# Ensure your model is an lmer model with factor variables
model_data$Sward_type <- as.factor(model_data$Sward_type)
model_data$Management <- as.factor(model_data$Management)
model_data$Year <- as.factor(model_data$Year)
model_data$Block <- as.factor(model_data$Block)

# Fit model again (in case needed)
model2 <- lmer(DMY ~ Sward_type * Management + Year + (1 | Block), data = model_data)

# Run partial R²
partial_r2 <- r2beta(model2, method = "sgv", partial = TRUE)
print(partial_r2)

# Check the result
if (nrow(partial_r2) > 0) {
  # Clean names and plot
  partial_r2_df <- partial_r2 %>%
    as.data.frame() %>%
    mutate(Effect = fct_reorder(as.factor(Effect), Rsq))
  
  ggplot(partial_r2_df, aes(x = Rsq, y = Effect)) +
    geom_col(fill = "steelblue") +
    labs(
      title = "Variance Explained by Fixed Effects (Partial R²)",
      x = "Partial R²",
      y = "Fixed Effect Term"
    ) +
    theme_minimal(base_size = 14)
} else {
  print("No valid fixed effect terms found for partial R².")
}
# 检查模型对象
class(model2)

# 查看模型结构
summary(model2)

# 查看 r2beta 返回内容
str(partial_r2)

# 检查数据类型
str(model_data)
# ---------------------------------------------------------------------------------------------------------
