# Required libraries
library(lme4)        # v1.1-34
library(performance) # v0.10.3
library(tidyverse)   # v2.0.0

# Read and prepare data
df <- read.csv("/Users/dycfg/Downloads/Final/merged_grassland_modeldata.csv")
df <- df %>% mutate(across(c(Sward_type, Management, Block, Year), as.factor))

# Identify years
years <- unique(df$year)

# Initialize result list
results <- list()

# Loop through each year
for (yr in years) {
  data_subset <- df %>% filter(year != yr)
  
  tryCatch({
    model <- lmer(DMY ~ Sward_type * Management + Year + (1 | Block), data = data_subset)
    r2_vals <- performance::r2(model)
    coef_vals <- fixef(model)
    
    # Safely extract coefficients
    sward <- if ("sward_typeMixture" %in% names(coef_vals)) coef_vals["sward_typeMixture"] else NA
    mgmt  <- if ("managementIntensive" %in% names(coef_vals)) coef_vals["managementIntensive"] else NA
    inter <- if ("sward_typeMixture:managementIntensive" %in% names(coef_vals)) coef_vals["sward_typeMixture:managementIntensive"] else NA
    
    results[[as.character(yr)]] <- data.frame(
      year_removed = yr,
      Marginal_R2 = r2_vals$R2_marginal,
      Conditional_R2 = r2_vals$R2_conditional,
      sward_type_Mixture = sward,
      management_Intensive = mgmt,
      `sward_type:management` = inter
    )
  }, error = function(e) {
    warning(paste("Model failed for year:", yr))
  })
}

# Combine results
sensitivity_df <- bind_rows(results)
write.csv(sensitivity_df, "/Users/dycfg/Downloads/Final/sensitivity_leave_one_year_out_results.csv", row.names = FALSE)