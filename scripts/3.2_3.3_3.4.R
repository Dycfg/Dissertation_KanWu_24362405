# ================================================
# Exploratory Data Visualization for Grassland Dataset
# Author: 吴侃
# Date: 2025-07
# R version: 4.3+
# Required Packages: tidyverse, pheatmap, RColorBrewer
# ================================================

# ==== Load required libraries ====
library(tidyverse)      # for ggplot2 and data wrangling
library(visdat)   # For better missing data visualization
library(naniar)   # Alternative visualization
library(RColorBrewer)
library(pheatmap)

# ==== Create output directory first ====
if (!dir.exists("EDA_outputs")) dir.create("EDA_outputs")

# ==== Load the dataset ====
data <- read.csv("/Users/dycfg/Downloads/Final/merged_grassland_dmy_2019_2023.csv")

# ============================================================
# 1. Missing Data Heatmap (Figure 3.7)
# ============================================================

# Create logical matrix for NA, convert to numeric (0 = present, 1 = missing)
missing_mat <- is.na(data) * 1
missing_mat <- t(missing_mat)  # transpose for variable-wise display

# Optional: clean column names (or just use row indices)
colnames(missing_mat) <- paste0("Row", 1:ncol(missing_mat))
rownames(missing_mat) <- colnames(data)

# Define custom colors and breaks for binary matrix
colors <- c("white", "orange")
breaks <- c(0, 0.5, 1)

# Output heatmap as PNG
png("EDA_outputs/missing_data_heatmap_visdat.png", width = 1200, height = 600)
vis_miss(data) + 
  ggtitle("Missing Data Pattern in Grassland Dataset (2019–2023)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
dev.off()
# ============================================================
# 2. Distribution of Dry Matter Yield (Figure 3.8)
# ============================================================

# Histogram and density plot of DMY
png("EDA_outputs/DMY_distribution.png", width = 700, height = 450)
print(
  ggplot(data, aes(x = DMY)) +
    geom_histogram(fill = "steelblue", bins = 30, alpha = 0.8) +
    geom_density(aes(y = ..count..), color = "darkred", size = 1.2) +
    theme_minimal(base_size = 14) +
    labs(title = "Distribution of Dry Matter Yield (DMY)",
         x = "Dry Matter Yield (kg/ha)",
         y = "Frequency")
)
dev.off()

# ============================================================
# 3. Correlation Heatmap of Numeric Variables (Figure 3.9)
# ============================================================

# Select numeric columns only
num_data <- data %>% select(where(is.numeric))

# Compute correlation matrix
corr_matrix <- cor(num_data, use = "pairwise.complete.obs")

# Create color palette for correlations
corr_colors <- colorRampPalette(rev(brewer.pal(n = 9, name = "RdBu")))(100)

# Output correlation heatmap
png("EDA_outputs/correlation_heatmap.png", width = 800, height = 600)
print(
  pheatmap(corr_matrix,
           display_numbers = TRUE,
           number_format = "%.2f",
           color = corr_colors,
           main = "Correlation Matrix of Numeric Variables (2019–2023)",
           fontsize_number = 10)
)
dev.off()