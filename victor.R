# ================================================================
# Exploratory Data Analysis (EDA) Script
# Author: [Your Name]
# Date: [YYYY-MM-DD]
# Description: This script performs univariate and bivariate analysis,
#              visualizations, and statistical tests for COVID-19 data.
# ================================================================

# Load necessary libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(DataExplorer)
library(corrplot)
library(lubridate)
library(nortest)  # Normality tests
library(moments)  # Skewness & Kurtosis

# ================================================================
# 1. Load Cleaned Data
# ================================================================
bing_covid <- readRDS("cleaned_bing_covid.rds")

# Check dataset structure
glimpse(bing_covid)

# Check for missing values visually
plot_missing(bing_covid)

# ================================================================
# 2. Univariate Analysis
# ================================================================
# Summary statistics
summary(select_if(bing_covid, is.numeric))

# Visualize distributions
bing_covid %>%
  pivot_longer(cols = where(is.numeric), names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(x = log10(Value))) +
  geom_histogram(binwidth = 0.5, fill = "steelblue", alpha = 0.5, color = "white") +
  facet_wrap(~Variable, scales = "free") +
  theme_minimal() +
  labs(title = "Distribution of Numerical Variables", x = "log10(Value)", y = "Count")

# ================================================================
# 3. Bivariate Analysis
# ================================================================
# Compute correlation matrix
num_cols <- select_if(bing_covid, is.numeric)
cor_matrix <- cor(num_cols, use = "complete.obs")

# Visualize correlation matrix
corrplot(cor_matrix, method = "number", type = "upper", tl.cex = 0.8, col = rainbow(10))

# Boxplot of confirmed cases by country
bing_covid %>%
  ggplot(aes(x = factor(country_region), y = log10(confirmed))) +
  geom_boxplot(fill = "lightblue") +
  theme_minimal() +
  labs(title = "Confirmed Cases by Country", x = "Country", y = "Log(Confirmed Cases)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# ================================================================
# 4. Time-Series Analysis
# ================================================================
bing_covid$updated <- as.Date(bing_covid$updated)
global_trends <- bing_covid %>%
  group_by(updated) %>%
  summarise(confirmed = sum(confirmed, na.rm = TRUE),
            deaths = sum(deaths, na.rm = TRUE))

# Time-series visualization
ggplot(global_trends, aes(x = updated)) +
  geom_line(aes(y = confirmed, color = "Confirmed Cases"), size = 1) +
  geom_line(aes(y = deaths, color = "Deaths"), size = 1) +
  theme_minimal() +
  labs(title = "Global COVID-19 Trends Over Time", x = "Date", y = "Count") +
  scale_color_manual(values = c("blue", "red"))

# ================================================================
# End of Script
# ================================================================
