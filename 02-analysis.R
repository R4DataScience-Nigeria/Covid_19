
# 02_eda.R
# This script performs exploratory data analysis (EDA)
library(dplyr)  
library(tidyverse)
library(ggplot2)
library(DataExplorer)
library(corrplot)
library(corrplot)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(readr)
library(forecast)
library(nortest)  # For normality tests
library(moments)  # For skewness & kurtosis
# Load cleaned data
bing_covid <- readRDS("cleaned_bing_covid.rds")

# ------------------- Data Summary -------------------

# Check dataset dimensions
dim(bing_covid) # We have 542.813k rows and 17 columns

# Glimpse the dataset
glimpse(bing_covid)

# Check for missing values
plot_missing(bing_covid)  # Visualize missing values

# Group the data by country
bing_covid %>% 
  group_by(country_region)

year_data <- bing_covid %>% 
  mutate(month_year = floor_date(updated, 'month')) %>% 
  group_by(country_region, updated) %>% 
  summarise(across(c(confirmed, 
                     confirmed_change, 
                     deaths, 
                     deaths_change, 
                     recovered, 
                     recovered_change),
                   \(x) sum(x, na.rm = TRUE)), 
            .groups = 'drop')

view(year_data)
# ------------------- Univariate Analysis -------------------

# Summary statistics for numerical variables
summary(select_if(year_data, is.numeric))

# Transform the dataset from wide to long format for visualization
year_data %>%
  pivot_longer(cols = where(is.numeric), names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(x = log10(Value))) +
  
  # Histogram to show distribution
  geom_histogram(binwidth  = 0.5, fill = "steelblue", alpha = 0.5, color = "white") +
  
  # Facet by variable name to visualize distributions separately
  facet_wrap(~Variable, scales = "free") +
  
  # Minimal theme for better readability
  theme_minimal() +
  
  # Title and axis labels
  labs(
    title = "Distribution of Numerical Variables",
    x = "log10(Value)",  # Log-transformed scale for better distribution visibility
    y = "Count"
  )

# ---------------- Interpretation ----------------
# 1. Log transformation is applied (log10(Value)) to manage skewed distributions.
#    - This makes large values easier to compare and prevents a single variable from dominating the scale.
# 
# 2. Key observations for each variable:
#    - Confirmed Cases: Most values are low, but some extreme high values exist.
#    - Deaths: Skewed distribution, with a concentration of lower values.
#    - Recovered: Similar trend to confirmed cases, but smoother.
#    - Other Variables (confirmed_change, deaths_change, recovered_cha): Also right-skewed.
#
# 3. Key takeaways:
#    - The spikes indicate certain common reporting thresholds.
#    - The free scale ensures that each variable is interpreted independently.
#    - Further investigation is needed to analyze extreme values.
# -------------------------------------------------
# To show the common distribution for the RECOVERED (Adedolapo)
year_data %>% 
  ggplot(aes(x = log10(recovered))) + 
  geom_histogram(binwidth = 0.5, 
                 fill = "steelblue", 
                 color = "white") + 
  
  theme_minimal() + 
  labs(title = "Distribution of Numerical Variables")


# Function to confirmed the  distribution of a variable
check_distribution <- function(year_data, column) {
  values <- na.omit(year_data[[column]])  # Remove NAs
  
  # Shapiro-Wilk Test (for normality, best for small samples)
  shapiro_result <- shapiro.test(values)
  
  # Kolmogorov-Smirnov Test (for larger samples)
  ks_result <- ks.test(values, "pnorm", mean(values), sd(values))
  
  # Skewness & Kurtosis
  skew <- skewness(values)
  kurt <- kurtosis(values)
  
  # QQ Plot
  qq_plot <- ggplot(year_data, aes(sample = values)) +
    stat_qq() + stat_qq_line() +
    ggtitle(paste("QQ Plot of", column))
  
  print(qq_plot)
  
  return(list(
    Shapiro_Wilk_p = shapiro_result$p.value,
    KS_p = ks_result$p.value,
    Skewness = skew,
    Kurtosis = kurt
  ))
}

# Run checks for all key variables
columns_to_check <- c("confirmed", "confirmed_change", "deaths", "deaths_change", "recovered", "recovered_change")

results <- lapply(columns_to_check, function(col) {
  cat("\nChecking Distribution for:", col, "\n")
  print(check_distribution(year_data, col))
})

# How to Interpret
# Shapiro-Wilk & KS Test:
# p-value < 0.05 → Data not normally distributed.

# p-value > 0.05 → Data may be normal.

# Skewness:
  
# > 0 → Right-skewed.

#  ≈ 0 → Symmetric (normal).

# < 0 → Left-skewed.

# Kurtosis:
  
#  > 3 → Heavy tails (leptokurtic).

# ≈ 3 → Normal distribution.

# < 3 → Light tails (platykurtic).
# ------------------- Bivariate Analysis -------------------

# Select only numeric columns for correlation analysis
num_cols <- select_if(year_data, is.numeric)
# Compute correlation matrix for numeric variables
cor_matrix <- cor(num_cols %>% select_if(is.numeric), use = "complete.obs")

#  Visualize correlation matrix
corrplot(cor_matrix, method = "number", type = "upper", 
         tl.cex = 0.8, col = rainbow(10))

# Set up graphical parameters for better readability
par(mar = c(5, 5, 5, 5))  # Adjust margins for clear labels

# Plot correlation heatmap with coefficients
corrplot(
  corr_matrix, 
  method = "color",        # Use color representation
  type = "upper",          # Show only the upper triangle
  tl.col = "black",        # Change label color for clarity
  tl.cex = 0.9,            # Adjust text size for readability
  cl.cex = 0.5,            # Adjust legend text size
  col = colorRampPalette(c("blue", "white", "red"))(200),  # Custom color scale
  addCoef.col = "black",   # Add correlation coefficients in black
  number.cex = 0.6,         # Adjust coefficient text size
  title = "Bivariate Correlation Analysis of COVID-19 Metrics",
  mar = c(1, 1, 1, 1), # Adjust margin space
  cex.main = 1.5       # Adjust title size
)

# ------------------- Interpretation -------------------
# 1. Darker blue indicates strong negative correlations, while darker colors indicate stronger relationships.
# 2. High correlation between:
#    - confirmed & confirmed_change
#    - deaths & deaths_change
#    - recovered & recovered_change
# 3. Interdependencies between confirmed, deaths, and recovered suggest that these variables influence each other.
# 4. The upper triangle approach avoids redundant information.
# -------------------------------------------------------


# Boxplot of confirmed cases by country 
ggplot(year_data, aes(x = factor(country_region), y = log10(confirmed))) +
  geom_boxplot(fill = "lightblue") +
  theme_minimal() +
  labs(title = "Boxplot of Log tranforms for Confirmed Cases by Country", 
       x = "Country", 
       y = "Confirmed Cases") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for readability

# Interpretation:
# - India has the highest number of confirmed cases, with a wide distribution.
# - Other countries show relatively lower case counts, barely visible in the boxplot.
# - Some outliers exist, representing unusually high confirmed cases in certain countries.
 # Open a new plotting window

top_countries <- year_data %>%
  group_by(country_region) %>%
  summarise(total_confirmed = sum(confirmed, na.rm = TRUE)) %>%
  top_n(6, total_confirmed) %>%
  pull(country_region)

plot <- ggplot(year_data %>% filter(country_region %in% top_countries), 
               aes(x = factor(country_region), y = confirmed)) +
  geom_boxplot(fill = "lightblue") +
  theme_minimal() +
  labs(title = "Boxplot of Confirmed Cases for Top 6 Countries", 
       x = "Country", 
       y = "Confirmed Cases") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(plot)

# convert to time series data
ts_confirmed <- ts(df_global$confirmed, start = c(2020, 1), frequency = 365)
ts_deaths <- ts(df_global$deaths, start = c(2020, 1), frequency = 365)
ts_recovered <- ts(df_global$recovered, start = c(2020, 1), frequency = 365)

# To Visualize Time Series Trends (Multiple Variables & Countries)
ggplot(df_global, aes(x = date)) +
  geom_line(aes(y = confirmed, color = "Confirmed Cases"), size = 1) +
  geom_line(aes(y = deaths, color = "Deaths"), size = 1) +
  geom_line(aes(y = recovered, color = "Recovered"), size = 1) +
  facet_wrap(~country_region, scales = "free_y") +
  labs(title = "COVID-19 Cases Over Time by Country", x = "Date", y = "Count") +
  scale_color_manual(values = c("blue", "red", "green")) +
  theme_minimal()

# To visualize the time series trend
ggplot(df_global, aes(x = date)) +
  geom_line(aes(y = confirmed, color = "Confirmed Cases"), size = 1) +
  geom_line(aes(y = deaths, color = "Deaths"), size = 1) +
  labs(title = "COVID-19 Cases Over Time", x = "Date", y = "Count") +
  scale_color_manual(values = c("blue", "red")) +
  theme_minimal()



# How have confirmed cases and deaths evolved over time?

# Convert date column to Date format
year_data$updated <- as.Date(year_data$updated)

# Aggregate global confirmed cases and deaths over time
global_trends <- year_data %>%
  group_by(updated) %>%
  summarise(confirmed = sum(confirmed, na.rm = TRUE),
            deaths = sum(deaths, na.rm = TRUE))

# Plot the trend
ggplot(global_trends, aes(x = updated)) +
  geom_line(aes(y = confirmed, color = "Confirmed Cases"), size = 1) +
  geom_line(aes(y = deaths, color = "Deaths"), size = 1) +
  theme_minimal() +
  labs(title = "Global COVID-19 Trends Over Time",
       x = "Date",
       y = "Count",
       color = "Legend") +
  scale_color_manual(values = c("Confirmed Cases" = "blue", "Deaths" = "red"))

global_trends %>%
  pivot_longer(cols = 2:3, values_to = "value", names_to = "variable") %>% 
  ggplot(aes(x = updated, y = value)) +
  geom_line(aes(colour = variable)) +
  theme_minimal() +
  labs(title = "Global COVID-19 Trends Over Time",
       x = "Date",
       y = "Count",
       color = "Legend") +
  scale_color_manual(values = c("Confirmed Cases" = "blue", "Deaths" = "red"))+
  facet_grid(~variable)



# --------interpretaion-------- -------------------------------------------

# Global COVID-19 Trends Over Time
# The chart displays the trend of confirmed cases (blue line) and deaths (red line) globally over time.

# The confirmed cases show a steep rise, especially in 2021 and early 2022, indicating the rapid spread of COVID-19.

# The deaths remain significantly lower than the confirmed cases, which suggests improved medical interventions or lower fatality rates relative to infections.

# The vertical jumps in confirmed cases indicate data anomalies or reporting inconsistencies in certain periods.

# ---------------------------------------------------------- --------------


# Which countries show the highest growth rate in cases over time?

# Aggregate confirmed cases by country and date
country_trends <- year_data %>%
  group_by(country_region, updated) %>%
  summarise(confirmed = sum(confirmed, na.rm = TRUE))

# Select top 5 countries with highest total confirmed cases
top_countries <- country_trends %>%
  group_by(country_region) %>%
  summarise(total_cases = max(confirmed)) %>%
  top_n(5, total_cases) %>%
  pull(country_region)

# Filter dataset for these top countries
filtered_trends <- country_trends %>%
  filter(country_region %in% top_countries)

# Plot the trend for each country
ggplot(filtered_trends, aes(x = updated, y = confirmed, color = country_region)) +
  geom_line(size = 1) +
  facet_wrap(~country_region, scales = "free_y") +
  theme_minimal() +
  labs(title = "COVID-19 Growth Trends in Top 5 Affected Countries",
       x = "Date",
       y = "Confirmed Cases",
       color = "Country")

# -------------------Interpretation---------------------- -----------------

# COVID-19 Growth Trends in Top 5 Affected Countries

# This chart represents the growth trends in the top 5 affected countries: Costa Rica, India, Italy, United Kingdom, and the United States.
# 
# India and the United States show the highest number of confirmed cases, with India's cases exceeding 30 million.
# 
# Costa Rica, Italy, and the United Kingdom show a much lower case count but exhibit spikes, possibly due to specific outbreaks or delayed data reporting.
# 
# Some curves appear discontinuous, indicating missing or irregularly reported data.



# Computing Daily New COVID-19 Cases and Deaths per Country

year_data <- year_data %>%
  arrange(updated) %>%
  group_by(country_region) %>%
  mutate(daily_cases = confirmed - lag(confirmed, default = first(confirmed)),
         daily_deaths = deaths - lag(deaths, default = first(deaths)))

# Smoothing the data

# Apply LOESS smoothing or a rolling average to remove fluctuations in trends:
ggplot(year_data, aes(x = updated, y = confirmed, group = country_region, color = country_region)) +
  geom_line(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE) +  # LOESS smoothing
  theme_minimal() +
  labs(title = "Smoothed COVID-19 Trends Over Time", x = "Date", y = "Confirmed Cases")

ggplot(year_data, aes(x = updated, y = confirmed, color = country_region)) +
  geom_line() +
  geom_vline(xintercept = as.Date("2020-03-15"), linetype = "dashed", color = "red") +  # Lockdown start
  labs(title = "Effect of Lockdowns on COVID-19 Spread",
       x = "Date", y = "Confirmed Cases")

# --------------interpretation for effect of lock down --------------------

# Effect of Lockdowns on COVID-19 Spread
# The chart visualizes the growth in confirmed cases over time for six countries (Chile, Costa Rica, India, Italy, United Kingdom, and the United States).
# 
# A red dashed vertical line marks the lockdown implementation date.
# 
# India (green line) shows the steepest rise in cases, especially after lockdown measures were introduced.
# 
# The other countries have relatively lower confirmed cases, but there are noticeable spikes in Costa Rica and the United States.
# 
# The trend suggests that while lockdowns slowed the initial spread, the cases continued to rise, likely due to policy relaxations, new variants, or increased testing.

# ----------------------Geographic Analysis:--------- ---------------------

# Top 5 Countries with the Highest COVID-19 Cases
top_countries <- year_data %>%
  group_by(country_region) %>%
  summarise(total_cases = sum(confirmed, na.rm = TRUE),
            total_deaths = sum(deaths, na.rm = TRUE),
            total_recoveries = sum(recovered, na.rm = TRUE)) %>%
  arrange(desc(total_cases)) %>%
  top_n(5, wt = total_cases)

# Visualization
ggplot(top_countries, aes(x = reorder(country_region, total_cases), y = total_cases, fill = country_region)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 5 Countries by Total COVID-19 Cases", x = "Country", y = "Total Cases") +
  theme_minimal()

# interpretation

# the most affected countries, giving a clear comparison is India and followed by United States

#  Case Fatality Rate (CFR) Analysis Across Regions
covid_data <- year_data %>%
  mutate(case_fatality_rate = (deaths / confirmed) * 100)

ggplot(covid_data, aes(x = country_region, y = case_fatality_rate, fill = country_region)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Case Fatality Rate Across Countries", x = "Country", y = "CFR (%)") +
  theme_minimal()

# Choropleth map

# Load world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Load and process COVID-19 data
covid_data <- year_data  # Replace with actual file path

# Aggregate total cases by country
covid_cases <- covid_data %>%
  group_by(country_region) %>%
  summarise(total_cases = sum(confirmed, na.rm = TRUE))

# Merge COVID data with world map data
world_data <- left_join(world, covid_cases, by = c("name" = "country_region"))

# Plot choropleth map
ggplot(world_data) +
  geom_sf(aes(fill = total_cases), color = "black", size = 0.2) +
  scale_fill_gradient(low = "yellow", high = "red", na.value = "grey50") +
  labs(title = "Global COVID-19 Spread", fill = "Total Cases") +
  theme_minimal()


# -------------------Trends & Patterns in COVID-19 Cases---------- --------

# Are there seasonal trends in COVID-19 cases?

# Monthly trends in cases

# Convert 'updated' to Date format
year_data <- year_data %>%
  mutate(updated = as.Date(updated), 
         year = year(updated), 
         month = month(updated, label = TRUE))

# Aggregate monthly cases 
monthly_cases <- year_data %>%
  group_by(year, month) %>%
  summarise(total_cases = sum(daily_cases, na.rm = TRUE))

# Plot monthly trend
ggplot(monthly_cases, aes(x = month, y = total_cases, group = year, color = as.factor(year))) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Seasonal Trends in COVID-19 Cases",
       x = "Month",
       y = "Total Cases",
       color = "Year") +
  theme_minimal()

# Recovery rate by country
# Compute recovery rate
year_data <- year_data %>%
  mutate(recovery_rate = (recovered / confirmed) * 100)

# Check data
head(year_data)

# Load world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# Merge COVID data with world map
world_recovery <- world %>%
  left_join(year_data, by = c("name" = "country_region"))

# Global recovery rate
# Plot map
ggplot(data = world_recovery) +
  geom_sf(aes(fill = recovery_rate), color = "black") +
  scale_fill_gradient(low = "red", high = "green", na.value = "gray") +
  labs(title = "Global COVID-19 Recovery Rates",
       fill = "Recovery Rate (%)") +
  theme_minimal()

# Adedolapo started from here
# Time series Analysis
# Load necessary functions
library(dplyr)
library(lubridate)
library(zoo)
library(ggplot2)
library(xts)
library(dplyr)
library(tidyr)


# convert the date to time series date format
df_global <- year_data %>%
  mutate(date = as.Date(updated, format = "%Y-%m-%d")) %>%
  arrange(date)
view(df_global)

# To Handle Missing Values and Aggregate by Country & Date
df_global_1 <- df_global %>%
  group_by(country_region, date) %>%
  summarise(
    confirmed = sum(confirmed, na.rm = TRUE),
    deaths = sum(deaths, na.rm = TRUE),
    recovered = sum(recovered, na.rm = TRUE),
    .groups = "drop"
  )
view(df_global_1)
#--------------- IN reshaping the Data (pivoting to long format)
#----1. the original dataset had multiple columns ( confirmed, deaths, recovered etc.)
#----2. the code reshape it so that instead of seperating into diff columns for each
#----they become a single column called variable with values stored in a column
#---- value
# Convert data to long format
df_long <- df_global_1 %>%
  pivot_longer(cols = where(is.numeric),  # Pivot only numeric columns
               names_to = "variable",
               values_to = "value")
view(df_long)
# Separate the dataset into categories
df_death_recovered <- df_long %>% filter(variable %in% c("deaths", "recovered"))
df_countries <- df_long %>% filter(!variable %in% c("deaths", "recovered"))


# Load necessary libraries
library(ggplot2)
library(dplyr)
library(zoo)  # For rolling mean (smoothing)

# Plot for Deaths and Recovered only
# Handle missing values using interpolation
df_death_recovered <- df_death_recovered %>%
  group_by(variable) %>%
  mutate(value = na.approx(value, na.rm = FALSE))  # Linear interpolation

# Create the enhanced plot
ggplot(df_death_recovered, aes(x = date, y = value, color = variable)) +
  geom_line(size = 1) +  # Increase line thickness
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed") +  # Add smoothed trend lines
  facet_wrap(~variable, scales = "free_y") +
  labs(title = "COVID-19: Deaths and Recoveries Over Time",
       x = "Date", y = "Count") +
  scale_y_continuous(labels = scales::comma) +  # Format y-axis with commas
  scale_color_manual(values = c("deaths" = "red", "recovered" = "blue")) +  # Custom colors
  theme_minimal() +
  theme(legend.position = "bottom",  # Move legend to bottom
        text = element_text(size = 14))  # Increase text size

#------Observations from the Plot
#----Deaths (Red Panel):The death toll rises sharply around 
#-------2020–2021 and plateaus in 2022. A sudden break or missing data is visible in some parts.

# -------------   Recoveries (Blue Panel) :The recoveries increase exponentially, peaking at 30M+ cases in 2021.

# ---------------The trend follows a similar shape to deaths but at a much higher scale.

ggplot(df_countries, aes(x = date, y = value, color = variable)) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y") +
  labs(title = "COVID-19 Cases Trends Across Countries",
       x = "Date", y = "Count") +
  theme_minimal()

ggplot(df_long, aes(x = date, y = value, color = variable)) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y") +
  scale_y_log10() +   # Log scale to improve visibility
  labs(title = "COVID-19 Time Series Trends (Log Scale)",
       x = "Date", y = "Log Count") +
  theme_minimal()

ggplot(df_long, aes(x = date, y = value, color = variable)) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y") +  # Separate plots for each variable
  labs(title = "COVID-19 Time Series Trends",
       x = "Date",
       y = "Count") +
  theme_minimal()

ggplot(df_long, aes(x = date, y = value, color = variable)) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y") +  # Separate plots for each variable
  labs(title = "COVID-19 Time Series Trends",
       x = "Date",
       y = "Count") +
  theme_minimal()
view(df_long)
library(tseries)

#----- to extract the data variable and convert it into a time series object sarting 
#------ January 2020, the frequency = 365 means it is a daily time series
# Convert to time series format (deaths)
ts_deaths <- ts(df_long$value[df_long$variable == "deaths"],
                start = c(2020, 1), frequency = 365)

# STL decomposition breaks down the time series into three parts 
# 1. trend (long term moment)
# 2. seasonality(repeating patterns, e.g covid 19 cases )
# 3.remainder(random noise)

# STL decomposition
decomposed_deaths <- stl(ts_deaths, s.window = "periodic")

# Plot decomposition
autoplot(decomposed_deaths)
decomp_deaths <- decompose(ts_deaths, type = "multiplicative")
plot(decomp_deaths)  # Visualize trend & seasonality
# Run ADF Test for a selected variable (e.g., deaths)
adf_test <- adf.test(df_global_1$value[df_long$variable == "deaths"],
                     alternative = "stationary")

print(adf_test)

# To apply differencing after the first stationary when the p-value > 0.05
ts_diff_1 <- diff(ts_deaths, differences = 1)
adf.test(ts_diff_1)

# Convert to time series format (confirmed cases)
ts_confirmed <- ts(df_long$value[df_long$variable == "confirmed"],
                   start = c(2020, 1), frequency = 365)
# STL decomposition
decomposed_confirmed <- stl(ts_confirmed, s.window = "periodic")

# Plot decomposition
autoplot(decomposed_confirmed)
decomp_confirmed <- decompose(ts_confirmed, type = "multiplicative")
plot(decomp_confirmed)  # Visualize trend & seasonality
# Run ADF Test for a selected variable (e.g., confirmed)
adf_test <- adf.test(df_long$value[df_long$variable == "confirmed"],
                     alternative = "stationary")

print(adf_test)

# To apply differencing after the first stationary when the p-value > 0.05
ts_diff_2 <- diff(ts_confirmed, differences = 1)
adf.test(ts_diff_2)

# Convert to time series format (recovered case)
ts_recovered <- ts(df_long$value[df_long$variable == "recovered"],
                   start = c(2020, 1), frequency = 365)
# STL decomposition
decomposed_recovered <- stl(ts_recovered, s.window = "periodic")

# Plot decomposition
autoplot(decomposed_recovered)
decomp_recovered <- decompose(ts_recovered, type = "multiplicative")
plot(decomp_recovered)  # Visualize trend & seasonality
# Run ADF Test for a selected variable (e.g., deaths)
adf_test <- adf.test(df_long$value[df_long$variable == "recovered"],
                     alternative = "stationary")

print(adf_test)

# To apply differencing after the first stationary when the p-value > 0.05
ts_diff_3 <- diff(ts_recovered, differences = 1)
adf.test(ts_diff_3)

# since the covid_19 dataset is in stationary lets move further with ARIMA and Identify ARIMA Parameters (p, d, q)
library(forecast)


# Before fitting an ARIMA model, we need to determine the autoregressive 
# (AR) and moving average (MA) terms.

#------------------Run these for each of your differenced series:
# How to Interpret
# ACF (Autocorrelation Function):

#  If ACF tails off gradually, the series likely has an AR (p) process.

#  If ACF cuts off sharply, it suggests an MA (q) process.

#  PACF (Partial Autocorrelation Function):

#  If PACF cuts off after a few lags, it indicates the order of AR (p).

#  If PACF tails off gradually, it suggests an MA (q) process.

# Load necessary libraries
library(forecast)
library(ggplot2)

#---------------------------------------------------------
# ACF and PACF Analysis
#---------------------------------------------------------

# ACF and PACF for differenced deaths
acf(ts_diff_1, main = "ACF for Differenced Deaths")
pacf(ts_diff_1, main = "PACF for Differenced Deaths")

# ACF and PACF for differenced confirmed cases
acf(ts_diff_2, main = "ACF for Differenced Confirmed Cases")
pacf(ts_diff_2, main = "PACF for Differenced Confirmed Cases")
# ACF and PACF for differenced recovered
acf(ts_diff_3, main = "ACF for Differenced recovered cases")
pacf(ts_diff_3, main = "PACF for Differenced recovered cases")
#---------------------------------------------------------
# Automatically Selecting the Best ARIMA Model
#---------------------------------------------------------

# Find the best ARIMA model for deaths
model_deaths <- auto.arima(ts_deaths, seasonal = FALSE)
summary(model_deaths)

# Find the best ARIMA model for confirmed cases
model_confirmed <- auto.arima(ts_confirmed, seasonal = FALSE)
summary(model_confirmed)

# Find the best ARIMA model for recovered cases
model_recovered <- auto.arima(ts_recovered, seasonal = FALSE)
summary(model_recovered)

#---------------------------------------------------------
# Model Diagnostics (Checking Residuals)
#---------------------------------------------------------

# Check if residuals are white noise (random)
checkresiduals(model_deaths)
checkresiduals(model_confirmed)
checkresiduals(model_recovered)

#---------------------------------------------------------
# Forecasting Future Values
#---------------------------------------------------------

# Forecast for the next 30 days
forecast_deaths <- forecast(model_deaths, h = 30)
autoplot(forecast_deaths)

forecast_confirmed <- forecast(model_confirmed, h = 30)
autoplot(forecast_confirmed)

forecast_recovered <- forecast(model_recovered, h = 30)
autoplot(forecast_recovered)

#---------------------------------------------------------
# Evaluate Forecast Accuracy
#---------------------------------------------------------

accuracy(model_deaths)
accuracy(model_confirmed)
accuracy(model_recovered)

#---------------------------------------------------------
# Improving the Model: Finding the Best ARIMA Model
#---------------------------------------------------------

# Run auto.arima() with stepwise = FALSE and approximation = FALSE for better model selection
best_model_deaths <- auto.arima(ts_deaths, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
summary(best_model_deaths)

best_model_confirmed <- auto.arima(ts_confirmed, seasonal = FALSE)
summary(best_model_confirmed)

best_model_recovered <- auto.arima(ts_recovered, seasonal = FALSE)
summary(best_model_recovered)

# Evaluate accuracy of the best models
accuracy(best_model_confirmed)
accuracy(best_model_recovered)
accuracy(best_model_deaths)

#---------------------------------------------------------
# Applying Log Transformation to Improve Model Performance
#---------------------------------------------------------

ts_confirmed_log <- log(ts_confirmed + 1)
ts_deaths_log <- log(ts_deaths + 1)
ts_recovered_log <- log(ts_recovered + 1)

# Fit ARIMA models to log-transformed data
best_model_confirmed <- auto.arima(ts_confirmed_log)
best_model_deaths <- auto.arima(ts_deaths_log)
best_model_recovered <- auto.arima(ts_recovered_log)

# Evaluate accuracy of log-transformed models
accuracy(best_model_confirmed)
accuracy(best_model_deaths)
accuracy(best_model_recovered)
#---------------------------------------------------------
# 2. Forecasting Future Values
#---------------------------------------------------------

# Forecast for the next 30 days
forecast_deaths <- forecast(best_model_deaths, h = 30)
autoplot(forecast_deaths)

forecast_confirmed <- forecast(best_model_confirmed, h = 30)
autoplot(forecast_confirmed)

forecast_recovered <- forecast(best_model_recovered, h = 30)
autoplot(forecast_recovered)
