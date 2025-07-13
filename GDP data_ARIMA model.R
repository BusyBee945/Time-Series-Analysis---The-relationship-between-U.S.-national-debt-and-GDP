# # # # Read the Data # # # #

file_path <- "C:/Users/User/Desktop/時間序列/GDP_YoY.csv"
GDP_data <- read.csv(file_path)

# Check the data format
head(GDP_data)

#table(is.na(GDP_data$observation_date))  
#unique(GDP_data$observation_date) # Missing value check   

# Add a date column
str(GDP_data)
GDP_data$observation_date <- as.Date(GDP_data$observation_date, format = "%Y/%m/%d")

# Define time series GDP_ts, check if needed：print(GDP_ts)
GDP_ts <- ts(GDP_data$GDPC1_PC1, start = c(1967, 1), frequency = 4)

# Plot GDP_ts
plot(GDP_ts, main = "Time Series Plot of GDP YoY", ylab = "Debt (%)", xlab = "Year", col = "blue")
library(tseries)

# # # # # # # # # # 
# # # # Data Testing and Model # # # #

# 1. ADF Test, no unit root detected
adf_result <- adf.test(GDP_ts, alternative = "stationary")
print(adf_result)

# 2. ACF / PACF
acf(GDP_ts, main = "ACF of GDP YoY")
pacf(GDP_ts, main = "PACF of GDP YoY")

# 3. Model Selection
# install.packages("forecast")  # Install forecast package

library(forecast) 
ma_model <- arima(GDP_ts, order = c(0,0,3))
summary(ma_model)
checkresiduals(ma_model)
#------------------------------------------------------------------------------#
# # # # Model Optimization # # # #

# 1. Epidemic (2020, 2021 Q3) Dummy

# (1) Create Year and Quarter columns
GDP_data$year <- as.numeric(format(GDP_data$observation_date, "%Y"))
GDP_data$quarter <- as.numeric(format(GDP_data$observation_date, "%m")) %/% 3 + 1

# (2) Add Dummy Variables
GDP_data$dummy_2020_Q2 <- ifelse(GDP_data$year == 2020 & GDP_data$quarter == 2, 1, 0)
GDP_data$dummy_2021_Q2 <- ifelse(GDP_data$year == 2021 & GDP_data$quarter == 2, 1, 0)

# (3) Recession Dummy
GDP_data$dummy_recession <- ifelse((GDP_data$year == 1990 & GDP_data$quarter == 3) |(GDP_data$year == 1990 & GDP_data$quarter == 4)|(GDP_data$year == 1991 & GDP_data$quarter == 1)|(GDP_data$year == 2001)|(GDP_data$year == 2007 & GDP_data$quarter == 4)|(GDP_data$year == 2008)| (GDP_data$year == 2009 & GDP_data$quarter == 1)|(GDP_data$year == 2009 & GDP_data$quarter == 2), 1, 0)

# (4) Define matrix of exogenous variables
covid_dummy <- cbind(GDP_data$dummy_2020_Q2, GDP_data$dummy_2021_Q2)
recession_dummy <- cbind(GDP_data$dummy_recession)
all_dummy <- cbind(GDP_data$dummy_2020_Q2, GDP_data$dummy_2021_Q2, GDP_data$dummy_recession)

# (5) Run Model on 3 different Dummy Variable set

##  ARIMA on COVID
arima_covid <- arima(GDP_ts, order = c(0, 0, 3), xreg = covid_dummy)
summary(arima_covid)
checkresiduals(arima_covid)

##  ARIMA on Recession
arima_recession <- arima(GDP_ts, order = c(0, 0, 3), xreg = recession_dummy)
summary(arima_recession)
checkresiduals(arima_recession)

##  ARIMA on both Dummy
arima_all_dummy <- arima(GDP_ts, order = c(0, 0, 3), xreg = all_dummy)
summary(arima_all_dummy)
checkresiduals(arima_all_dummy)

#------------------------------------------------------------------------------#
# # # # Detrend on time or Seasonality # # # #

# 1.  Identify Time Trend

# (1) Confirm existence of time trend
GDP_data$quarterly_dummy <- seq(1, nrow(GDP_data), by = 1)

GDP_quarter_time_trend <- lm(GDPC1_PC1 ~ quarterly_dummy, data = GDP_data)
summary(GDP_quarter_time_trend) # Confirmed, time trend still exists


# 2. Processing Methods

# Method 1: Generate linear trend variable, remove time trend, and use as new series for ARIMA
GDP_data$time_trend <- 1:nrow(GDP_data) 

# (1) Regress on t, remove time trend
lm_trend <- lm(GDPC1_PC1 ~ time_trend, data = GDP_data)
GDP_data$trend_component <- fitted(lm_trend)
GDP_data$GDP_detrended <- GDP_data$GDPC1_PC1 - GDP_data$trend_component

# (2) Define GDP Series after removing time trend
GDP_ts_detrended <- ts(GDP_data$GDP_detrended, start = c(1967, 1), frequency = 4)

# (3) ARIMA Test
detrend_arima_1 <- arima(GDP_ts_detrended, order = c(0, 0, 3), xreg = all_dummy)
summary(detrend_arima_1)
checkresiduals(detrend_arima_1)

# Method 2: Generate time dummy, incorporate into ARIMA as control variable

# (1) Define matrix of exogenous variables (including dummy variables and time trend variable)
exo_vars <- cbind(GDP_data$dummy_2020_Q2, GDP_data$dummy_2021_Q2,GDP_data$dummy_recession, GDP_data$time_trend)

# (2) ARIMA with t trend and exogenous variable
detrend_arima <- arima(GDP_ts, order = c(0, 0, 3), xreg = exo_vars)
summary(detrend_arima)
checkresiduals(detrend_arima)

# 3. Seasonality Test

# (1) Add Quarterly Dummy (Seasonality on Quarter)
GDP_data$Q1 <- ifelse(GDP_data$quarter == 1, 1, 0)
GDP_data$Q2 <- ifelse(GDP_data$quarter == 2, 1, 0)
GDP_data$Q3 <- ifelse(GDP_data$quarter == 3, 1, 0)
GDP_data$Q4 <- ifelse(GDP_data$quarter == 4, 1, 0)

GDP_Quarter_season_trend <- lm(GDPC1_PC1 ~ time_trend + Q2 + Q3 + Q4, data = GDP_data)
summary(GDP_Quarter_season_trend)

# # # # Split Data & Test # # # #

# (1) Split before and after 1990; filter data after 1990
GDP_data_subset <- GDP_data[GDP_data$year >= 1990, ]

# (2) Redefine time series
GDP_ts_subset <- ts(GDP_data_subset$GDPC1_PC1, start = c(1990, 1), frequency = 4)

# (3) Define exogenous variables
exo_vars_subset <- cbind(GDP_data_subset$dummy_2020_Q2, GDP_data_subset$dummy_2021_Q2, GDP_data_subset$recession_dummy, GDP_data_subset$time_trend)

# (4) ARIMA Model
arima_model_subset <- arima(GDP_ts_subset, order = c(0, 0, 3), xreg = exo_vars_subset)

summary(arima_model_subset)
checkresiduals(arima_model_subset)

# # # # Split Data & Test # # # #
# 4. Automatically select the optimal ARIMA model
auto_model <- auto.arima(GDP_ts, xreg = exo_vars, seasonal = FALSE)
summary(auto_model)
checkresiduals(auto_model)



# # # # Remove Outliers and Refit # # # #
# Remove data from 2020 Q2 and 2021 Q2
GDP_data_filtered <- GDP_data[!(GDP_data$year == 2020 & GDP_data$quarter == 2) & 
                                !(GDP_data$year == 2021 & GDP_data$quarter == 2), ]
# Confirm data format after removal
str(GDP_data_filtered)

# Redefine time series
GDP_ts_filtered <- ts(GDP_data_filtered$GDPC1_PC1, start = c(1967, 1), frequency = 4)

# Plot new series to confirm changes
plot(GDP_ts_filtered, main = "Filtered GDP YoY (Excluding 2020Q2 & 2021Q2)", ylab = "Debt (%)", xlab = "Year", col = "blue")

# Fit ARIMA(0,0,3) model
arima_filtered <- arima(GDP_ts_filtered, order = c(0, 0, 3))
summary(arima_filtered)

# Check if residuals are white noise
checkresiduals(arima_filtered)

# # # # Adjust Data to Two Standard Deviations # # # #

gdp_mean <- mean(GDP_data$GDPC1_PC1, na.rm = TRUE)
gdp_sd <- sd(GDP_data$GDPC1_PC1, na.rm = TRUE)

# Define upper and lower bounds (within two standard deviations)
upper_bound <- gdp_mean + 2 * gdp_sd
lower_bound <- gdp_mean - 2 * gdp_sd

# Adjust 2020Q2 and 2021Q2 data outside the bounds
GDP_data$GDPC1_PC1 <- ifelse((GDP_data$year == 2020 & GDP_data$quarter == 2) & 
                               GDP_data$GDPC1_PC1 > upper_bound, upper_bound + gdp_sd,
                             ifelse((GDP_data$year == 2021 & GDP_data$quarter == 2) &
                                      GDP_data$GDPC1_PC1 > upper_bound, upper_bound + gdp_sd,
                                    GDP_data$GDPC1_PC1))

# Check adjusted data
summary(GDP_data$GDPC1_PC1)

GDP_ts_adjusted <- ts(GDP_data$GDPC1_PC1, start = c(1967, 1), frequency = 4)
# Fit ARIMA(0,0,3) model
arima_adjusted <- arima(GDP_ts_adjusted, order = c(0, 0, 3))
summary(arima_adjusted)

# Check if residuals are white noise
checkresiduals(arima_adjusted)
