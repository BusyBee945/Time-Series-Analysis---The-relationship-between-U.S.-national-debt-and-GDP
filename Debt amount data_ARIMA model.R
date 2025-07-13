# Read the Data
file_path <- "C:/Users/User/Desktop/時間序列/Debt_YoY.csv"
debt_data <- read.csv(file_path)

# Check the data format
head(debt_data)

#table(is.na(debt_data$observation_date))  
#unique(debt_data$observation_date) # Missing value check   

str(debt_data)
debt_data$observation_date <- as.Date(debt_data$observation_date, format = "%Y-%m-%d")


# Convert date to time series format
debt_ts <- ts(debt_data$GFDEBTN_PC1, start = c(1967, 1), frequency = 4)

# View time series
print(debt_ts)

# Plot time series
plot(debt_ts, main = "Time Series Plot of Debt YoY", ylab = "Debt (%)", xlab = "Year", col = "blue")
library(tseries)

# Perform ADF test
adf_result <- adf.test(debt_ts, alternative = "stationary")
print(adf_result)

# We found a unit root
debt_diff <- diff(debt_ts)
print(debt_ts)
print(debt_diff)
# Plot differenced series
plot(debt_diff, main = "Differenced Debt YoY", ylab = "Differenced Debt (%)", xlab = "Year", col = "blue")

adf_result_diff <- adf.test(debt_diff, alternative = "stationary")
print(adf_result_diff)

acf(debt_diff, main = "ACF of Differenced Debt YoY")
pacf(debt_diff, main = "PACF of Differenced Debt YoY")

# Model selection
# install.packages("forecast")  # Install forecast package
library(forecast) 
ma_model <- arima(debt_ts, order = c(0,1,4))
summary(ma_model)
checkresiduals(ma_model)

## Handle 2020, 2021 issues
# Ensure correct date
debt_data$observation_date <- as.Date(debt_data$observation_date, format = "%Y-%m-%d")

# Create year and quarter columns
debt_data$year <- as.numeric(format(debt_data$observation_date, "%Y"))
debt_data$quarter <- as.numeric(format(debt_data$observation_date, "%m")) %/% 3 + 1

# Add Dummy variables
debt_data$dummy_2020_Q2 <- ifelse(debt_data$year == 2020 & debt_data$quarter == 2, 1, 0)
debt_data$dummy_2021_Q2 <- ifelse(debt_data$year == 2021 & debt_data$quarter == 2, 1, 0)
# Define time series (assuming quarterly data)
debt_ts <- ts(debt_data$GFDEBTN_PC1, start = c(1967, 1), frequency = 4)

# Define exogenous variable matrix
dummy_vars <- cbind(debt_data$dummy_2020_Q2, debt_data$dummy_2021_Q2)

arima_model <- arima(debt_ts, order = c(0, 1, 4), xreg = dummy_vars)

# View model summary
summary(arima_model)
checkresiduals(arima_model)
