# Read Debt_YoY data
debt_data <- read.csv("C:/Users/User/Desktop/時間序列/Debt_YoY.csv")
debt_data$observation_date <- as.Date(debt_data$observation_date, format = "%Y-%m-%d")

# Read GDP_YoY data
gdp_data <- read.csv("C:/Users/User/Desktop/時間序列/GDP_YoY.csv")
gdp_data$observation_date <- as.Date(gdp_data$observation_date, format = "%Y-%m-%d")

# Merge two data frames
merged_data <- merge(debt_data, gdp_data, by = "observation_date")

# Check data
head(merged_data)

# Create time series
# Create time series
debt_ts <- ts(merged_data$GFDEBTN_PC1, start = c(1967, 1), frequency = 4)
gdp_ts <- ts(merged_data$GDPC1_PC1, start = c(1967, 1), frequency = 4)

# First-order difference for Debt_YoY
debt_diff <- diff(debt_ts)

# Merge differenced Debt_YoY and original GDP_YoY
var_data <- cbind(Debt_YoY = debt_diff, GDP_YoY = gdp_ts[-1])  # Remove the first GDP value corresponding to the differenced data

# Check results
head(var_data)
# install.packages("vars")
library(vars)

# Select optimal lag order
lag_selection <- VARselect(var_data, lag.max = 10, type = "const")
print(lag_selection)

# Build VAR model
var_model <- VAR(var_data, p = lag_selection$selection["AIC(n)"], type = "const")

# View model summary
summary(var_model)
# Residual diagnostics
serial.test(var_model, lags.pt = 10, type = "PT.asymptotic")

plot(residuals(var_model))

# ==============================
# Granger Causality Test
# ==============================
# Test if Debt_YoY Granger causes GDP_YoY
granger1 <- causality(var_model, cause = "Debt_YoY")
print("Debt_YoY Granger Causes GDP_YoY:")
print(granger1$Granger)

# Test if GDP_YoY Granger causes Debt_YoY
granger2 <- causality(var_model, cause = "GDP_YoY")
print("GDP_YoY Granger Causes Debt_YoY:")
print(granger2$Granger)

# Validate VAR model stability:
stability <- roots(var_model)
print(stability)
# Impulse Response Function (IRF)
irf_result <- irf(var_model, impulse = "Debt_YoY", response = "GDP_YoY", n.ahead = 10)
plot(irf_result)
irf_result1 <- irf(var_model, impulse = "GDP_YoY", response = "Debt_YoY", n.ahead = 10)
plot(irf_result1)

# Forecast Error Variance Decomposition (FEVD):
fevd_result <- fevd(var_model, n.ahead = 10)
plot(fevd_result)

# Forecast
forecast <- predict(var_model, n.ahead = 8)
plot(forecast)

tail(merged_data)
