# Read the Data
file_path <- "C:/Users/User/Desktop/時間序列/Debt_YoY.csv"
debt_data <- read.csv(file_path)

# Check the data format
head(debt_data)

#table(is.na(debt_data$observation_date))  
#unique(debt_data$observation_date) # Missing value check   

str(debt_data)
debt_data$observation_date <- as.Date(debt_data$observation_date, format = "%Y-%m-%d")


# 將日期轉換為時間序列格式
debt_ts <- ts(debt_data$GFDEBTN_PC1, start = c(1967, 1), frequency = 4)

# 檢視時間序列
print(debt_ts)

# 繪製時間序列
plot(debt_ts, main = "Time Series Plot of Debt YoY", ylab = "Debt (%)", xlab = "Year", col = "blue")
library(tseries)

# 執行ADF檢定
adf_result <- adf.test(debt_ts, alternative = "stationary")
print(adf_result)

# 我們發現有單根
debt_diff <- diff(debt_ts)
print(debt_ts)
print(debt_diff)
# 繪製差分後的序列
plot(debt_diff, main = "Differenced Debt YoY", ylab = "Differenced Debt (%)", xlab = "Year", col = "blue")

adf_result_diff <- adf.test(debt_diff, alternative = "stationary")
print(adf_result_diff)

acf(debt_diff, main = "ACF of Differenced Debt YoY")
pacf(debt_diff, main = "PACF of Differenced Debt YoY")

# Model selection
# install.packages("forecast")  # 安裝 forecast 套件
library(forecast) 
ma_model <- arima(debt_ts, order = c(0,1,4))
summary(ma_model)
checkresiduals(ma_model)

## 處理2020、21的問題
# 確保日期正確
debt_data$observation_date <- as.Date(debt_data$observation_date, format = "%Y-%m-%d")

# 建立年份與季節欄位
debt_data$year <- as.numeric(format(debt_data$observation_date, "%Y"))
debt_data$quarter <- as.numeric(format(debt_data$observation_date, "%m")) %/% 3 + 1

# 新增 Dummy 變數
debt_data$dummy_2020_Q2 <- ifelse(debt_data$year == 2020 & debt_data$quarter == 2, 1, 0)
debt_data$dummy_2021_Q2 <- ifelse(debt_data$year == 2021 & debt_data$quarter == 2, 1, 0)
# 定義時間序列（假設是季資料）
debt_ts <- ts(debt_data$GFDEBTN_PC1, start = c(1967, 1), frequency = 4)

# 定義外生變數矩陣
dummy_vars <- cbind(debt_data$dummy_2020_Q2, debt_data$dummy_2021_Q2)

arima_model <- arima(debt_ts, order = c(0, 1, 4), xreg = dummy_vars)

# 查看模型摘要
summary(arima_model)
checkresiduals(arima_model)

