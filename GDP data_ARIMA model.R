# # # # Read the Data # # # #

file_path <- "C:/Users/User/Desktop/時間序列/GDP_YoY.csv"
GDP_data <- read.csv(file_path)

# Check the data format
head(GDP_data)

#table(is.na(GDP_data$observation_date))  
#unique(GDP_data$observation_date) # Missing value check   

# 新增一行日期格
str(GDP_data)
GDP_data$observation_date <- as.Date(GDP_data$observation_date, format = "%Y/%m/%d")

# 定義時間序列 GDP_ts, check if needed：print(GDP_ts)
GDP_ts <- ts(GDP_data$GDPC1_PC1, start = c(1967, 1), frequency = 4)

# 繪製GDP_ts
plot(GDP_ts, main = "Time Series Plot of GDP YoY", ylab = "Debt (%)", xlab = "Year", col = "blue")
library(tseries)

# # # # # # # # # # 
# # # # 資料檢定與模型 # # # #

# 1. ADF Test, 經檢定無單根
adf_result <- adf.test(GDP_ts, alternative = "stationary")
print(adf_result)

# 2. ACF / PACE
acf(GDP_ts, main = "ACF of GDP YoY")
pacf(GDP_ts, main = "PACF of GDP YoY")

# 3. Model Selection
# install.packages("forecast")  # 安裝 forecast 套件

library(forecast) 
ma_model <- arima(GDP_ts, order = c(0,0,3))
summary(ma_model)
checkresiduals(ma_model)
#------------------------------------------------------------------------------#
# # # # Model Optimization # # # #

# 1. 疫情(2020、2021Q3）Dummy

# (1) 建立年份與季節欄位
GDP_data$year <- as.numeric(format(GDP_data$observation_date, "%Y"))
GDP_data$quarter <- as.numeric(format(GDP_data$observation_date, "%m")) %/% 3 + 1

# (2) 新增 Dummy 變數
GDP_data$dummy_2020_Q2 <- ifelse(GDP_data$year == 2020 & GDP_data$quarter == 2, 1, 0)
GDP_data$dummy_2021_Q2 <- ifelse(GDP_data$year == 2021 & GDP_data$quarter == 2, 1, 0)

# (3) Recession Dummy
GDP_data$dummy_recession <- ifelse((GDP_data$year == 1990 & GDP_data$quarter == 3) |(GDP_data$year == 1990 & GDP_data$quarter == 4)|(GDP_data$year == 1991 & GDP_data$quarter == 1)|(GDP_data$year == 2001)|(GDP_data$year == 2007 & GDP_data$quarter == 4)|(GDP_data$year == 2008)| (GDP_data$year == 2009 & GDP_data$quarter == 1)|(GDP_data$year == 2009 & GDP_data$quarter == 2), 1, 0)

# (4) 定義外生變數矩陣
covid_dummy <- cbind(GDP_data$dummy_2020_Q2, GDP_data$dummy_2021_Q2)
recession_dummy <- cbind(GDP_data$dummy_recession)
all_dummy <- cbind(GDP_data$dummy_2020_Q2, GDP_data$dummy_2021_Q2, GDP_data$dummy_recession)

# (5) Run Model on 3 different Dummy Variable set

##  ARIMA on COVID
arima_covid <- arima(GDP_ts, order = c(0, 0, 3), xreg = covid_dummy)
summary(arima_covid)
checkresiduals(arima_covid)

##  ARIMA on Recession
arima_recession <- arima(GDP_ts, order = c(0, 0, 3), xreg = recession_dummy)
summary(arima_recession)
checkresiduals(arima_recession)

##  ARIMA on both Dummy
arima_all_dummy <- arima(GDP_ts, order = c(0, 0, 3), xreg = all_dummy)
summary(arima_all_dummy)
checkresiduals(arima_all_dummy)

#------------------------------------------------------------------------------#
# # # # Detrend on time or Seasonality # # # #

# 1.  時間趨勢處理辨認

# (1)確認時間趨勢存在
GDP_data$quarterly_dummy <- seq(1, nrow(GDP_data), by = 1)

GDP_quarter_time_trend <- lm(GDPC1_PC1 ~ quarterly_dummy, data = GDP_data)
summary(GDP_quarter_time_trend) # 經確認，發現時間趨勢依舊存在


# 2. 處理方法

# 方法一：生成線性trend variable、扣除time trend後做為新序列進行ARIMA
GDP_data$time_trend <- 1:nrow(GDP_data) 

# (1) 對t迴歸、扣除time trend
lm_trend <- lm(GDPC1_PC1 ~ time_trend, data = GDP_data)
GDP_data$trend_component <- fitted(lm_trend)
GDP_data$GDP_detrended <- GDP_data$GDPC1_PC1 - GDP_data$trend_component

# (2) 定義清除time trend後的GDP Series
GDP_ts_detrended <- ts(GDP_data$GDP_detrended, start = c(1967, 1), frequency = 4)

# (3) ARIMA Test
detrend_arima_1 <- arima(GDP_ts_detrended, order = c(0, 0, 3), xreg = all_dummy)
summary(detrend_arima_1)
checkresiduals(detrend_arima_1)

# 方法二：生成time dummy, 併入ARIMA做為控制變數

# (1)定義外生變數矩陣（包含 dummy 變數與time trend變數）
exo_vars <- cbind(GDP_data$dummy_2020_Q2, GDP_data$dummy_2021_Q2,GDP_data$dummy_recession, GDP_data$time_trend)

# (2) ARIMA with t trend and exogenous variable
detrend_arima <- arima(GDP_ts, order = c(0, 0, 3), xreg = exo_vars)
summary(detrend_arima)
checkresiduals(detrend_arima)

# 3. Seasonality檢驗

# (1) 新增季度Dummy (Seasonality on Quarter)
GDP_data$Q1 <- ifelse(GDP_data$quarter == 1, 1, 0)
GDP_data$Q2 <- ifelse(GDP_data$quarter == 2, 1, 0)
GDP_data$Q3 <- ifelse(GDP_data$quarter == 3, 1, 0)
GDP_data$Q4 <- ifelse(GDP_data$quarter == 4, 1, 0)

GDP_Quarter_season_trend <- lm(GDPC1_PC1 ~ time_trend + Q2 + Q3 + Q4, data = GDP_data)
summary(GDP_Quarter_season_trend)

# # # # 分割資料&檢驗 # # # #

# (1) 分割為1990前後；篩選 1990 年以後的資料
GDP_data_subset <- GDP_data[GDP_data$year >= 1990, ]

# (2) 重新定義時間序列
GDP_ts_subset <- ts(GDP_data_subset$GDPC1_PC1, start = c(1990, 1), frequency = 4)

# (3) 定義外生變數
exo_vars_subset <- cbind(GDP_data_subset$dummy_2020_Q2, GDP_data_subset$dummy_2021_Q2, GDP_data_subset$recession_dummy, GDP_data_subset$time_trend)

# (4) ARIMA 模型
arima_model_subset <- arima(GDP_ts_subset, order = c(0, 0, 3), xreg = exo_vars_subset)

summary(arima_model_subset)
checkresiduals(arima_model_subset)

# # # # 分割資料&檢驗 # # # #
# 4. 自動選擇最優的ARIMA模型
auto_model <- auto.arima(GDP_ts, xreg = exo_vars, seasonal = FALSE)
summary(auto_model)
checkresiduals(auto_model)



# # # # 刪除極端值並重新擬和 # # # #
# 移除2020 Q2 和 2021 Q2的數據
GDP_data_filtered <- GDP_data[!(GDP_data$year == 2020 & GDP_data$quarter == 2) & 
                                !(GDP_data$year == 2021 & GDP_data$quarter == 2), ]
# 確認資料移除後格式
str(GDP_data_filtered)

# 重新定義時間序列
GDP_ts_filtered <- ts(GDP_data_filtered$GDPC1_PC1, start = c(1967, 1), frequency = 4)

# 繪製新序列以確認變動
plot(GDP_ts_filtered, main = "Filtered GDP YoY (Excluding 2020Q2 & 2021Q2)", ylab = "Debt (%)", xlab = "Year", col = "blue")

# 擬合ARIMA(0,0,3)模型
arima_filtered <- arima(GDP_ts_filtered, order = c(0, 0, 3))
summary(arima_filtered)

# 檢查殘差是否為白噪音
checkresiduals(arima_filtered)

# # # # 修改數據到兩個標準差 # # # #

gdp_mean <- mean(GDP_data$GDPC1_PC1, na.rm = TRUE)
gdp_sd <- sd(GDP_data$GDPC1_PC1, na.rm = TRUE)

# 定義上下界（兩倍標準差範圍）
upper_bound <- gdp_mean + 2 * gdp_sd
lower_bound <- gdp_mean - 2 * gdp_sd

# 修改2020Q2 和 2021Q2數據到上下界之外
GDP_data$GDPC1_PC1 <- ifelse((GDP_data$year == 2020 & GDP_data$quarter == 2) & 
                               GDP_data$GDPC1_PC1 > upper_bound, upper_bound + gdp_sd,
                             ifelse((GDP_data$year == 2021 & GDP_data$quarter == 2) &
                                      GDP_data$GDPC1_PC1 > upper_bound, upper_bound + gdp_sd,
                                    GDP_data$GDPC1_PC1))

# 檢查修改後數據
summary(GDP_data$GDPC1_PC1)

GDP_ts_adjusted <- ts(GDP_data$GDPC1_PC1, start = c(1967, 1), frequency = 4)
# 擬合ARIMA(0,0,3)模型
arima_adjusted <- arima(GDP_ts_adjusted, order = c(0, 0, 3))
summary(arima_adjusted)

# 檢查殘差是否為白噪音
checkresiduals(arima_adjusted)



