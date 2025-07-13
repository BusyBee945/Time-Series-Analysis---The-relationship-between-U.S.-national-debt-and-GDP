# 讀取 Debt_YoY 資料
debt_data <- read.csv("C:/Users/User/Desktop/時間序列/Debt_YoY.csv")
debt_data$observation_date <- as.Date(debt_data$observation_date, format = "%Y-%m-%d")

# 讀取 GDP_YoY 資料
gdp_data <- read.csv("C:/Users/User/Desktop/時間序列/GDP_YoY.csv")
gdp_data$observation_date <- as.Date(gdp_data$observation_date, format = "%Y-%m-%d")

# 合併兩個資料框
merged_data <- merge(debt_data, gdp_data, by = "observation_date")

# 檢查資料
head(merged_data)

# 建立時間序列
# 建立時間序列
debt_ts <- ts(merged_data$GFDEBTN_PC1, start = c(1967, 1), frequency = 4)
gdp_ts <- ts(merged_data$GDPC1_PC1, start = c(1967, 1), frequency = 4)

# 對 Debt_YoY 做一階差分
debt_diff <- diff(debt_ts)

# 合併差分後的 Debt_YoY 和原始 GDP_YoY
var_data <- cbind(Debt_YoY = debt_diff, GDP_YoY = gdp_ts[-1])  # 去除第一個差分後對應的 GDP 值

# 檢查結果
head(var_data)
# install.packages("vars")
library(vars)

# 選擇最佳滯後階數
lag_selection <- VARselect(var_data, lag.max = 10, type = "const")
print(lag_selection)

# 建立 VAR 模型
var_model <- VAR(var_data, p = lag_selection$selection["AIC(n)"], type = "const")

# 查看模型摘要
summary(var_model)
# 殘差診斷
serial.test(var_model, lags.pt = 10, type = "PT.asymptotic")

plot(residuals(var_model))

# ==============================
# Granger 因果檢定
# ==============================
# 測試 Debt_YoY 是否 Granger 導致 GDP_YoY
granger1 <- causality(var_model, cause = "Debt_YoY")
print("Debt_YoY Granger Causes GDP_YoY:")
print(granger1$Granger)

# 測試 GDP_YoY 是否 Granger 導致 Debt_YoY
granger2 <- causality(var_model, cause = "GDP_YoY")
print("GDP_YoY Granger Causes Debt_YoY:")
print(granger2$Granger)


#驗證 VAR 模型的穩定性：
stability <- roots(var_model)
print(stability)
# 脈衝反應函數 (Impulse Response Function, IRF)
irf_result <- irf(var_model, impulse = "Debt_YoY", response = "GDP_YoY", n.ahead = 10)
plot(irf_result)
irf_result1 <- irf(var_model, impulse = "GDP_YoY", response = "Debt_YoY", n.ahead = 10)
plot(irf_result1)

#預測誤差方差分解 (FEVD)：
fevd_result <- fevd(var_model, n.ahead = 10)
plot(fevd_result)

# 預測
forecast <- predict(var_model, n.ahead = 8)
plot(forecast)

tail(merged_data)

