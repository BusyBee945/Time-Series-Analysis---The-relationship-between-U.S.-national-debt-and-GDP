# Time Series Analysis--The relationship between U.S. national debt and GDP
This project employed R language and time series method (ARIMA and VAR models) to investigate the effectiveness, extent, and timing of debt-financing policies on the economy using U.S. data

# Time Series Analysis of U.S. National Debt and GDP
This project investigates the relationship between U.S. national debt and Gross Domestic Product (GDP) using time series analysis techniques. The primary objective is to assess the effectiveness, extent, and timing of impacts of debt-financing policies on the economy, especially during periods of global economic challenges when governments often increase borrowing to stimulate growth. The analysis uses data from the United States, the world's largest economy.

## Data
The study utilizes quarterly year-over-year growth rates of U.S. public debt and GDP.

## Research Methodology
The research methodology involves several key steps:
1. Data Processing: The data is processed to achieve stationarity, which includes performing unit root tests, analyzing time trends, and accounting for seasonal effects.
2. ARIMA Model: An Autoregressive Integrated Moving Average (ARIMA) model is employed to individually analyze the autoregressive nature and time-series characteristics of both the debt and GDP variables.
3. VAR Model: A Vector Autoregression (VAR) model is then used to examine the mutual influences and lagged effects between the growth rate of debt and the growth rate of GDP.
4. Model Diagnostics: The goodness-of-fit of the chosen models is checked, and residuals are verified to behave like white noise to ensure model validity.

## Result

### Debt Data Analysis
1. Unit Root Test: The Augmented Dickey-Fuller (ADF) test initially indicated a unit root problem in the quarterly year-over-year growth rate data of U.S. public debt.
2. Differencing: After first differencing, the unit root problem was resolved.
3. ARIMA Model (0,1,4): An ARIMA(0,1,4) model was found to fit the debt data well, showing no residual autocorrelation, and residuals were approximately normally distributed with slight skewness in the tails.
4. Outlier Handling: An attempt to address extreme values in Q3 2020 and Q3 2021 (likely due to the pandemic) using dummy variables introduced autocorrelation issues in the model residuals.

### GDP Data Analysis
1. Unit Root Test: The ADF test indicated that the quarterly year-over-year GDP growth rate data did not have a unit root issue, thus differencing was not required.
2. Cointegration Test: Since only one of the two variables exhibited a unit root problem, no further cointegration tests were performed.
3. ARIMA Model (0,0,4): An ARIMA(0,0,4) model was found to fit the GDP data well, with no residual autocorrelation and approximately normally distributed residuals with slight skewness.

### VAR Model Analysis
1. Lag Selection: The VAR model's lag parameter was selected as 8, based on criteria such as AIC and BIC using the vars package in R.
2. Debt_YoY Regression: In the VAR model, with the first difference of Debt data as the dependent variable, Debt_YoY primarily reacted to its own past values.
3. GDP_YoY Regression: When GDP was the dependent variable, debt data showed a significant impact on GDP.
4. A positive effect was observed at lag 1, potentially related to market confidence and short-term consumption recovery.
5. Another positive effect at lag 5 is more likely associated with the multiplier effect of fiscal policy.
6. Residual Analysis: The residual plots and covariance matrix indicated that there is still room for improvement in the model, as the correlation remains high. This suggests the need to consider additional endogenous variables to improve the VAR model.

## Conclusion
The analysis indicates that the growth rate of debt has a significant positive impact on the GDP growth rate in the short term, possibly due to market confidence effects in the first lag. A positive impact observed in the fifth lag might reflect the delayed economic effects of policy stimuli, potentially related to the short-term multiplier effect of fiscal policy. While the current VAR model provides valuable insights, the high residual correlation suggests that incorporating additional endogenous variables would further improve the model's accuracy and explanatory power.
