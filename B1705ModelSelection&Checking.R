# ----- B1705 Week 7 | Model Selection and Checking | 27.02.2024 -----
# ----- Lecture Work -----

##### 1.1. Loading Dataset -----

rm(list=ls())
df <- read.csv('https://www.dropbox.com/scl/fi/2zbxksqqv2igj3vq3hnh3/step_counts.csv?rlkey=t22p8s0wuzqbolewc0yph1m17&dl=1')

# Load library
library(xts)

##### 1.2. Creating Time-Series and Converting into XTS Object -----
# Create a sequence of 730 dates starting from January 1st, 2020
start_date <- as.Date("2020-01-01")
dates <- seq(from = start_date, by = "day", length.out = 730)

# Convert to an `xts` time-series object
df_xts <- xts(df, order.by = dates)

# Plot the time series
plot(df_xts, main = "Daily Step Counts", xlab = "Date", ylab = "Steps")

ts_data <- ts(df_xts, frequency = 365) # convert to ts format

##### 1.3. Classical Decomposition -----
decomposed <- decompose(ts_data)
plot(decomposed)

# ----- 2. Building Models -----
##### 2.1. Load necessary libraries -----
library(xts)
library(forecast)
library(ggplot2)

##### 2.2. Fitting AR Model -----
# Fit an AR model
ar_model <- auto.arima(df_xts, max.p = 5, max.q = 0, seasonal = FALSE)
summary(ar_model)

# AIC and BIC Scores at this stage provide us with a baseline to begin with

##### 2.3. Plotting Fitted Model -----
# Plot stepcount data
p <- ggplot(data = as.data.frame(df_xts), aes(x = index(df_xts), y = coredata(df_xts))) +
  geom_line(aes(y = coredata(df_xts)), color = "blue") +
  labs(title = "AR Model Visualisation", x = "Time", y = "Count")

# Add the fitted values
fitted_values <- fitted(ar_model)
p <- p + geom_line(aes(y = fitted_values), color = "red")

# Display the plot
print(p)

##### 2.4. Fitting Moving Average Model ----- 
# Fit an MA model
ma_model <- auto.arima(df_xts, max.p = 0, max.q = 5, seasonal = FALSE)
summary(ma_model)

##### 2.5. Plotting MA Model -----
# Plot stepcount data
p <- ggplot(data = as.data.frame(df_xts), aes(x = index(df_xts), y = coredata(df_xts))) +
  geom_line(aes(y = coredata(df_xts)), color = "blue") +
  labs(title = "MA Model Visualisation", x = "Time", y = "Count")

# Add the fitted values
fitted_values <- fitted(ma_model)
p <- p + geom_line(aes(y = fitted_values), color = "red")

# Display plot
print(p)

##### 2.6. Fitting ARIMA Model -----
# Fit an ARIMA model
arima_model <- auto.arima(df_xts)
summary(arima_model)

##### 2.7. Plotting ARIMA Model -----
# Plot stepcount data
p <- ggplot(data = as.data.frame(df_xts), aes(x = index(df_xts), y = coredata(df_xts))) +
  geom_line(aes(y = coredata(df_xts)), color = "blue") +
  labs(title = "ARIMA Model Visualisation", x = "Time", y = "Count")

# Add the fitted values
fitted_values <- fitted(arima_model)
p <- p + geom_line(aes(y = fitted_values), color = "red")

# Display plot
print(p)

##### 2.8. Fitting an Exponential Smoothing State Space Model (ETS) -----
ets_model <- ets(df_xts)
summary(ets_model)

##### 2.9. Plotting ETS Model -----
# Plot stepcount data
p <- ggplot(data = as.data.frame(df_xts), aes(x = index(df_xts), y = coredata(df_xts))) +
  geom_line(aes(y = coredata(df_xts)), color = "blue") +
  labs(title = "ETS Model Visualisation", x = "Time", y = "Count")

# Add the fitted values
fitted_values <- fitted(ets_model)
p <- p + geom_line(aes(y = fitted_values), color = "red")

# Display plot
print(p)

# ----- 3. Criterion Based Selection -----

checkresiduals(ar_model) # visual inspection

checkresiduals(ma_model) # visual inspection

checkresiduals(arima_model) # visual inspection

checkresiduals(ets_model) # visual inspection

print(arima_model$aic)
print(arima_model$bic)
print(ar_model$aic)
print(ar_model$bic)

# note that the AIC and BIC from the ARIMA model are lower than the AR model. This suggests that the ARIMA model is better.
print(ets_model$aic)
print(ets_model$bic)
print(ma_model$aic)
print(ma_model$bic)

# again, note that the ARIMA continues to out-perform the other models.


# ----- 4. Residual Diagnostics -----

# Assuming you have your four models: model1, model2, model3, model4

# Extract residuals from each model
residuals1 <- residuals(ar_model)
residuals2 <- residuals(arima_model)
residuals3 <- residuals(ets_model)
residuals4 <- residuals(ma_model)

# Apply the Ljung-Box test
ljung_box_result_ar <- Box.test(residuals1, type = "Ljung-Box")
ljung_box_result_arima <- Box.test(residuals2, type = "Ljung-Box")
ljung_box_result_ets <- Box.test(residuals3, type = "Ljung-Box")
ljung_box_result_ma <- Box.test(residuals4, type = "Ljung-Box")

# Print the results
print(ljung_box_result_ar)
print(ljung_box_result_arima)
print(ljung_box_result_ets)
print(ljung_box_result_ma)

# The function returns a test statistic and a p-value.

# A high p-value (typically above 0.05) suggests that 
# the residuals are independently distributed, which 
# is a good indication that the model has adequately 
# captured the information in the time series.n

# Conversely, a low p-value might indicate that there 
# is autocorrelation in the residuals, suggesting that 
# the model may be missing some information from the data.

##### 4.2. Homoscedasticity of Residuals #####
# Load packages
library(forecast)
library(tseries)
library(lmtest)
library(MASS)
library(FinTS)

# Step 1: Visual Inspection of Residuals
residuals_arima <- residuals(arima_model)
ts.plot(residuals_arima, main="Residuals over Time", ylab="Residuals")

# Auto-correlation function of the residuals
acf(residuals_arima, main="ACF of Residuals")

# Partial auto-correlation function of the residuals 
pacf(residuals_arima, main="PACF of Residuals")

# Breusch-Pagan Test
bp_test <- bptest(residuals_arima ~ fitted(arima_model) + I(fitted(arima_model)^2))
print(bp_test)

# Goldfeld-Quandt Test
gq_test <- gqtest(residuals_arima ~ fitted(arima_model))
print(gq_test)

##### 4.3. Normality of Residuals #####

# Extract residuals
residuals_arima <- residuals(arima_model)

# Q-Q plot
qqnorm(residuals_arima)
qqline(residuals_arima, col = "red")

# Shapiro-Wilk normality test
shapiro_test <- shapiro.test(residuals_arima)
print(shapiro_test)










