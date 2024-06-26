library(forecast)


#######################################
##########DANE DZIENNE#################
#######################################

forecast_values_SARIMA_PGE <- numeric()
forecast_values_SARIMA_PKN <- numeric()
forecast_values_SARIMA_PKO <- numeric()

# Parametry modelu SARIMA
order_PGE <- c(1, 1, 1)
seasonal_order_PGE <- c(1, 1, 1, 7)

order_PKN <- c(1, 1, 2)
seasonal_order_PKN <- c(1, 1, 2, 7)

order_PKO <- c(2, 1, 2)
seasonal_order_PKO <- c(2, 1, 2, 7)

forecast_steps_SARIMA_PGE_daily <- length(df_test_PGE_daily)

data_PGE_SARIMA_train_daily <- PGE_daily$daily_turnover[1:1162]
data_PKN_SARIMA_train_daily_s <- PKN_daily$daily_turnover[1:1162]
data_PKO_SARIMA_train_daily <- PKO_daily$daily_turnover[1:1162]
df_test_PGE_daily <- PGE_daily$daily_turnover[1163:length(PGE_daily$daily_turnover)]
df_test_PKN_daily_s <- PKN_daily$daily_turnover[1163:length(PKN_daily$daily_turnover)]
df_test_PKO_daily <- PKO_daily$daily_turnover[1163:length(PKO_daily$daily_turnover)]

model_PGE_SARIMA_daily <- Arima(data_PGE_SARIMA_train_daily, order = order_PGE, seasonal = seasonal_order_PGE)
model_PKN_SARIMA_daily <- Arima(data_PKN_SARIMA_train_daily_s, order = order_PKN, seasonal = seasonal_order_PKN)
model_PKO_SARIMA_daily <- Arima(data_PKO_SARIMA_train_daily, order = order_PKO, seasonal = seasonal_order_PKO)

# Pętla prognozowania dla każdego kroku
for (i in 1:forecast_steps_SARIMA_PGE_daily) {
  
  forecast_SARIMA_PGE_daily <- forecast(model_PGE_SARIMA_daily, h = 1)
  forecast_SARIMA_PKN_daily <- forecast(model_PKN_SARIMA_daily, h = 1)
  forecast_SARIMA_PKO_daily <- forecast(model_PKO_SARIMA_daily, h = 1)
  
  forecast_value_PGE_SARIMA <- forecast_SARIMA_PGE_daily$mean[1]
  forecast_value_PKN_SARIMA <- forecast_SARIMA_PKN_daily$mean[1]
  forecast_value_PKO_SARIMA <- forecast_SARIMA_PKO_daily$mean[1]
  
  actual_value_PGE_SARIMA <- df_test_PGE_daily[i]
  actual_value_PKN_SARIMA <- df_test_PKN_daily_s[i]
  actual_value_PKO_SARIMA <- df_test_PKO_daily[i]
  
  data_PGE_SARIMA_train_daily <- c(data_PGE_SARIMA_train_daily, actual_value_PGE_SARIMA)
  data_PKN_SARIMA_train_daily_s <- c(data_PKN_SARIMA_train_daily_s, actual_value_PKN_SARIMA)
  data_PKO_SARIMA_train_daily <- c(data_PKO_SARIMA_train_daily, actual_value_PKO_SARIMA)
  
  model_PGE_SARIMA_daily <- Arima(data_PGE_SARIMA_train_daily, order = order_PGE, seasonal = seasonal_order_PGE)
  model_PKN_SARIMA_daily <- Arima(data_PKN_SARIMA_train_daily_s, order = order_PKN, seasonal = seasonal_order_PKN)
  model_PKO_SARIMA_daily <- Arima(data_PKO_SARIMA_train_daily, order = order_PKO, seasonal = seasonal_order_PKO)
  
  forecast_values_SARIMA_PGE <- c(forecast_values_SARIMA_PGE, forecast_value_PGE_SARIMA)
  forecast_values_SARIMA_PKN <- c(forecast_values_SARIMA_PKN, forecast_value_PKN_SARIMA)
  forecast_values_SARIMA_PKO <- c(forecast_values_SARIMA_PKO, forecast_value_PKO_SARIMA)
}


par(mfrow=c(3,1), mar=c(4,4,2,1), oma=c(0,0,2,0))
plot(PGE_daily$Date[1:1162], PGE_daily$daily_turnover[1:1162], main="PGE - SARIMA(1,1,1)", xlab="", ylab="", col="blue", type='l')
lines(PGE_daily$Date[1163:length(PGE_daily$daily_turnover)], df_test_PGE_daily, col="red")
lines(PGE_daily$Date[1163:length(PGE_daily$daily_turnover)], forecast_values_SARIMA_PGE, col="green")
legend("topleft", legend = c("Dane treningowe", "Dane testowe" ,"Predykcja one-step-ahead"), col = c("blue", "red", "green"), lty = 1:1, cex = 0.8)

plot(PKN_daily$Date[1:1162],PKN_daily$daily_turnover[1:1162], main="PKN - SARIMA(1,1,2)", xlab="", ylab="Dzienny obrót spółek", col="blue", type='l')
lines(PKN_daily$Date[1163:length(PKN_daily$daily_turnover)], df_test_PKN_daily_s, col="red")
lines(PKN_daily$Date[1163:length(PKN_daily$daily_turnover)], forecast_values_SARIMA_PKN, col="green")
legend("topleft", legend = c("Dane treningowe", "Dane testowe" ,"Predykcja one-step-ahead"), col = c("blue", "red", "green"), lty = 1:1, cex = 0.8)

plot(PKO_daily$Date[1:1162],  PKO_daily$daily_turnover[1:1162], main="PKO - SARIMA(2,1,2)", xlab = "data", ylab="", col="blue", type='l')
lines(PKO_daily$Date[1163:length(PKO_daily$daily_turnover)], df_test_PKO_daily, col="red")
lines(PKO_daily$Date[1163:length(PKO_daily$daily_turnover)], forecast_values_SARIMA_PKO, col="green")
legend("topleft", legend = c("Dane treningowe", "Dane testowe" ,"Predykcja one-step-ahead"), col = c("blue", "red", "green"), lty = 1:1, cex = 0.8)

mtext("Predykcja one-step-ahead za pomocą szeregu SARIMA", side=3, line=2, outer=TRUE)










forecast_values_SARIMA_PGE <- numeric()
forecast_values_SARIMA_PKN <- numeric()
forecast_values_SARIMA_PKO <- numeric()

# Parametry modelu SARIMA
order_PGE <- c(1, 1, 1)
seasonal_order_PGE <- c(1, 1, 1, 7)

order_PKN <- c(1, 1, 2)
seasonal_order_PKN <- c(1, 1, 2, 7)

order_PKO <- c(2, 1, 2)
seasonal_order_PKO <- c(2, 1, 2, 7)

forecast_steps_SARIMA_PGE_daily <- length(df_test_PGE_daily)

data_PGE_SARIMA_train_daily <- PGE_daily$daily_turnover[1:1162]
data_PKN_SARIMA_train_daily_s <- PKN_daily$daily_turnover[1:1162]
data_PKO_SARIMA_train_daily <- PKO_daily$daily_turnover[1:1162]
df_test_PGE_daily <- PGE_daily$daily_turnover[1163:length(PGE_daily$daily_turnover)]
df_test_PKN_daily_s <- PKN_daily$daily_turnover[1163:length(PKN_daily$daily_turnover)]
df_test_PKO_daily <- PKO_daily$daily_turnover[1163:length(PKO_daily$daily_turnover)]

model_PGE_SARIMA_daily <- Arima(data_PGE_SARIMA_train_daily, order = order_PGE, seasonal = seasonal_order_PGE)
model_PKN_SARIMA_daily <- Arima(data_PKN_SARIMA_train_daily_s, order = order_PKN, seasonal = seasonal_order_PKN)
model_PKO_SARIMA_daily <- Arima(data_PKO_SARIMA_train_daily, order = order_PKO, seasonal = seasonal_order_PKO)

# Pętla prognozowania dla każdego kroku
for (i in 1:forecast_steps_SARIMA_PGE_daily) {
  
  forecast_SARIMA_PGE_daily <- forecast(model_PGE_SARIMA_daily, h = 1)
  forecast_SARIMA_PKN_daily <- forecast(model_PKN_SARIMA_daily, h = 1)
  forecast_SARIMA_PKO_daily <- forecast(model_PKO_SARIMA_daily, h = 1)
  
  forecast_value_PGE_SARIMA <- forecast_SARIMA_PGE_daily$mean[1]
  forecast_value_PKN_SARIMA <- forecast_SARIMA_PKN_daily$mean[1]
  forecast_value_PKO_SARIMA <- forecast_SARIMA_PKO_daily$mean[1]
  
  actual_value_PGE_SARIMA <- df_test_PGE_daily[i]
  actual_value_PKN_SARIMA <- df_test_PKN_daily_s[i]
  actual_value_PKO_SARIMA <- df_test_PKO_daily[i]
  
  data_PGE_SARIMA_train_daily <- c(data_PGE_SARIMA_train_daily, actual_value_PGE_SARIMA)
  data_PKN_SARIMA_train_daily_s <- c(data_PKN_SARIMA_train_daily_s, actual_value_PKN_SARIMA)
  data_PKO_SARIMA_train_daily <- c(data_PKO_SARIMA_train_daily, actual_value_PKO_SARIMA)
  
  model_PGE_SARIMA_daily <- Arima(data_PGE_SARIMA_train_daily, order = order_PGE, seasonal = seasonal_order_PGE)
  model_PKN_SARIMA_daily <- Arima(data_PKN_SARIMA_train_daily_s, order = order_PKN, seasonal = seasonal_order_PKN)
  model_PKO_SARIMA_daily <- Arima(data_PKO_SARIMA_train_daily, order = order_PKO, seasonal = seasonal_order_PKO)
  
  forecast_values_SARIMA_PGE <- c(forecast_values_SARIMA_PGE, forecast_value_PGE_SARIMA)
  forecast_values_SARIMA_PKN <- c(forecast_values_SARIMA_PKN, forecast_value_PKN_SARIMA)
  forecast_values_SARIMA_PKO <- c(forecast_values_SARIMA_PKO, forecast_value_PKO_SARIMA)
}


par(mfrow=c(3,1), mar=c(4,4,2,1), oma=c(0,0,2,0))
plot(PGE_daily$Date[1:1162], PGE_daily$daily_turnover[1:1162], main="PGE - SARIMA(1,1,1)", xlab="", ylab="", col="blue", type='l')
lines(PGE_daily$Date[1163:length(PGE_daily$daily_turnover)], df_test_PGE_daily, col="red")
lines(PGE_daily$Date[1163:length(PGE_daily$daily_turnover)], forecast_values_SARIMA_PGE, col="green")
legend("topleft", legend = c("Dane treningowe", "Dane testowe" ,"Predykcja one-step-ahead"), col = c("blue", "red", "green"), lty = 1:1, cex = 0.8)

plot(PKN_daily$Date[1:1162],PKN_daily$daily_turnover[1:1162], main="PKN - SARIMA(1,1,2)", xlab="", ylab="Dzienny obrót spółek", col="blue", type='l')
lines(PKN_daily$Date[1163:length(PKN_daily$daily_turnover)], df_test_PKN_daily_s, col="red")
lines(PKN_daily$Date[1163:length(PKN_daily$daily_turnover)], forecast_values_SARIMA_PKN, col="green")
legend("topleft", legend = c("Dane treningowe", "Dane testowe" ,"Predykcja one-step-ahead"), col = c("blue", "red", "green"), lty = 1:1, cex = 0.8)

plot(PKO_daily$Date[1:1162],  PKO_daily$daily_turnover[1:1162], main="PKO - SARIMA(2,1,2)", xlab = "data", ylab="", col="blue", type='l')
lines(PKO_daily$Date[1163:length(PKO_daily$daily_turnover)], df_test_PKO_daily, col="red")
lines(PKO_daily$Date[1163:length(PKO_daily$daily_turnover)], forecast_values_SARIMA_PKO, col="green")
legend("topleft", legend = c("Dane treningowe", "Dane testowe" ,"Predykcja one-step-ahead"), col = c("blue", "red", "green"), lty = 1:1, cex = 0.8)

mtext("Predykcja one-step-ahead za pomocą szeregu SARIMA", side=3, line=2, outer=TRUE)