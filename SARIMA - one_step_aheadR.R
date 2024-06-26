library(forecast)


###K.Burnecki, analiza residuow dla arfimy (dane dzienne)

#######################################
##########DANE DZIENNE################# 
#######################################

#SARIMA one-step-ahead prediction
forecast_values_SARIMA_PGE <- numeric()
forecast_values_SARIMA_PKN <- numeric()
forecast_values_SARIMA_PKO <- numeric()

# Parametry modelu SARIMA
order_PGE <- c(1, 1, 1)
seasonal_order_PGE <- c(1, 1, 1, 9)

order_PKN <- c(1, 1, 2)
seasonal_order_PKN <- c(1, 1, 2, 9)

order_PKO <- c(2, 1, 2)
seasonal_order_PKO <- c(2, 1, 2, 9)

forecast_steps_SARIMA_PGE_daily <- length(df_test_PGE_daily)

data_PGE_SARIMA_train_daily <- PGE_daily$daily_turnover[1:1162]
data_PKN_SARIMA_train_daily_s <- PKN_daily$daily_turnover[1:1162]
data_PKO_SARIMA_train_daily <- PKO_daily$daily_turnover[1:1162]
df_test_PGE_daily <- PGE_daily$daily_turnover[1163:length(PGE_daily$daily_turnover)]
df_test_PKN_daily_s <- PKN_daily$daily_turnover[1163:length(PKN_daily$daily_turnover)]
df_test_PKO_daily <- PKO_daily$daily_turnover[1163:length(PKO_daily$daily_turnover)]

model_PGE_SARIMA_daily <- Arima(data_PGE_SARIMA_train_daily, order = order_PGE, seasonal=seasonal_order_PGE)
model_PKN_SARIMA_daily <- Arima(data_PKN_SARIMA_train_daily_s, order = order_PKN, seasonal=seasonal_order_PKN)
model_PKO_SARIMA_daily <- Arima(data_PKO_SARIMA_train_daily, order = order_PKO, seasonal=seasonal_order_PKO)

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
  
  model_PGE_SARIMA_daily <- Arima(data_PGE_SARIMA_train_daily, order = order_PGE, seasonal=seasonal_order_PGE)
  model_PKN_SARIMA_daily <- Arima(data_PKN_SARIMA_train_daily_s, order = order_PKN, seasonal=seasonal_order_PKN)
  model_PKO_SARIMA_daily <- Arima(data_PKO_SARIMA_train_daily, order = order_PKO, seasonal=seasonal_order_PKO)
  
  forecast_values_SARIMA_PGE <- c(forecast_values_SARIMA_PGE, forecast_value_PGE_SARIMA)
  forecast_values_SARIMA_PKN <- c(forecast_values_SARIMA_PKN, forecast_value_PKN_SARIMA)
  forecast_values_SARIMA_PKO <- c(forecast_values_SARIMA_PKO, forecast_value_PKO_SARIMA)
}


par(mfrow=c(3,1), mar=c(5,5,2,2), oma=c(0,0,2,0), xaxt='s', yaxt = "s", las = 1, mgp=c(4,1,0))
#plot(PGE_daily$Date[1:1162], PGE_daily$daily_turnover[1:1162], main="PGE - ARFIMA(1,1,1)", xlab="", ylab="", col="blue", type='l', yaxt = "n")
plot(PGE_daily$Date[1163:length(PGE_daily$daily_turnover)], df_test_PGE_daily, main="PGE - SARIMA(1,1,1)", xlab="", ylab="", col="blue", type='l', xaxt="n", yaxt = "n")
points(PGE_daily$Date[1163:length(PGE_daily$daily_turnover)], forecast_values_SARIMA_PGE, col="red", xaxt= "n", yaxt = "n", pch=4)
legend("topleft", legend = c("Dane testowe" ,"Predykcja one-step-ahead"), col = c("blue", "red"), lty=c(1,0), cex = 0.9, pch=c(NA, 4))
axis(side = 2, at = pretty(PGE_daily$daily_turnover), labels = sapply(pretty(PGE_daily$daily_turnover), millions_formatter))
axis.Date(1, at = PGE_daily$Date, format="%m-%Y", col = "transparent")

#plot(PKN_daily$Date[1:1162],PKN_daily$daily_turnover[1:1162], main="PKN - ARFIMA(1,1,2)", xlab="", ylab="Dzienny obrót spółek", col="blue", type='l', yaxt = "n")
plot(PKN_daily$Date[1163:length(PKN_daily$daily_turnover)], df_test_PKN_daily_s, main="PKN - SARIMA(1,1,2)", xlab="", ylab="dzienny obrót spółek", col="blue", type='l', xaxt="n", yaxt = "n")
points(PKN_daily$Date[1163:length(PKN_daily$daily_turnover)], forecast_values_SARIMA_PKN, col="red", xaxt = "n", yaxt = "n", pch=4)
legend("topleft", legend = c("Dane testowe" ,"Predykcja one-step-ahead"), col = c("blue", "red"), lty=c(1,0), cex = 0.9, pch=c(NA, 4))
axis(side = 2, at = pretty(PKN_daily$daily_turnover), labels = sapply(pretty(PKN_daily$daily_turnover), millions_formatter))
axis.Date(1, at = PKN_daily$Date, format="%m-%Y", col = "transparent")


#plot(PKO_daily$Date[1:1162],  PKO_daily$daily_turnover[1:1162], main="PKO - ARFIMA(2,1,2)", xlab = "data", ylab="", col="blue", type='l', yaxt = "n")
plot(PKO_daily$Date[1163:length(PKO_daily$daily_turnover)], df_test_PKO_daily, main="PKO - SARIMA(2,1,2)", xlab = "data", ylab="", col="blue", type='l', xaxt= "n", yaxt = "n")
points(PKO_daily$Date[1163:length(PKO_daily$daily_turnover)], forecast_values_SARIMA_PKO, col="red", xaxt = "n", yaxt = "n", pch=4)
legend("topleft", legend = c("Dane testowe" ,"Predykcja one-step-ahead"), col = c("blue", "red"), lty=c(1,0), cex = 0.9, pch=c(NA, 4))
axis(side = 2, at = pretty(PKO_daily$daily_turnover), labels = sapply(pretty(PKO_daily$daily_turnover), millions_formatter))
axis.Date(1, at = PKO_daily$Date, format="%m-%Y", col = "transparent")

mtext("Predykcja one-step-ahead za pomocą szeregu SARIMA - dane dzienne", side=3, line=0.7, outer=TRUE)


#######################################
##########DANE TYGODNIOWE##############
#######################################


forecast_values_SARIMA_PGE_weekly <- numeric()
forecast_values_SARIMA_PKN_weekly <- numeric()
forecast_values_SARIMA_PKO_weekly <- numeric()

# Parametry modelu SARIMA
order_PGE_weekly <- c(1, 2, 2)
seasonal_order_PGE_weekly <- c(1, 2, 2, 18)

order_PKN_weekly <- c(1, 2, 2)
seasonal_order_PKN_weekly <- c(1, 2, 2, 18)

order_PKO_weekly <- c(1, 2, 2)
seasonal_order_PKO_weekly <- c(1, 2, 2, 18)

forecast_steps_SARIMA_PGE_weekly <- length(df_test_PGE_weekly)

data_PGE_SARIMA_train_weekly <- PGE_weekly$daily_turnover[1:272]
data_PKN_SARIMA_train_daily_s_weekly <- PKN_weekly$daily_turnover[1:272]
data_PKO_SARIMA_train_weekly <- PKO_weekly$daily_turnover[1:272]
df_test_PGE_weekly <- PGE_weekly$daily_turnover[273:length(PGE_weekly$daily_turnover)]
df_test_PKN_daily_s_weekly <- PKN_weekly$daily_turnover[273:length(PKN_weekly$daily_turnover)]
df_test_PKO_weekly <- PKO_weekly$daily_turnover[273:length(PKO_weekly$daily_turnover)]

model_PGE_SARIMA_weekly <- Arima(data_PGE_SARIMA_train_weekly, order = order_PGE, seasonal=seasonal_order_PGE)
model_PKN_SARIMA_weekly <- Arima(data_PKN_SARIMA_train_daily_s_weekly, order = order_PKN, seasonal=seasonal_order_PKN)
model_PKO_SARIMA_weekly <- Arima(data_PKO_SARIMA_train_weekly, order = order_PKO, seasonal=seasonal_order_PKO)

# Pętla prognozowania dla każdego kroku
for (i in 1:forecast_steps_SARIMA_PGE_weekly) {
  
  forecast_SARIMA_PGE_weekly <- forecast(model_PGE_SARIMA_weekly, h = 1)
  forecast_SARIMA_PKN_weekly <- forecast(model_PKN_SARIMA_weekly, h = 1)
  forecast_SARIMA_PKO_weekly <- forecast(model_PKO_SARIMA_weekly, h = 1)
  
  forecast_value_PGE_SARIMA_weekly <- forecast_SARIMA_PGE_weekly$mean[1]
  forecast_value_PKN_SARIMA_weekly <- forecast_SARIMA_PKN_weekly$mean[1]
  forecast_value_PKO_SARIMA_weekly <- forecast_SARIMA_PKO_weekly$mean[1]
  
  actual_value_PGE_SARIMA_weekly <- df_test_PGE_weekly[i]
  actual_value_PKN_SARIMA_weekly <- df_test_PKN_daily_s_weekly[i]
  actual_value_PKO_SARIMA_weekly <- df_test_PKO_weekly[i]
  
  data_PGE_SARIMA_train_weekly <- c(data_PGE_SARIMA_train_weekly, actual_value_PGE_SARIMA_weekly)
  data_PKN_SARIMA_train_daily_s_weekly <- c(data_PKN_SARIMA_train_daily_s_weekly, actual_value_PKN_SARIMA_weekly)
  data_PKO_SARIMA_train_weekly <- c(data_PKO_SARIMA_train_weekly, actual_value_PKO_SARIMA_weekly)
  
  model_PGE_SARIMA_weekly <- Arima(data_PGE_SARIMA_train_weekly, order = order_PGE, seasonal=seasonal_order_PGE)
  model_PKN_SARIMA_weekly <- Arima(data_PKN_SARIMA_train_daily_s_weekly, order = order_PKN, seasonal=seasonal_order_PKN)
  model_PKO_SARIMA_weekly <- Arima(data_PKO_SARIMA_train_weekly, order = order_PKO, seasonal=seasonal_order_PKO)
  
  forecast_values_SARIMA_PGE_weekly <- c(forecast_values_SARIMA_PGE_weekly, forecast_value_PGE_SARIMA_weekly)
  forecast_values_SARIMA_PKN_weekly <- c(forecast_values_SARIMA_PKN_weekly, forecast_value_PKN_SARIMA_weekly)
  forecast_values_SARIMA_PKO_weekly <- c(forecast_values_SARIMA_PKO_weekly, forecast_value_PKO_SARIMA_weekly)
}


par(mfrow=c(3,1), mar=c(5,5,2,2), oma=c(0,0,2,0), yaxt = "s", las = 1, mgp=c(4,1,0), mai=c(0.7, 0.7, 0.2, 0.1))
#plot(PGE_weekly$Date[1:272], PGE_weekly$daily_turnover[1:272], main="PGE - ARFIMA(1,1,1)", xlab="", ylab="", col="blue", type='l', yaxt = "n")
plot(PGE_weekly$Date[273:length(PGE_weekly$daily_turnover)],  PGE_weekly$daily_turnover[273:length(PGE_weekly$daily_turnover)],main="PGE - SARIMA(1,2,2)", xlab="", ylab="", col="blue", type='l', xaxt = "n", yaxt = "n")
points(PGE_weekly$Date[273:length(PGE_weekly$daily_turnover)], forecast_values_SARIMA_PGE_weekly, col="red", yaxt = "n", xaxt = "n", pch=4)
legend("topleft", legend = c("Dane testowe" ,"Predykcja one-step-ahead"), col = c("blue", "red"), lty=c(1,0), cex = 0.9, pch=c(NA, 4))
axis(side = 2, at = pretty(PGE_weekly$daily_turnover[1:272]), labels = sapply(pretty(PGE_weekly$daily_turnover[1:272]), millions_formatter))
axis.Date(1, at = PGE_weekly$Date, format="%m-%Y", col = "transparent")

#plot(PKN_weekly$Date[1:272],PKN_weekly$daily_turnover[1:272], main="PKN - ARFIMA(1,1,2)", xlab="", ylab="Dzienny obrót spółek", col="blue", type='l', yaxt = "n")
plot(PKN_weekly$Date[273:length(PKN_weekly$daily_turnover)],  PKN_weekly$daily_turnover[273:length(PKN_weekly$daily_turnover)], main="PKN - SARIMA(1,2,2)", xlab="", ylab="tygodniowy obrót spółek", col="blue", type='l', xaxt = "n", yaxt = "n")
points(PKN_weekly$Date[273:length(PKN_weekly$daily_turnover)], forecast_values_SARIMA_PKN_weekly, col="red", yaxt = "n", xaxt = "n", pch=4)
legend("topleft", legend = c("Dane testowe" ,"Predykcja one-step-ahead"), col = c("blue", "red"), lty=c(1,0), cex = 0.9, pch=c(NA, 4))
axis(side = 2, at = pretty(PKN_weekly$daily_turnover[1:272]), labels = sapply(pretty(PKN_weekly$daily_turnover[1:272]), millions_formatter))
axis.Date(1, at = PKN_weekly$Date, format="%m-%Y", col = "transparent")

#plot(PKO_weekly$Date[1:272],  PKO_weekly$daily_turnover[1:272], main="PKO - ARFIMA(2,1,2)", xlab = "data", ylab="", col="blue", type='l', yaxt = "n")
plot(PKO_weekly$Date[273:length(PKO_weekly$daily_turnover)], PKO_weekly$daily_turnover[273:length(PKO_weekly$daily_turnover)], main="PKO - SARIMA(1,2,2)", xlab = "data", ylab="", col="blue", type='l', yaxt = "n", xaxt = "n")
points(PKO_weekly$Date[273:length(PKO_weekly$daily_turnover)], forecast_values_SARIMA_PKO_weekly, col="red", yaxt = "n", xaxt = "n", pch=4)
legend("topleft", legend = c("Dane testowe" ,"Predykcja one-step-ahead"), col = c("blue", "red"), lty=c(1,0), cex = 0.9, pch=c(NA, 4))
axis(side = 2, at = pretty(PKO_weekly$daily_turnover[1:272]), labels = sapply(pretty(PKO_weekly$daily_turnover[1:272]), millions_formatter))
axis.Date(1, at = PKO_weekly$Date, format="%m-%Y", col = "transparent")

mtext("Predykcja one-step-ahead za pomocą szeregu SARIMA - dane tygodniowe", side=3, line=0.7, outer=TRUE)


#######################################
##########DANE DWUTYGODNIOWE###########
#######################################


forecast_values_SARIMA_PGE_weekly2 <- numeric()
forecast_values_SARIMA_PKN_weekly2 <- numeric()
forecast_values_SARIMA_PKO_weekly2 <- numeric()

# Parametry modelu SARIMA
order_PGE_weekly2 <- c(1, 1, 1)
seasonal_order_PGE_weekly2 <- c(1, 1, 1, 15)

order_PKN_weekly2 <- c(1, 1, 2)
seasonal_order_PKN_weekly2 <- c(1, 1, 2, 15)

order_PKO_weekly2 <- c(2, 1, 2)
seasonal_order_PKO_weekly2 <- c(2, 1, 2, 15)

forecast_steps_SARIMA_PGE_weekly2 <- length(df_test_PGE_weekly2)

data_PGE_SARIMA_train_weekly2 <- PGE_weekly2$daily_turnover[1:136]
data_PKN_SARIMA_train_daily_s_weekly2 <- PKN_weekly2$daily_turnover[1:136]
data_PKO_SARIMA_train_weekly2 <- PKO_weekly2$daily_turnover[1:136]
df_test_PGE_weekly2 <- PGE_weekly2$daily_turnover[137:length(PGE_weekly2$daily_turnover)]
df_test_PKN_daily_s_weekly2 <- PKN_weekly2$daily_turnover[137:length(PKN_weekly2$daily_turnover)]
df_test_PKO_weekly2 <- PKO_weekly2$daily_turnover[137:length(PKO_weekly2$daily_turnover)]

model_PGE_SARIMA_weekly2 <- Arima(data_PGE_SARIMA_train_weekly2, order = order_PGE, seasonal=seasonal_order_PGE)
model_PKN_SARIMA_weekly2 <- Arima(data_PKN_SARIMA_train_daily_s_weekly2, order = order_PKN, seasonal=seasonal_order_PKN)
model_PKO_SARIMA_weekly2 <- Arima(data_PKO_SARIMA_train_weekly2, order = order_PKO, seasonal=seasonal_order_PKO)

# Pętla prognozowania dla każdego kroku
for (i in 1:forecast_steps_SARIMA_PGE_weekly2) {
  
  forecast_SARIMA_PGE_weekly2 <- forecast(model_PGE_SARIMA_weekly2, h = 1)
  forecast_SARIMA_PKN_weekly2 <- forecast(model_PKN_SARIMA_weekly2, h = 1)
  forecast_SARIMA_PKO_weekly2 <- forecast(model_PKO_SARIMA_weekly2, h = 1)
  
  forecast_value_PGE_SARIMA_weekly2 <- forecast_SARIMA_PGE_weekly2$mean[1]
  forecast_value_PKN_SARIMA_weekly2 <- forecast_SARIMA_PKN_weekly2$mean[1]
  forecast_value_PKO_SARIMA_weekly2 <- forecast_SARIMA_PKO_weekly2$mean[1]
  
  actual_value_PGE_SARIMA_weekly2 <- df_test_PGE_weekly2[i]
  actual_value_PKN_SARIMA_weekly2 <- df_test_PKN_daily_s_weekly2[i]
  actual_value_PKO_SARIMA_weekly2 <- df_test_PKO_weekly2[i]
  
  data_PGE_SARIMA_train_weekly2 <- c(data_PGE_SARIMA_train_weekly2, actual_value_PGE_SARIMA_weekly2)
  data_PKN_SARIMA_train_daily_s_weekly2 <- c(data_PKN_SARIMA_train_daily_s_weekly2, actual_value_PKN_SARIMA_weekly2)
  data_PKO_SARIMA_train_weekly2 <- c(data_PKO_SARIMA_train_weekly2, actual_value_PKO_SARIMA_weekly2)
  
  model_PGE_SARIMA_weekly2 <- Arima(data_PGE_SARIMA_train_weekly2, order = order_PGE, seasonal=seasonal_order_PGE)
  model_PKN_SARIMA_weekly2 <- Arima(data_PKN_SARIMA_train_daily_s_weekly2, order = order_PKN, seasonal=seasonal_order_PKN)
  model_PKO_SARIMA_weekly2 <- Arima(data_PKO_SARIMA_train_weekly2, order = order_PKO, seasonal=seasonal_order_PKO)
  
  forecast_values_SARIMA_PGE_weekly2 <- c(forecast_values_SARIMA_PGE_weekly2, forecast_value_PGE_SARIMA_weekly2)
  forecast_values_SARIMA_PKN_weekly2 <- c(forecast_values_SARIMA_PKN_weekly2, forecast_value_PKN_SARIMA_weekly2)
  forecast_values_SARIMA_PKO_weekly2 <- c(forecast_values_SARIMA_PKO_weekly2, forecast_value_PKO_SARIMA_weekly2)
}

par(mfrow=c(3,1), mar=c(5,5,2,2), oma=c(0,0,2,0), yaxt = "s", las = 1, mgp=c(4,1,0))
#plot(PGE_weekly2$Date[1:136], PGE_weekly2$daily_turnover[1:136], main="PGE - ARFIMA(1,1,1)", xlab="", ylab="", col="blue", type='l', yaxt = "n")
plot(PGE_weekly2$Date[137:length(PGE_weekly2$daily_turnover)],  PGE_weekly2$daily_turnover[137:length(PGE_weekly2$daily_turnover)], main="PGE - SARIMA(1,1,1)", xlab="", ylab="", col="blue", type='l', xaxt = "n", yaxt = "n")
points(PGE_weekly2$Date[137:length(PGE_weekly2$daily_turnover)], forecast_values_SARIMA_PGE_weekly2, col="red", yaxt = "n", xaxt = "n", pch=4)
legend("topleft", legend = c("Dane testowe" ,"Predykcja one-step-ahead"), col = c("blue", "red"), lty=c(1,0), cex = 0.9, pch=c(NA, 4))
axis(side = 2, at = pretty(PGE_weekly2$daily_turnover[1:136]), labels = sapply(pretty(PGE_weekly2$daily_turnover[1:136]), millions_formatter))
axis.Date(1, at = PGE_weekly2$Date, format="%m-%Y", col = "transparent")


#plot(PKN_weekly2$Date[1:136],PKN_weekly2$daily_turnover[1:136], main="PKN - ARFIMA(1,1,2)", xlab="", ylab="Dzienny obrót spółek", col="blue", type='l', yaxt = "n")
plot(PKN_weekly2$Date[137:length(PKN_weekly2$daily_turnover)],  PKN_weekly2$daily_turnover[137:length(PKN_weekly2$daily_turnover)], main="PKN - SARIMA(1,1,2)", xlab="", ylab="dwutygodniowy obrót spółek", col="blue", type='l', xaxt = "n", yaxt = "n")
points(PKN_weekly2$Date[137:length(PKN_weekly2$daily_turnover)], forecast_values_SARIMA_PKN_weekly2, col="red",xaxt = "n", yaxt = "n", pch=4)
legend("topleft", legend = c("Dane testowe" ,"Predykcja one-step-ahead"), col = c("blue", "red"), lty=c(1,0), cex = 0.9, pch=c(NA, 4))
axis(side = 2, at = pretty(PKN_weekly2$daily_turnover[1:136]), labels = sapply(pretty(PKN_weekly2$daily_turnover[1:136]), millions_formatter))
axis.Date(1, at = PKN_weekly2$Date, format="%m-%Y", col = "transparent")


#plot(PKO_weekly2$Date[1:136],  PKO_weekly2$daily_turnover[1:136], main="PKO - ARFIMA(2,1,2)", xlab = "data", ylab="", col="blue", type='l', yaxt = "n")
plot(PKO_weekly2$Date[137:length(PKO_weekly2$daily_turnover)], PKO_weekly2$daily_turnover[137:length(PKO_weekly2$daily_turnover)], main="PKO - SARIMA(2,1,2)", xlab = "data", ylab="", col="blue", type='l', yaxt = "n", xaxt = "n")
points(PKO_weekly2$Date[137:length(PKO_weekly2$daily_turnover)], forecast_values_SARIMA_PKO_weekly2, col="red", xaxt="n", yaxt = "n", pch=4)
legend("topleft", legend = c("Dane testowe" ,"Predykcja one-step-ahead"), col = c("blue", "red"), lty=c(1,0), cex = 0.9, pch=c(NA, 4))
axis(side = 2, at = pretty(PKO_weekly2$daily_turnover[1:136]), labels = sapply(pretty(PKO_weekly2$daily_turnover[1:136]), millions_formatter))
axis.Date(1, at = PKO_weekly2$Date, format="%m-%Y", col = "transparent")


mtext("Predykcja one-step-ahead za pomocą szeregu SARIMA - dane dwutygodniowe", side=3, line=0.7, outer=TRUE) 



# Błędy predykcji

##################################################
#################DANE DZIENNE#####################
##################################################

PGE_pred <- forecast_values_SARIMA_PGE
PGE_true <- df_test_PGE_daily

#Mean Absolute Percentage Error (MAPE)
mape_PGE_SARIMA_daily <- mean(abs(PGE_pred - PGE_true) / abs(PGE_true), na.rm = TRUE)

#Mean Absolute Error (MAE)
mae_PGE_SARIMA_daily <- mean(abs(PGE_pred - PGE_true), na.rm = TRUE)

#Mean Percentage Error (MPE)
mpe_PGE_SARIMA_daily <- mean((PGE_pred - PGE_true) / PGE_true, na.rm = TRUE)

#Root Mean Squared Error (RMSE)
rmse_PGE_SARIMA_daily <- sqrt(mean((PGE_pred - PGE_true)^2, na.rm = TRUE))

# Display the results
cat("MAPE: ", mape_PGE_SARIMA_daily, "\n")
cat("MAE: ", mae_PGE_SARIMA_daily, "\n")
cat("MPE: ", mpe_PGE_SARIMA_daily, "\n")
cat("RMSE: ", rmse_PGE_SARIMA_daily, "\n")


PKN_pred <- forecast_values_SARIMA_PKN
PKN_true <- df_test_PKN_daily

#Mean Absolute Percentage Error (MAPE)
mape_PKN_SARIMA_daily <- mean(abs(PKN_pred - PKN_true) / abs(PKN_true), na.rm = TRUE)

#Mean Absolute Error (MAE)
mae_PKN_SARIMA_daily <- mean(abs(PKN_pred - PKN_true), na.rm = TRUE)

#Mean Percentage Error (MPE)
mpe_PKN_SARIMA_daily <- mean((PKN_pred - PKN_true) / PKN_true, na.rm = TRUE)

#Root Mean Squared Error (RMSE)
rmse_PKN_SARIMA_daily <- sqrt(mean((PKN_pred - PKN_true)^2, na.rm = TRUE))

# Display the results
cat("MAPE: ", mape_PKN_SARIMA_daily, "\n")
cat("MAE: ", mae_PKN_SARIMA_daily, "\n")
cat("MPE: ", mpe_PKN_SARIMA_daily, "\n")
cat("RMSE: ", rmse_PKN_SARIMA_daily, "\n")


PKO_pred <- forecast_values_SARIMA_PKO
PKO_true <- df_test_PKO_daily

#Mean Absolute Percentage Error (MAPE)
mape_PKO_SARIMA_daily <- mean(abs(PKO_pred - PKO_true) / abs(PKO_true), na.rm = TRUE)

#Mean Absolute Error (MAE)
mae_PKO_SARIMA_daily <- mean(abs(PKO_pred - PKO_true), na.rm = TRUE)

#Mean Percentage Error (MPE)
mpe_PKO_SARIMA_daily <- mean((PKO_pred - PKO_true) / PKO_true, na.rm = TRUE)

#Root Mean Squared Error (RMSE)
rmse_PKO_SARIMA_daily <- sqrt(mean((PKO_pred - PKO_true)^2, na.rm = TRUE))

# Display the results
cat("MAPE: ", mape_PKO_SARIMA_daily, "\n")
cat("MAE: ", mae_PKO_SARIMA_daily, "\n")
cat("MPE: ", mpe_PKO_SARIMA_daily, "\n")
cat("RMSE: ", rmse_PKO_SARIMA_daily, "\n")


##################################################
#################DANE TYGODNIOWE##################
##################################################

PGE_pred_w <- forecast_values_SARIMA_PGE_weekly
PGE_true_w <- df_test_PGE_weekly
#Mean Absolute Percentage Error (MAPE)
mape_PGE_SARIMA_week <- mean(abs(PGE_pred_w - PGE_true_w) / abs(PGE_true_w), na.rm = TRUE)

#Mean Absolute Error (MAE)
mae_PGE_SARIMA_week <- mean(abs(PGE_pred_w - PGE_true_w), na.rm = TRUE)

#Mean Percentage Error (MPE)
mpe_PGE_SARIMA_week <- mean((PGE_pred_w - PGE_true_w) / PGE_true_w, na.rm = TRUE)

#Root Mean Squared Error (RMSE)
rmse_PGE_SARIMA_week <- sqrt(mean((PGE_pred_w - PGE_true_w)^2, na.rm = TRUE))

# Display the results
cat("MAPE: ", mape_PGE_SARIMA_week, "\n")
cat("MAE: ", mae_PGE_SARIMA_week, "\n")
cat("MPE: ", mpe_PGE_SARIMA_week, "\n")
cat("RMSE: ", rmse_PGE_SARIMA_week, "\n")


PKN_pred_w <- forecast_values_SARIMA_PKN_weekly
PKN_true_w <- df_test_PKN_weekly
#Mean Absolute Percentage Error (MAPE)
mape_PKN_SARIMA_week <- mean(abs(PKN_pred_w - PKN_true_w) / abs(PKN_true_w), na.rm = TRUE)

#Mean Absolute Error (MAE)
mae_PKN_SARIMA_week <- mean(abs(PKN_pred_w - PKN_true_w), na.rm = TRUE)

#Mean Percentage Error (MPE)
mpe_PKN_SARIMA_week <- mean((PKN_pred_w - PKN_true_w) / PKN_true_w, na.rm = TRUE)

#Root Mean Squared Error (RMSE)
rmse_PKN_SARIMA_week <- sqrt(mean((PKN_pred_w - PKN_true_w)^2, na.rm = TRUE))

# Display the results
cat("MAPE: ", mape_PKN_SARIMA_week, "\n")
cat("MAE: ", mae_PKN_SARIMA_week, "\n")
cat("MPE: ", mpe_PKN_SARIMA_week, "\n")
cat("RMSE: ", rmse_PKN_SARIMA_week, "\n")


PKO_pred_w <- forecast_values_SARIMA_PKO_weekly
PKO_true_w <- df_test_PKO_weekly
#Mean Absolute Percentage Error (MAPE)
mape_PKO_SARIMA_week <- mean(abs(PKO_pred_w - PKO_true_w) / abs(PKO_true_w), na.rm = TRUE)

#Mean Absolute Error (MAE)
mae_PKO_SARIMA_week <- mean(abs(PKO_pred_w - PKO_true_w), na.rm = TRUE)

#Mean Percentage Error (MPE)
mpe_PKO_SARIMA_week <- mean((PKO_pred_w - PKO_true_w) / PKO_true_w, na.rm = TRUE)

#Root Mean Squared Error (RMSE)
rmse_PKO_SARIMA_week <- sqrt(mean((PKO_pred_w - PKO_true_w)^2, na.rm = TRUE))

# Display the results
cat("MAPE: ", mape_PKO_SARIMA_week, "\n")
cat("MAE: ", mae_PKO_SARIMA_week, "\n")
cat("MPE: ", mpe_PKO_SARIMA_week, "\n")
cat("RMSE: ", rmse_PKO_SARIMA_week, "\n")


##################################################
#################DANE DWUTYGODNIOWE###############
##################################################


PGE_pred_w2 <- forecast_values_SARIMA_PGE_weekly2
PGE_true_w2 <- df_test_PGE_weekly2
#Mean Absolute Percentage Error (MAPE)
mape_PGE_SARIMA_week2 <- mean(abs(PGE_pred_w2 - PGE_true_w2) / abs(PGE_true_w2), na.rm = TRUE)

#Mean Absolute Error (MAE)
mae_PGE_SARIMA_week2 <- mean(abs(PGE_pred_w2 - PGE_true_w2), na.rm = TRUE)

#Mean Percentage Error (MPE)
mpe_PGE_SARIMA_week2 <- mean((PGE_pred_w2 - PGE_true_w2) / PGE_true_w2, na.rm = TRUE)

#Root Mean Squared Error (RMSE)
rmse_PGE_SARIMA_week2 <- sqrt(mean((PGE_pred_w2 - PGE_true_w2)^2, na.rm = TRUE))

# Display the results
cat("MAPE: ", mape_PGE_SARIMA_week2, "\n")
cat("MAE: ", mae_PGE_SARIMA_week2, "\n")
cat("MPE: ", mpe_PGE_SARIMA_week2, "\n")
cat("RMSE: ", rmse_PGE_SARIMA_week2, "\n")


PKN_pred_w2 <- forecast_values_SARIMA_PKN_weekly2
PKN_true_w2 <- df_test_PKN_weekly2

#Mean Absolute Percentage Error (MAPE)
mape_PKN_SARIMA_week2 <- mean(abs(PKN_pred_w2 - PKN_true_w2) / abs(PKN_true_w2), na.rm = TRUE)

#Mean Absolute Error (MAE)
mae_PKN_SARIMA_week2 <- mean(abs(PKN_pred_w2 - PKN_true_w2), na.rm = TRUE)

#Mean Percentage Error (MPE)
mpe_PKN_SARIMA_week2 <- mean((PKN_pred_w2 - PKN_true_w2) / PKN_true_w2, na.rm = TRUE)

#Root Mean Squared Error (RMSE)
rmse_PKN_SARIMA_week2 <- sqrt(mean((PKN_pred_w2 - PKN_true_w2)^2, na.rm = TRUE))

# Display the results
cat("MAPE: ", mape_PKN_SARIMA_week2, "\n")
cat("MAE: ", mae_PKN_SARIMA_week2, "\n")
cat("MPE: ", mpe_PKN_SARIMA_week2, "\n")
cat("RMSE: ", rmse_PKN_SARIMA_week2, "\n")


PKO_pred_w2 <- forecast_values_SARIMA_PKO_weekly2
PKO_true_w2 <- df_test_PKO_weekly2
#Mean Absolute Percentage Error (MAPE)
mape_PKO_SARIMA_week2 <- mean(abs(PKO_pred_w2 - PKO_true_w2) / abs(PKO_true_w2), na.rm = TRUE)

#Mean Absolute Error (MAE)
mae_PKO_SARIMA_week2 <- mean(abs(PKO_pred_w2 - PKO_true_w2), na.rm = TRUE)

#Mean Percentage Error (MPE)
mpe_PKO_SARIMA_week2 <- mean((PKO_pred_w2 - PKO_true_w2) / PKO_true_w2, na.rm = TRUE)

#Root Mean Squared Error (RMSE)
rmse_PKO_SARIMA_week2 <- sqrt(mean((PKO_pred_w2 - PKO_true_w2)^2, na.rm = TRUE))

# Display the results
cat("MAPE: ", mape_PKO_SARIMA_week2, "\n")
cat("MAE: ", mae_PKO_SARIMA_week2, "\n")
cat("MPE: ", mpe_PKO_SARIMA_week2, "\n")
cat("RMSE: ", rmse_PKO_SARIMA_week2, "\n")
