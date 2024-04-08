library(tseries)
library(quantmod)
library(forecast)
rmse <- function(y_true, y_pred){
  y_true = as.numeric(y_true)
  y_pred = as.numeric(y_pred)
  mse = mean((y_true - y_pred)^2)
  return(sqrt(mse))
}

# Load data
btc = read.csv("btc_minute_20211001_to_20211231.csv")
btc = btc[dim(btc)[1]:1, c("Date", "Close")]
btc_time = strptime(btc$Date, format = "%Y-%m-%d %H:%M:%S")

plot(btc$Close, type = "l")

# train-test split
train_last_day = "2021-12-31"
train = btc[btc_time < train_last_day,]$Close
test = btc[btc_time >= train_last_day,]$Close

# Tune ARIMA order (p,d,q) and train best ARIMA model
(best_model<-auto.arima(train,max.p=5,max.q=5,max.d=5,seasonal=FALSE,ic='bic',allowdrift=FALSE))

# Ljung-Box test - Desired is HO not rejected, i.e. p > 0.05
orders = arimaorder(best_model)
Box.test(best_model$residuals, lag = 10, type = "Ljung-Box", fitdf = orders[1] + orders[3])
orders

# Prediction and Evaluation
hist_data <- tail(train,30)
arima_pred <- array(NA,dim=length(test))
for(i in 1:length(test)){
  #update the model with old estimates of ARIMA parameters
  best_model <- Arima(hist_data, model = best_model)
  arima_pred[i] <- predict(best_model,n.ahead=1)$pred[1]
  hist_data <- c(hist_data,test[i])
}

# Plot of prediction
plot(as.numeric(test), type = "l", ylab = "Price")
lines(arima_pred, col = "red")

# Closer look of the plot (performance is actually bad) 
plot(as.numeric(test)[1:50], type = "l", ylab = "Price")
lines(arima_pred, col = "red")

rmse(test, c(tail(train,1), test[-length(test)])) # naive prediction
rmse(test, arima_pred) # arima prediction (very little improvement or even worse)
