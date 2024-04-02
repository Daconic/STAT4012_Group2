library(tseries)
library(quantmod)
library(forecast)

# Load data
btc = get.hist.quote(instrument = "BTC-USD", start = "2018-01-01", end = "2023-12-31", 
                     quote = c("Open", "High", "Low", "Close", "Volume"))

plot(btc$Close)

# train-test split
train_last_day = "2022-12-31"
train = btc[index(btc) <= train_last_day]$Close
test = btc[index(btc) > train_last_day]$Close

# Tune ARIMA order (p,d,q) and train best ARIMA model
(best_model<-auto.arima(train,max.p=5,max.q=5,max.d=5,seasonal=FALSE,ic='aic',allowdrift=FALSE))

# Ljung-Box test - Desired is HO not rejected, i.e. p > 0.05
orders = arimaorder(best_model)
Box.test(best_model$residuals, lag = 10, type = "Ljung-Box", fitdf = orders[1] + orders[3])
orders

# Evaluation
hist_data <- tail(train,30)
arima_pred <- array(NA,dim=length(test))
for(i in 1:length(test)){
  #update the model with old estimates of ARIMA parameters
  best_model <- Arima(hist_data, model = best_model)
  arima_pred[i] <- predict(best_model,n.ahead=1)$pred[1]
  hist_data <- c(hist_data,test[i])
}
sqrt(mean((arima_pred - test)^2))
plot(as.numeric(test)[1:10], type = "l")
lines(arima_pred, col = "red")
