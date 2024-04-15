library(tseries)
library(quantmod)
library(forecast)
mse <- function(y_true, y_pred){
  y_true = as.numeric(y_true)
  y_pred = as.numeric(y_pred)
  mse = mean((y_true - y_pred)^2)
  return(mse)
}

# Load data
btc = get.hist.quote(instrument = "BTC-USD", "2018-01-01", "2023-12-31",
                     quote = c("Close"),
                     provider = c("yahoo"))

plot(btc$Close, type = "l")

# train-test split
train_last_day = "2022-12-31"
train = btc[index(btc) <= train_last_day,]
test = btc[index(btc) > train_last_day,]

# Tune ARIMA order (p,d,q) and train best ARIMA model
(best_model<-auto.arima(train,max.p=5,max.q=5,max.d=5,seasonal=FALSE,ic='aic',allowdrift=FALSE))
best_model = arima(train, order = c(2,1,5))
best_model

# Ljung-Box test - Desired is HO not rejected, i.e. p > 0.05
orders = arimaorder(best_model)
Box.test(best_model$residuals, lag = 10, type = "Ljung-Box", fitdf = orders[1] + orders[3])
orders

# Prediction and Evaluation
hist_data <- tail(train,30)
arima_pred <- array(NA,dim=length(test))
model_coef = best_model$coef
for(i in 1:length(test)){
  #update the model with old estimates of ARIMA parameters
  best_model <- Arima(hist_data, model = best_model)
  model_coef = c(model_coef, best_model$coef)
  arima_pred[i] <- predict(best_model,n.ahead=1)$pred[1]
  hist_data <- c(hist_data,test[i])
}

# Plot of prediction
plot(as.numeric(test), type = "l", ylab = "Price")
lines(arima_pred, col = "red")

# Closer look of the plot (performance is actually bad) 
plot(as.numeric(test)[1:50], type = "l", ylab = "Price")
lines(arima_pred, col = "red")

mse(test, c(tail(train,1), test[-length(test)])) # naive prediction
mse(test, arima_pred) # arima prediction (very little improvement or even worse)
