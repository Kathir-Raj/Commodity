require(timeSeries)
require(TTR)
require(forecast)
require(stats)

setwd("C:/Data/R_Work/commodity/")
model=read.csv("C:/Data/R_Work/commodity/gold.csv")
modelname="gold"


model$Month=as.Date(model$Month,format="%d-%m-%y")
model$Price=as.numeric(model$Price)
model.TS=ts(model$Price, frequency=12, start=c(1997,10))
plot.ts(model.TS)
model.DE=decompose(model.TS)
plot(model.DE)

cdate=as.Date("31-10-2016",format="%d-%m-%Y")
model_train=subset(model,model$Month<cdate)
model_test=subset(model,model$Month>cdate)

model_trn=ts(model_train$Price, frequency=12, start=c(1997,10), end=c(2016,10))
model_tst=ts(model_test$Price, frequency=12, start=c(2016,11), end=c(2017,10))
model_trn
model_tst

#ARIMA Model
auto.arima(model_tst)
ARIMA_Train=auto.arima(model_trn)
pred_rprice = forecast::forecast(ARIMA_Train, h=12)
acc.arima=accuracy(model_tst,pred_rprice$mean)

plot(model.TS)
plot(pred_rprice)
pred_rprice
comp=cbind(model_test$Price,pred_rprice$upper)
comp

#TbATS Model
price.tbats = tbats(model_trn)
price.fs.tbats = forecast(price.tbats, h=12)
acc.tb=accuracy(price.fs.tbats,model_tst)

#NNETS Model
price.nn = nnetar(model_trn)
price.fs.nn = forecast(price.nn, h=12)
acc.nn=accuracy(price.fs.nn,model_tst)

# Accuracy summary
acc.arima
acc.tb
acc.nn

#implementing TBATS model for full model

TBATS_Full = tbats(model.TS)
Forecast_12M_TB = forecast::forecast(TBATS_Full, h=12 )
Forecast_12M_TB$mean
plot(Forecast_12M_TB,xlab="Monthwise",ylab=paste(modelname,"price in Rs/unit "))

nn_Full = nnetar(model.TS)
Forecast_12M_nn = forecast::forecast(nn_Full, h=12 )
Forecast_12M_nn
plot(Forecast_12M_nn,xlab="Monthwise",ylab=paste(modelname,"price in Rs/unit "))






