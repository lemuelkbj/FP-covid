#installling packages
install.packages("hrbrthemes")
install.packages("tseries")
install.packages("forecast")

#callingpackages
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(tseries)
library(forecast)
library(data.table)

#Preprocessing 

#data reduced to date and confirmed cases
conf=data.frame(covid_19_india$Date,covid_19_india$Confirmed)


#data also reduced to date ,deaths and confirmed for futher use
selectd=data.frame(covid_19_india$Date,covid_19_india$Deaths,
                   covid_19_india$Cured,covid_19_india$Confirmed)#this data not used


#grouping of date with respect to date accordingly to dataset
dt = as.data.table(conf)#conf is used ,predicting only confirmed cases
dt
df=dt[, lapply(.SD, sum), by = conf$covid_19_india.Date]
View(df)#data grouped with respect to date for confirmed cases only

#converting to time series data
#######BOX JENKINs##########
#converting to time series object
tsdata=ts(df$covid_19_india.Confirmed)
ts.plot(tsdata)
adf.test(tsdata)

######################################################
#p value is greater and D-F value is insignificant   #
#so data is not stationary                           #
#so do differentiation(differences=2)                #
######################################################

#making data stationary
tsdata1 <- diff(tsdata, differences = 2)
adf.test(tsdata1)

#################################################
#pvalue is smaller and D-F value is significant #
#so data is stationary                          #
#acf and pacf                                   #
#################################################

#Plotting the ACF plot and PACF
acf(tsdata1,main="ACF")
pacf(tsdata1,main="PCF")
tsdata1 %>% diff() %>% ggtsdisplay(main="")


#################################################
#ARIMA MODEL                                    #
#################################################


#Building model#########################################
#syntax for auto.arima
#fit <- auto.arima(tsdata, seasonal=FALSE,
#                   stepwise=FALSE, approximation=FALSE))
#########################################################

#auto arima on tsdata
auto_tsdata_model=auto.arima(tsdata,seasonal = FALSE,stepwise=FALSE, approximation=FALSE)
tsdisplay(residuals(mymodel),lag.max = 15,main="")

########################################################
#ARIMA(1,2,3) on tsdata
fit=arima(tsdata,c(1,2,3))
tsdisplay(residuals(fit),lag.max = 15,main="arima(1,2,3)")
fo=forecast(fit)
plot(fo)
#auto ARIMA on tsdata1
model_tsdata1=auto.arima(tsdata1,seasonal=FALSE)
tsdisplay(residuals(model_tsdata1),lag.max = 15,
          main="arima 0,0,2 with non-zero mean")

#ARIMA(1,2,3) on tsdata1
custom_model_tsdata1=arima(tsdata1,c(1,2,3))
tsdisplay(residuals(custom_model_tsdata1),lag.max = 15,main = "ar")

################################
#########FORECAST###############
################################

#plot forecast
par(mfrow=c(2,2))
#forecast for tsdata

#forecast auto on tsdata
tsdata_auto_model=forecast(auto_tsdata_model,h=30)
tsdata_auto_model
plot(tsdata_auto_model)

#forecast for tsdata1
forecast_custom_model=forecast(custom_model_tsdata1,h=10)
forecast_custom_model
plot(forecast_custom_model)
#forecast auto on tsdata1
forecast_auto_model=forecast(model_tsdata1,h=30)
forecast_auto_model
plot(forecast_auto_model)

#validate using box.test
Box.test(residuals(fit),lag=5,type="Ljung-Box")
Box.test(residuals(fit),lag=10,type="Ljung-Box")
