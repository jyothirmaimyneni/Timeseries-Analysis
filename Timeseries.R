# Load the necessary libraries
install.packages(c("forecast", "ggplot2", "tseries"))
library(forecast)
library(ggplot2)
library(ggplot2)
library(tseries)

# Load the data
monthly_data <- read.csv('nitm18442004 Aug 2023.csv')
yearly_data <- read.csv('nity18442004 Aug 2023.csv')

# Convert the data to time series objects
monthly_ts <- ts(monthly_data[, 1], start=1844, frequency=12)
yearly_ts <- ts(yearly_data[, 1], start=1844)

# Visualize the Data//Figures. 13,14
autoplot(monthly_ts) + ggtitle("Monthly Data")
autoplot(yearly_ts) + ggtitle("Yearly Data")



############
#EXPONENTIAL SMOOTHING

# Monthly//Figure.15
monthly_ts <- ts(monthly_data[,1], start=1844, frequency=12) 
fit_monthly_es <- ets(monthly_ts[1:(length(monthly_ts)-12)])
forecast_monthly_es <- forecast(fit_monthly_es, h=12)
autoplot(forecast_monthly_es)

# Yearly//Figure.16
yearly_ts <- ts(yearly_data[,1], start=1844)
fit_yearly_es <- ets(yearly_ts[1:(length(yearly_ts)-1)])
forecast_yearly_es <- forecast(fit_yearly_es, h=1)
autoplot(forecast_yearly_es)


##################ARIMA/SARIMA
# Monthly//Figure.17
fit_monthly_arima <- auto.arima(monthly_ts[1:(length(monthly_ts)-12)])
forecast_monthly_arima <- forecast(fit_monthly_arima, h=12)
autoplot(forecast_monthly_arima)

# Yearly//Figue.18
fit_yearly_arima <- auto.arima(yearly_ts[1:(length(yearly_ts)-1)])
forecast_yearly_arima <- forecast(fit_yearly_arima, h=1)
autoplot(forecast_yearly_arima)

#############
#Simple Time series models(Naive model for illustration)
# Monthly//Figure.19
fit_monthly_naive <- naive(monthly_ts[1:(length(monthly_ts)-12)], h=12)
autoplot(fit_monthly_naive)

# Yearly//Figure.20
fit_yearly_naive <- naive(yearly_ts[1:(length(yearly_ts)-1)], h=1)
autoplot(fit_yearly_naive)

#####################
#Evaluations
# Evaluate the Forecasts for Monthly Data
accuracy(forecast_monthly_es, monthly_ts[(length(monthly_ts)-11):length(monthly_ts)])
accuracy(forecast_monthly_arima, monthly_ts[(length(monthly_ts)-11):length(monthly_ts)])
accuracy(fit_monthly_naive, monthly_ts[(length(monthly_ts)-11):length(monthly_ts)])

# Evaluate the Forecasts for Yearly Data
accuracy(forecast_yearly_es, yearly_ts[length(yearly_ts)])
accuracy(forecast_yearly_arima, yearly_ts[length(yearly_ts)])
accuracy(fit_yearly_naive, yearly_ts[length(yearly_ts)])






