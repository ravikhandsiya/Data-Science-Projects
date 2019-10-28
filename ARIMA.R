#install.packages("forecast")
library(forecast)

#ARIMA's feature: 1)Time Series, 2)Seasonality
#Ex-  reservation tickets, hotel bookings, sales
#Abv, ARIMA - Auto Regressve Integrated Moving Average
#P<- denotes for AR part of abv ARIMA
#D<-denotes for Lag or I part of abv ARIMA
#Q<-denotes for MA part of abv ARIMA

data=read.csv('http://ucanalytics.com/blogs/wp-content/uploads/2015/06/Tractor-Sales.csv')
data
data=ts(data[,2],start = c(2003,1),frequency = 12)
data
plot(data,xlab="Years",ylab="Tractor Sales")

#Clearly the above chart has an upward trend for tractors sales and there is
#also a seasonal component on time series composition.

#differencing the series stationary

plot(diff(data), ylab="Difference Tractor Sales")


#log transform data to make data stationary on variance

plot(log10(data), ylab = 'Log(Tractor Sales)')

#Difference log transform data to make data
#stationary on both mean and variance

plot(diff(log10(data)),ylab="Differenced Log(Tractor Sales)")

#Plot ACF and PACF to identify potential AR and MA model

#ACF - autocorrelation factor
#PACF - potential autocorrelation factor

par(mfrow=c(1,2))
acf(ts(diff(log10(data))), main="ACF Tractor Sales")
pacf(ts(diff(log10(data))), main="PACF Tractor Sales")
 #this will give us a chart of residuals

#identification of best fir ARIMA model

ARIMAfit=auto.arima(log10(data), approximation = FALSE, trace = FALSE )
summary(ARIMAfit)

#Forecast sales using the best fit ARIMA model

#The next step is to predict tractor sales for next 3 years i.e.
#for 2015, 2016  and 2017 through the above model
dev.off()

par(mfrow = c(1,1))
pred=predict(ARIMAfit, n.ahead = 36)
pred
plot(data,type='l',xlim=c(2004,2018),ylim=c(1,1600),
     xlab='Year',ylab='Tractor Sales')
lines(10^(pred$pred),col='blue')
lines(10^(pred$pred+2*pred$se),col='orange')
lines(10^(pred$pred-2*pred$se),col='orange')

#plot acf and pacf for residuals of ARIMA model to ensure 
#no more information is left for extraction

#finally, let's create an ACF and PACF plot of the residuals of our best
#fit ARIMA model i.e. ARIMA(0,1,1)(0,1,1)[12]

par(mfrow=c(1,2))
acf(ts(ARIMAfit$residuals),main = 'ACF residual')
pacf(ts(ARIMAfit$residuals), main = 'PACF residual')
