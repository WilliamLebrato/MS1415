library(forecast)
library(tseries)
library(stats)
library(zoo)
source(file = "r_funcs/barma.r")
source(file = "r_funcs/karma.r")
data_set <- scan("./unemployment.txt")
size_set <- length(data_set)

#use 1991/01/01 as arbitrary start date

y_train <- data_set[0:(size_set - 10)]
y_test <- data_set[(1+size_set - 10):size_set]

# Convert to time series object
y_train <- ts(y_train, start = c(1991, 1), frequency = 12)
y_test <- ts(y_test, start = c(2005, 2), frequency = 12)

plot(y_train, type = "l", col = "blue",asp = 6, xlab = "Date", ylab = "Unemployment")

adf_result <- adf.test(y_train)
acf_result <- acf(y_train, plot=TRUE)
pacf_result <- pacf(y_train, plot=TRUE)

ARIMAfit = auto.arima(y_train)
print(summary(ARIMAfit))
checkresiduals(ARIMAfit)
plot(forecast(ARIMAfit))
lines(y_test, type="l", col="red", lwd = 2)
print(mean(ARIMAfit$residuals))