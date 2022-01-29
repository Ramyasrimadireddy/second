library(readr)
library(tseries)
s=read_csv("monthly-sunspots.txt")
View(s)
class(s)
colSums(is.na(s))

head(s)
summary(s)


data=ts(s[,2],start=c(1749,1),end=c(1983,12),frequency=12)
plot(data)
plot(decompose(data))
#mean
abline(reg = lm(data~time(data)))
summary(data)

#trend
plot(aggregate(data,FUN = mean), xlab= "year", ylab= "Sunspots", main = "Sunpots vs year (Original Dataset)" )
#sesonality 
boxplot(data~cycle(data),  xlab= "year", ylab= "Sunspots", main = "Sunpots vs year (Original Dataset)")

#differencing 
#1st degree
q<- diff(data)
plot(aggregate(q,FUN=mean),xlab= "year", ylab= "Sunspots", main = "Sunspots vs year(Original Dataset)")
q
summary(q)
#second degree
d.s<-diff(q)
plot(d.s,  xlab= "year", ylab= "Sunspots", main = "Sunspots vs year")
#summary 
summary(d.s)

adf.test(data)
adf.test(data,alternative = "stationary")
adf.test(q)
adf.test(d.s)


acf(data, main = "Original Seires")
pacf(data, main = "Original Seires")
acf(q , main = "1st differenced Seires")
pacf(q, main = "1st differenced Seires")
acf(d.s , main = "2nd differenced Seires")
pacf(d.s , main = "2nd differenced Seires")



arima(data,order=c(0,0,1))
arima(data,order=c(1,1,1))
arima(data,order=c(1,2,2))
arima(data,order=c(0,1,2))
arima(data,order=c(1,2,1))
arima(data,order=c(1,0,5))#less asi
arima(data,order=c(0,1,1))


library("forecast")
library("MLmetrics")
C <-auto.arima(data)
C 
b <-forecast(C,h=12,level=95)
b
accuracy(b)

#Split into train and test
strain <- ts(data[1:1974],start=c(1749,1),end=c(1913,6), frequency=12 ) 
fit1 <-arima(strain, order = c(2,1,1))
pred1 <- 2.718^predict(fit1,12)$pred
pred1

tst <-s[1975:2820,2]
st<-ts(tst,start = c(1913,7),,end=c(1983,12),frequency = 12)
st

class(st)



library("MLmetrics")
MAPE(st,pred1) 


fit <-arima(data, order = c(2,1,1))
nextpred <-predict(fit,n.ahead = 10*12)
nextpred
plot(forecast(fit,h=10*12), xlab = "year", ylab = "Sunspots", main = "Forecast")

