rm(list = ls())
lydiam = read.csv('Lydiam2.txt', header = T)
names(lydiam)
head(lydiam)
attach(lydiam)
#plot to see is need to log 
msales.ts = ts(lydiam[,6], start=c(1949,1), freq = 12) 
plot(msales.ts)
fmonth = factor(lydiam$month)
model1 = lm(msales ~ fmonth + madv + madvl1 + madvl2 + msalesl1 + dec57 + jan58 + c348 + s348, data = lydiam)
summary(model1)

#seasonal answer
b1 = coef(model1)[1]
b2 = coef(model1)[2:12] + b1
b3 = c(b1, b2)
s0 = b3 - mean(b3)
b4 = coef(model1)[16]
s = rep(0,12)
s1 = c(s0, s0, s0)
for(i in 1:12){
  s[i]<-s1[i]
  for(j in 1:16){ 
    jr<-i-j+24
    s[i]<-s[i]+(b4^j)*s1[jr]
  }
  }
s
s.ts = ts(s)
plot(s.ts, ylab = "seaonal indices", xlab = "month")
#residual of the model
qqnorm(resid(model1))
plot(ts(resid(model1)))
acf(ts(resid(model1)))
pacf(ts(resid(model1)))
resids1 = resid(model1)
spectrum(resids1, span = 13)
install.packages("hwwntest")
library("hwwntest")
bartlettB.test(resid(model1))
names(lydiam)
length(lydiam$msales)

1.36/sqrt(132/2)

#90 percent interval code
deltapartial = delta = c(rep(0, times = 500))
delta[1] = 9.244e-02 + (6.133e-01*1.085e-01)
delta[2] = 1.085e-01 + (6.133e-01 * 0.158983)
delta[3] = 6.133e-01*0.2060043
deltapartial[1] = delta[1]
deltapartial[2] = deltapartial[1] + delta[2]
deltapartial[3] = deltapartial[1] + deltapartial[2] + delta[3]
for (j in 4:500){
  j1 = j -1
  delta[j] = 6.133e-01 * delta[j1]
  deltapartial[j] = deltapartial[j1] + delta[j]
}
deltapartial[500]*0.9
deltapartial[1:20]

#granger causality 
names(lydiam)
model2 = lm(madv ~ fmonth + msales 
            + msalesl1 + c348 + s348, data = lydiam)
summary(model2)




#second pset
djia = read.csv('djia.txt', header = T)
attach(djia)
head(djia)
djia.ts = ts(logReturn, start = c(1971,7), freq = 52) #is this accurate?
plot(djia.ts)
#accoriding to this plot, im dividing it up to 1972w47
first.half = djia[1:73, "logReturn"]
first.half = na.omit(first.half)
second.half = djia[74:163,"logReturn"]
second.half = na.omit(second.half)
firsthalf.ts = ts(first.half)
secondhalf.ts = ts(second.half)
plot(firsthalf.ts, main = "first")
plot(secondhalf.ts, main = "second")
acf(firsthalf.ts)
pacf(firsthalf.ts)


first.ar1 = arima(firsthalf.ts, order = c(1,0,0))
first.ar1 #aic=-392.13

first.ma1 = arima(firsthalf.ts, order = c(0,0,1))
first.ma1 #-391.7

#t ratio
install.packages("lmtest")
library("lmtest")
coeftest(first.ar1)
coeftest(first.ma1)

#spectrum density
resids1 = resid(first.ar1)
spectrum(resids1, span = 8, main = "ar1")
resids2 = resid(first.ma1)
spectrum(resids2, span = 8, main = "ma1")


compare.aic = cbind(AIC(first.ar1), AIC(first.ma1))
colnames(compare.aic) = c("first.ar1", "first.ma1")
compare.aic
#AIC is showing me ar1 is the best
#need to look at other criterias

#with seasonal 
acf(secondhalf.ts)
pacf(secondhalf.ts)
secondhalf.seas.ts = ts(second.half, freq = 3)
model.second<-arima(secondhalf.ts,order=c(0,0,0),seasonal=c(3,0,0))
model.second
list.lag = c(0,0,0.1418,0,0,0.0339,0,0,-0.0506)
zeros = polyroot(list.lag)
zeros
cycle1 = 2*pi/Arg(zeros)[3]
cycle2 = 2*pi/Arg(zeros)[6]
cycle3= 2*pi/Arg(zeros)[8]
cbind(cycle1, cycle2, cycle3)

cycle = cycle3= 2*pi/Arg(zeros)[3:9]
cycle

