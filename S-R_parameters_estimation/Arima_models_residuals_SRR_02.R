####################################################################
# Arima model to predict the residuals of the SRR
# Ghassen Halouani, Mars 2019
####################################################################

library("forecast")
library("tseries")

########## COD ##########
# 1. data preparation
load("species_data.RData")
COD_data <- sp_data[[2]]

lag <- 1
ssb <- COD_data$SSB
rec <- COD_data$recruitment

ssb <- ssb[-c((length(ssb)-lag + 1):length(ssb)) ]
rec <- rec[-c(1:lag)]

# 2. calculation of the time series of residuals (1971 -2017)
sdev <- 0.8248383
S0.hat <- 78362.54 
R0.hat <- 7937.524
h <- 0.8659396

R <- 0.8*R0.hat*h*ssb / (0.2*S0.hat*(1-h) + ssb*(h-0.2))

# log(R)-(sdev^2)/2 corresponds to the meanlog of the residuals when we use a log normal distribution
resid_multi <- log(rec)-(log(R)-(sdev^2)/2)

# 3. plot of the residuals
plot(resid_multi, type = "b", bty = "l")
abline (h =0, col = "blue", lty = 2)

# 4. ARIMA simulation auto.arima(resid, seasonal=F)
# 4. ARIMA simulation
adf.test(resid_multi, alternative = "stationary")
auto.arima(resid_multi, seasonal=F, stepwise = F)

fit <- arima(resid_multi, order = c(0,0,0))
fit

tsdisplay(residuals(fit), lag.max=30, main='(0,0,0) Model Residuals')
fcast <- forecast(fit, h=15) 
plot(fcast)

z <- arima.sim(n=31,model=list(order=c(0,0,0)),  innov = rlnorm(31, mean = mean(log(resid_multi)), sd = sd(log(resid_multi))))

meanlog <- mean(resid_multi)
meanlog
sdlog <- sd(resid_multi)
sdlog
z <- arima.sim(n=31,model=list(order=c(0,0,0)),  innov = rnorm(31, mean = meanlog, sd = sdlog))
# z <- arima.sim(n=31,model=list(order=c(0,0,0)),  innov = rlnorm(31, mean = -0.3510179, sd =  0.8293486))

w <- c(resid_multi, z)
plot(w, type = "l", lty =2 )
lines(resid_multi, col = "red")

plot(z)

library(mgcv)
time <- 1:46
fit <- gam(resid_multi ~ s(time, k = 2))

plot(resid_multi)
lines(time, predict(fit))

resid_detrend <- resid(fit)

plot(resid_detrend, type = 'l')

spec.ar(resid_detrend)

auto.arima(resid_detrend, stationary = TRUE, allowdrift = FALSE)

AIC(arima(resid_detrend, order = c(0,0,0)))

########## HAD ##########
# 1. data preparation
load("species_data.RData")
HAD_data <- sp_data[[3]]

ssb <- HAD_data$SSB
rec <- HAD_data$recruitment

# 2. calculation of the time series of residuals (1971 -2017)
sdev <- 0.936227
S0.hat <- 165558.1
R0.hat <- 518845.8
h <- 0.7633254

R <- 0.8*R0.hat*h*ssb / (0.2*S0.hat*(1-h) + ssb*(h-0.2))

# log(R)-(sdev^2)/2 corresponds to the meanlog of the residuals when we use a log normal distribution
# resid_multi <- log(rec)/(log(R)-(sdev^2)/2)
resid_multi <- rec/R


# 3. plot of the residuals
plot(resid_multi, type = "b", bty = "l")
abline (h =0, col = "blue", lty = 2)

# 4. ARIMA simulation
adf.test(resid_multi, alternative = "stationary")
auto.arima(resid, seasonal=F)

fit <- arima(resid, order = c(0,0,0))
fit

tsdisplay(residuals(fit), lag.max=30, main='(0,0,0) Model Residuals')
fcast <- forecast(fit, h=15) 
plot(fcast)

z <- arima.sim(n=31,model=list(order=c(0,0,0)),  innov = rlnorm(31, mean = mean(log(resid_multi)), sd = sd(log(resid_multi))))

meanlog <- mean(log(resid_multi))
meanlog
sdlog <- sd(log(resid_multi))
sdlog
z <- arima.sim(n=31,model=list(order=c(0,0,0)),  innov = rlnorm(31, mean = meanlog, sd = sdlog))
z <- arima.sim(n=31,model=list(order=c(0,0,0)),  innov = rlnorm(31, mean = -0.3576565, sd = 0.9898839))

w <- c(resid_multi, z)
plot(w, type = "l", lty =2 )
lines(resid_multi, col = "red")

plot(z)

########## WHG ##########
# 1. data preparation
load("species_data.RData")
WHG_data <- sp_data[[1]]

ssb <- WHG_data$SSB
rec <-WHG_data$recruitment

# 2. calculation of the time series of residuals (1971 -2017)
sdev <- 0.3906877
S0.hat <- 129815.7
R0.hat <- 1107230
h <-  0.9179865

R <- 0.8*R0.hat*h*ssb / (0.2*S0.hat*(1-h) + ssb*(h-0.2))

# log(R)-(sdev^2)/2 corresponds to the meanlog of the residuals when we use a log normal distribution
# resid_multi <- log(rec)/(log(R)-(sdev^2)/2)
resid_multi <- rec/R

# 3. plot of residuals
plot(resid, type = "b", bty = "l")
abline (h =0, col = "blue", lty = 2)

# 4. ARIMA simulation
adf.test(resid, alternative = "stationary")
auto.arima(resid, seasonal=F)

fit <- arima(resid, order = c(0,0,0))
fit

tsdisplay(residuals(fit), lag.max=30, main='(0,0,0) Model Residuals')
fcast <- forecast(fit, h=15) 
plot(fcast)

z <- arima.sim(n=31,model=list(order=c(0,0,0)),  innov = rlnorm(31, mean = mean(log(resid_multi)), sd = sd(log(resid_multi))))

meanlog <- mean(log(resid_multi))
meanlog
sdlog <- sd(log(resid_multi))
sdlog
z <- arima.sim(n=31,model=list(order=c(0,0,0)),  innov = rlnorm(31, mean = meanlog, sd = sdlog))
z <- arima.sim(n=31,model=list(order=c(0,0,0)),  innov = rlnorm(31, mean = -0.07378541, sd = 0.4018922))

# plot(z)


w <- c(resid_multi, z)
plot(w, type = "l")
lines(resid_multi, col = "red")

plot(resid_multi, type ="l")
lines(z, col = "red")
