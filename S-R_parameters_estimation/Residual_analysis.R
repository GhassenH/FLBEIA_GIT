library(ggplot2)
## ssb and rec data
load("species_data.RData")

#####################################################################
################################ COD ################################
#####################################################################

COD_data <- sp_data[[2]]
COD_data <- COD_data[-c(1:19),]

lag <- 1
ssb <- COD_data$SSB
rec <- COD_data$recruitment

ssb <- ssb[-c((length(ssb)-lag + 1):length(ssb)) ]
rec <- rec[-c(1:lag)]
data <- data.frame(ssb, rec)

a <- 8257.106
b <- 3155.035
sr <- function(x) {(a * x) / (b + x)}

## SR cod plot
sr_plot <- ggplot(data = data, aes(x = ssb, y = rec)) +
  geom_point() +
  theme_minimal() +
  stat_function(fun = sr, col = "grey") +
  xlim(0,3e4)
sr_plot

# estimation rec
est_rec <- sapply(data$ssb, sr)

# diff between observation and estimation
diff <- data$rec - est_rec
plot(diff)
abline(h=mean(diff), col="red")
abline(h=0, col="blue")

## Likelihood function to estimation the distribution function of the residuals (sd parameter of dnorm)  
m <- mean (diff)
sd_est <- function (a) {
  f <- dnorm(x = diff, mean = m, sd = a, log = T)  
  return(-sum(f))
}

fit <- optim(par = c(a=100), fn = sd_est, method = "L-BFGS-B", lower = 0)
sd <-fit$par
sd

# Plot
h<-Vectorize(sd_est)
curve(h, from = 0, to=50000, xlab= expression(mu), ylab = "Neg Log likelihood")
abline(v=sd(diff), col="red")
abline(v=fit$par, col="blue", lty=2)

## Likelihood for the lognormal distribution
diff2 <- sqrt(diff^2)
hist(diff2)

m <- mean (diff2)
sd_est <- function (a) {
  f <- dlnorm(x = diff2, meanlog = log(m), sdlog = a, log = T)  
  return(-sum(f))
}

fit <- optim(par = c(a=10), fn = sd_est, method = "Brent", lower = 0, upper = 100)
fit
sd <-fit$par
sd

a <- rlnorm(n = 31, meanlog = log(m), sdlog = sd)
hist(a)
#####################################################################
################################ HAD ################################
#####################################################################
load("species_data.RData")
HAD_data <- sp_data[[3]]

ssb <- HAD_data$SSB
rec <- HAD_data$recruitment
data <- data.frame(ssb, rec)

a <- 562443.30
b <- 13911.46

sr <- function(x) {(a * x) / (b + x)}

## SR cod plot
sr_plot <- ggplot(data = data, aes(x = ssb, y = rec)) +
  geom_point() +
  theme_minimal() +
  stat_function(fun = sr, col = "grey") +
  xlim(0,1e5) +
  ggtitle("HAD")
sr_plot

# estimation rec
est_rec <- sapply(data$ssb, sr)

# diff between observation and estimation
diff <- data$rec - est_rec
plot(diff)
abline(h=mean(diff), col="red")
abline(h=0, col="blue")
median(diff)

## Likelihood function to estimation the distribution function of the residuals (sd parameter of dnorm)  
m <- mean (diff)
sd_est <- function (a) {
  f <- dnorm(x = diff, mean = m, sd = a, log = T)  
  return(-sum(f))
}

fit <- optim(par = c(a=100), fn = sd_est, method = "L-BFGS-B", lower = 0)
sd <-fit$par
sd

# Plot
h<-Vectorize(sd_est)
curve(h, from = 0, to=5000000, xlab= expression(mu), ylab = "Neg Log likelihood")
abline(v=sd(diff), col="red")
abline(v=fit$par, col="blue", lty=2)

## lognormal distribution
## Likelihood function to estimation the distribution function of the residuals (sd parameter of dnorm)  
diff2 <- sqrt(diff^2)
hist(diff2)

m <- mean (diff2)
sd_est <- function (a) {
  f <- dlnorm(x = diff2, meanlog = log(m), sdlog = a, log = T)  
  return(-sum(f))
}

fit <- optim(par = c(a=1000), fn = sd_est , method = "Brent", lower = 0, upper = 100)
fit
sd <-fit$par
sd

#####################################################################
################################ WHG ################################
#####################################################################

load("species_data.RData")
WHG_data <- sp_data[[1]]

ssb <- WHG_data$SSB
rec <- WHG_data$recruitment
data <- data.frame(ssb, rec)

a <- 1132525.277
b <- 2965.695

sr <- function(x) {(a * x) / (b + x)}

## SR cod plot
sr_plot <- ggplot(data = data, aes(x = ssb, y = rec)) +
  geom_point() +
  theme_minimal() +
  stat_function(fun = sr, col = "grey") +
  xlim(0,1e5) + 
  ggtitle("WHG")
sr_plot

# estimation rec
est_rec <- sapply(data$ssb, sr)

# diff between observation and estimation
diff <- data$rec - est_rec
plot(diff)
abline(h=mean(diff), col="red")
abline(h=0, col="blue")
median(diff)

## Likelihood function to estimation the distribution function of the residuals (sd parameter of dnorm)  
m <- mean (diff)
sd_est <- function (a) {
  f <- dnorm(x = diff, mean = m, sd = a, log = T)  
  return(-sum(f))
}

fit <- optim(par = c(a=100), fn = sd_est, method = "L-BFGS-B", lower = 0)
sd <-fit$par
sd

# Plot
h<-Vectorize(sd_est)
curve(h, from = 0, to=5000000, xlab= expression(mu), ylab = "Neg Log likelihood")
abline(v=sd(diff), col="red")
abline(v=fit$par, col="blue", lty=2)

## lognormal distribution
## Likelihood function to estimation the distribution function of the residuals (sd parameter of dnorm)  
diff2 <- sqrt(diff^2)
hist(diff2)

m <- mean (diff2)
sd_est <- function (a) {
  f <- dlnorm(x = diff2, meanlog = log(m), sdlog = a, log = T)  
  return(-sum(f))
}

fit <- optim(par = c(a=1000), fn = sd_est, method = "Brent", lower = 0, upper = 100)
fit
sd <-fit$par
sd
