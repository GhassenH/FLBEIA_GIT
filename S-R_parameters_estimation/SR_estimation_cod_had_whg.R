#################################################################
#
#   S-R paramter estimation for : 
# - Haddock (Melanogrammus aeglefinus) 
# - Whiting (Merlangius merlangus) 
# - Cod (Gadus morhua) 
# 
##################################################################

library(icesSAG) 

## Download stock data

WHG_data <- getSAG(stock = "whg.27.7b-ce-k", year = 2017, data = "summary")
COD_data <- getSAG(stock = "cod.27.7e-k", year = 2017, data = "summary")
HAD_data <- getSAG(stock = "had.27.7b-k", year = 2017, data = "summary")

## Estimation of the S-R parameters

#  ============== WHG ==============

# Adding n year lag for the cod since the recruitement age = n
lag <- 2 # lag should be > 0
ssb <- WHG_data$SSB
rec <- WHG_data$recruitment

ssbp <- ssb[-c(1:lag)]
recp <- rec[-c((length(rec)-lag + 1):length(rec)) ]

plot(ssb, rec)
points(ssbp, recp, pch = 20, col = "blue")

## Likelihood function
LL <- function(par){
  a <- exp(par[1])
  b <- exp(par[2])
  c <- exp(par[3])
  
  R <- a*ssb / (b+ssb)
  
  ll = dlnorm(rec, meanlog = log(R)-(c^2)/2, sdlog = c, log = T)
  lp <- ll + dnorm(log(b), log(3e4), 0.2) # adding a prior on b parameter to imporve the estimation
  -sum(ll)
}

# LL_opt <- optim(par = log(c(25e5, 50000, .5)),  fn = LL)
LL_opt <- nlminb(start = log(c(25e5, 50000, .5)),  objective = LL)
LL_opt
a <- exp(LL_opt$par[1])
b <- exp(LL_opt$par[2])

sr1 <- function(x){a*x / (b+x)}
s <- seq(0, 10e5, by = 1000)
plot(ssb, rec, bty = "l",  xlim = c(0, 1e5), ylim = c(0, 3e6))
lines(s, sr1(s))
text(ssb, rec, labels=WHG_data$Year, cex = 0.7, pos = 2)


#  ============== COD ==============

# Adding n year lag for the cod since the recruitement age = n
lag <- 1
ssb <- COD_data$SSB
rec <- COD_data$recruitment

ssb <- ssb[-c((length(ssb)-lag + 1):length(ssb)) ]
rec <- rec[-c(1:lag)]

plot(ssb, rec)
points(ssb, rec, pch = 20, col = "red")

# Likelohood function
LL <- function(par){
  a <- exp(par[1])
  b <- exp(par[2])
  c <- exp(par[3])
  
  R <- a*ssb / (b+ssb)
  
  y = dlnorm(rec, meanlog = log(R)-(c^2)/2, sdlog = c, log = T)
  -sum(y)
}

LL_opt <- optim(par = log(c(15e3, 10000, 2)),  fn = LL)
LL_opt

a <- exp(LL_opt$par[1])
b <- exp(LL_opt$par[2])

sr1 <- function(x){a*x / (b+x)}
s <- seq(0, 10e5, by = 1000)
plot(ssb, rec, bty = "l",  xlim = c(0, 3e4), ylim = c(0, 3e4))
lines(s, sr1(s))
# text(ssb, rec, labels=COD_data$Year, cex = 0.7, pos = 2)


#  ============== HAD ==============

ssb <- HAD_data$SSB
rec <- HAD_data$recruitment

plot(ssb, rec)

## Likelohood function
LL <- function(par){
  a <- exp(par[1])
  b <- exp(par[2])
  c <- exp(par[3])
  
  R <- a*ssb / (b+ssb)
  
  ll = dlnorm(rec, meanlog = log(R)-(c^2)/2, sdlog = c, log = T)
  lp <- ll + dnorm(log(b), log(4e4), 0.2) # adding a prior on b parameter to imporve the estimation
  -sum(lp)
}

# LL_opt <- optim(par = log(c(1e6, 10000, 2)),  fn = LL)
LL_opt <- nlminb(start = log(c(2e6, 4e4, 2)),  objective = LL)
LL_opt

a <- exp(LL_opt$par[1])
b <- exp(LL_opt$par[2])

sr1 <- function(x){a*x / (b+x)}
s <- seq(0, 10e5, by = 1000)
plot(ssb, rec, bty = "l",  xlim = c(0, 10e4), ylim = c(0, 2e6))
lines(s, sr1(s))

text(ssb, rec, labels=HAD_data$Year, cex = 0.7, pos = 2)





# ================================ Evoloutionary Algorithme============================
library(calibrar) 

ssb <- HAD_data$SSB
rec <- HAD_data$recruitment

## Likelohood function
LL <- function(par){
  a <- exp(par[1])
  b <- exp(par[2])
  c <- exp(par[3])
  
  R <- a*ssb / (b+ssb)
  
  ll = dlnorm(rec, meanlog = log(R)-(c^2)/2, sdlog = c, log = T)
  # lp <- ll + dnorm(log(b), log(1e4), 0.2) # adding a prior on b parameter to imporve the estimation
  -sum(ll)
}


# Max phase = n(activated paramteres) - 1 (at minimum two parameters in the first phase)
LL_calib <- calibrate(par = log(c(1e6, 20000, 0.5)),  fn = LL, phases = c(1,2,1))
a <- exp(LL_calib$par[1])
b <- exp(LL_calib$par[2])

sr1 <- function(x){a*x / (b+x)}
s <- seq(0, 10e5, by = 1000)
plot(ssb, rec, bty = "l",  xlim = c(0, 10e4), ylim = c(0, 2e6))
lines(s, sr1(s), lty = 2, col = "red")
text(ssb, rec, labels=WHG_data$Year, cex = 0.7, pos = 2)



