#####################################################################################################
# estimation of the stock recruitement parameters of Cod using a prior on the steepness h (z)
#####################################################################################################

# ============================== input data for COD (stock assessement 2016) 

## Age
age <- seq(from = 1,  to = 7)

## M by age
M <- c(0.512, 0.368, 0.304, 0.269, 0.247, 0.233, 0.233)

## Mat by age
Mat <- c(0, 0.39, 0.87, 0.93, 1, 1, 1)

## Weight by age
W2014 <- c(0.464, 1.654, 3.788, 6.530, 9.074, 10.584, 11.611) # Weight by age2014
W2015 <- c(1.161, 1.309, 4.079, 8.517, 10.105, 10.661, 12.288) # Weight by age 2015
W2016 <- c(0.647, 1.310, 3.683, 6.700, 10.573, 11.453, 12.928) #  Weight by age2016
W.data <- data.frame(W2014, W2015, W2016)
W <- apply(W.data, 1, mean) # mean weight 2014, 2015, 2016 


data <- data.frame(M = M, Mat = Mat, W = W)
row.names(data) <- age

## ssb and rec data
load("species_data.RData")
COD_data <- sp_data[[2]]

lag <- 1
ssb <- COD_data$SSB
rec <- COD_data$recruitment

ssb <- ssb[-c((length(ssb)-lag + 1):length(ssb)) ]
rec <- rec[-c(1:lag)]

# ============================== adding a prior for the steepness parameter (from Meyers et al. 1999)

zmed <- 0.84
z20 <- 0.76
z80 <- 0.9

## sum of square of the difference at z20 and z80
ssq <- function(alpha){
  beta <- (alpha - 1/3) / zmed - alpha + 2/3
  z20.pred <- qbeta(p = 0.2, shape1 = alpha, shape2 = beta)
  z80.pred <- qbeta(p = 0.8, shape1 = alpha, shape2 = beta)
  return((z20.pred-z20)^2 + (z80.pred-z80)^2)
}

fit <- optim(par = 2, fn = ssq, method = "Brent", lower = 1, upper = 50)

alpha.hat <- fit$par
beta.hat <- (alpha.hat - 1/3) / zmed - alpha.hat + 2/3

# curve(dbeta(x, shape1 = alpha.hat, shape2 = beta.hat), from = 0, to = 1, n = 1e3)
# abline(v = qbeta(c(0.2, 0.5, 0.8), shape1 = alpha.hat, shape2 = beta.hat))
# abline(v = c(z20, zmed, z80), lty = 2)

# ============================== Likelihood function : Estimation of R0 using a prior en h

## likelihood function
LL <- function(par) {
  
  # parameters to estimate
  R0 <- exp(par[1])
  sdev <- exp(par[2])
  h <- exp(par[3])
  
  # abundance by age class 
  N <- rep(NA, 7)
  N[1] <- R0
  for (i in 1:6) { 
    N[i+1] <- N[i]*exp(-M[i]) 
  }
  
  # S0 
  data$N <- N
  S <- apply(data, 1, function(x) x[2]*x[3]*x[4])
  S0 <- sum(S)
  
  R <- 0.8*R0*h*ssb / (0.2*S0*(1-h) + ssb*(h-0.2))
  
  ll = dlnorm(rec, meanlog = log(R)-(sdev^2)/2, sdlog = sdev, log = T)
  llp <- ll + dbeta(h, shape1 = alpha.hat, shape2 = beta.hat) # adding the prior
  
  -sum(llp)
}

LL_opt <- nlminb(start = log(c(10000, 0.2, 0.8)),  objective = LL)
LL_opt

R0.hat <- exp(LL_opt$par[1])
h <- exp(LL_opt$par[3])
sdev <- exp(LL_opt$par[2])

## Calculation of S0 from R0 
## N
N <- rep(NA, 7)
N[1] <- R0.hat
for (i in 1:6) { N[i+1] <- N[i]*exp(-M[i]) }

## S0   
data$N <- N
S <- apply(data, 1, function(x) x[2]*x[3]*x[4])
S0.hat <- sum(S)

a <- (S0.hat/R0.hat) * (1-h)/(4*h)
b <- (5*h-1) / (4*h*R0.hat)

sr1 <- function(x){ x / (a + b*x) }

s <- seq(0, 10e5, by = 100)
plot(ssb, rec, bty = "l",  xlim = c(0, 3e4), ylim = c(0, 3e4),
     xlab = "ssb", ylab = "rec", main = "COD SRR using a prior on the steepness parameter h")
lines(s, sr1(s))

