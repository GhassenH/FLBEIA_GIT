#####################################################################################################
# estimation of the stock recruitement parameters of Haddock using a prior on the steepness h (z)
#####################################################################################################


# ============================== input data for Haddock (stock assessement 2016) 

## Age
age <- seq(from = 0,  to = 8)

## M
M <- c(0.99, 0.72, 0.6, 0.5, 0.43, 0.4, 0.37, 0.36, 0.34)

## Mat
Mat <- c(0, 0, 1, 1, 1, 1, 1, 1, 1)

## Weight
W2014 <- c(0.055, 0.148, 0.315, 0.572, 0.824, 1.251, 1.617, 1.922, 2.115) # Weight Haddock by age 2014
W2015 <- c(0.07, 0.16, 0.32, 0.62, 0.87, 1.15, 1.65, 1.82, 2) # Weight Haddock by age 2015
W2016 <- c(0.08, 0.17, 0.32, 0.63, 0.93, 1.17, 1.63, 1.79, 1.97) # Weight Haddock by age 2016
W.data <- data.frame(W2014, W2015, W2016)
W <- apply(W.data, 1, mean) # mean weight 2014, 2015, 2016 

data <- data.frame(M = M, Mat = Mat, W = W)
row.names(data) <- age

## ssb and rec data
load("species_data.RData")
HAD_data <- sp_data[[3]]

ssb <- HAD_data$SSB
rec <- HAD_data$recruitment

# ============================== adding a prior for the steepness parameter (from Meyers et al. 1999)

zmed <- 0.74
z20 <- 0.64
z80 <- 0.82

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
  
  N <- rep(NA, 8)
  N[1] <- R0
  for (i in 1:8) { 
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

LL_opt <- nlminb(start = log(c(10000, 0.1, 0.8)),  objective = LL)
LL_opt

R0.hat <- exp(LL_opt$par[1])
h <- exp(LL_opt$par[3])
sdev <- exp(LL_opt$par[2])


## Calculation of SO from R0 
## N
N <- rep(NA, 8)
N[1] <- R0.hat
for (i in 1:8) { N[i+1] <- N[i]*exp(-M[i]) }

## S0   
data$N <- N
S <- apply(data, 1, function(x) x[2]*x[3]*x[4])
S0_hat <- sum(S)

# ============================== plotting SRR

a <- (S0_hat/R0.hat) * (1-h)/(4*h)
b <- (5*h-1) / (4*h*R0.hat)

sr1 <- function(x){ x / (a + b*x) }

s <- seq(0, 10e5, by = 1000)
plot(ssb, rec, bty = "l",  xlim = c(0, 1e5), ylim = c(0, 3e6),
     xlab = "ssb", ylab = "rec", main = "HAD SRR using a prior on the steepness parameter h")
lines(s, sr1(s))

