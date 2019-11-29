#####################################################################################################
# estimation of the stock recruitement parameters of Whiting using a prior on the steepness h (z)
#####################################################################################################


# ============================== input data for WHG (stock assessement 2016) 

## Age
age <- seq(from = 0,  to = 7)

## M by age
M <- c(1.22, 0.86, 0.65, 0.5, 0.43, 0.4, 0.38, 0.36)

## Mat by age
Mat <- c(0, 0, 1, 1, 1, 1, 1, 1)

## Weight by age
W2014 <- c(0.038, 0.142, 0.254, 0.397, 0.554, 0.662, 0.759, 1.007) # Weight by age2014
W2015 <- c(0.018, 0.102, 0.220, 0.375, 0.573, 0.778, 0.671, 0.929) # Weight by age2015
W2016 <- c(0.052, 0.149, 0.217, 0.358, 0.577, 0.685, 0.746, 0.784) # Weight by age2016
W.data <- data.frame(W2014, W2015, W2016)
W <- apply(W.data, 1, mean) # mean weight 2014, 2015, 2016 


data <- data.frame(M = M, Mat = Mat, W = W)
row.names(data) <- age

## ssb and rec data
load("species_data.RData")
WHG_data <- sp_data[[1]]

ssb <- WHG_data$SSB
rec <- WHG_data$recruitment

# ============================== adding a prior for the steepness parameter (from Meyers et al. 1999)

zmed <- 0.81
z20 <- 0.64
z80 <- 0.91

## sum of square of the difference at z20 and z80
ssq <- function(alpha){
  beta <- (alpha - 1/3) / zmed - alpha + 2/3 # from the equation of the median(see wiki)
  z20.pred <- qbeta(p = 0.2, shape1 = alpha, shape2 = beta)
  z80.pred <- qbeta(p = 0.8, shape1 = alpha, shape2 = beta)
  return((z20.pred-z20)^2 + (z80.pred-z80)^2)
}

fit <- optim(par = 2, fn = ssq, method = "Brent", lower = 1, upper = 50)

alpha.hat <- fit$par
beta.hat <- (alpha.hat - 1/3) / zmed - alpha.hat + 2/3

# curve(dbeta(x, shape1 = alpha.hat, shape2 = beta.hat), from = 0, to = 1, n = 1e4)
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
  N <- rep(NA, 8)
  N[1] <- R0
  for (i in 1:7) { 
    N[i+1] <- N[i]*exp(-M[i]) 
  }
  
  # S0 
  data$N <- N
  S <- apply(data, 1, function(x) x[2]*x[3]*x[4]) # calculation of the biomass by age class when F = 0 : mat * W * N
  S0 <- sum(S)
  
  R <- 0.8*R0*h*ssb / (0.2*S0*(1-h) + ssb*(h-0.2))
  
  ll = dlnorm(rec, meanlog = log(R)-(sdev^2)/2, sdlog = sdev, log = T)
  llp <- ll + dbeta(h, shape1 = alpha.hat, shape2 = beta.hat) # adding the prior on the h
  
  -sum(llp)
}

LL_opt <- nlminb(start = log(c(10000, 0.2, 0.8)),  objective = LL)
LL_opt

R0.hat <- exp(LL_opt$par[1])
h <- exp(LL_opt$par[3])
sd <- exp(LL_opt$par[2])


## Calculation of S0 from R0 
## N
N <- rep(NA, 8)
N[1] <- R0.hat
for (i in 1:7) { N[i+1] <- N[i]*exp(-M[i]) }

## S0   
data$N <- N
S <- apply(data, 1, function(x) x[2]*x[3]*x[4])
S0.hat <- sum(S)

a <- (S0.hat/R0.hat) * (1-h)/(4*h)
b <- (5*h-1) / (4*h*R0.hat)

sr1 <- function(x){ x / (a + b*x) }

s <- seq(0, 10e5, by = 100)
plot(ssb, rec, bty = "l",  xlim = c(0, 1e5), ylim = c(0, 3e6),
     xlab = "ssb", ylab = "rec", main = "WHG SRR using a prior on the steepness parameter h")
lines(s, sr1(s))
