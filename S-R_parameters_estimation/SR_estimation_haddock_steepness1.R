#####################################################################################################
# estimation the stock recruitement parameters of Haddock using the steepness h (fixed)
#####################################################################################################


# ============================== input data for Haddock (stock assessement 2016) 

## Age
age <- seq(from = 0,  to = 8)

## M
M <- c(0.99, 0.72, 0.6, 0.5, 0.43, 0.4, 0.37, 0.36, 0.34)

## Mat
Mat <- c(0, 0, 1, 1, 1, 1, 1, 1, 1)

## Weight
W2014 <- c(0.055, 0.148, 0.315, 0.572, 0.824, 1.251, 1.617, 1.922, 2.115) # Weight 2014
W2015 <- c(0.07, 0.16, 0.32, 0.62, 0.87, 1.15, 1.65, 1.82, 2) # Weight 2015
W2016 <- c(0.08, 0.17, 0.32, 0.63, 0.93, 1.17, 1.63, 1.79, 1.97) # Weight 2016
W.data <- data.frame(W2014, W2015, W2016)
W <- apply(W.data, 1, mean) # mean weight 2014, 2015, 2016 


data <- data.frame(M = M, Mat = Mat, W = W)
row.names(data) <- age

## ssb and rec data
load("species_data.RData")
HAD_data <- sp_data[[3]]

ssb <- HAD_data$SSB
rec <- HAD_data$recruitment

# value of steepness from Myers et al. 1999
h <- 0.74 

# ============================== Likelihood function : Estimation of R0  ============================== 

## likelihood function
LL <- function(par) {
  
  # parameters to estimate
  R0 <- exp(par[1])
  c <- exp(par[2])
  
  # abundance by age class 
  N <- rep(NA, 8)
  N[1] <- R0
  for (i in 1:8) { 
    N[i+1] <- N[i]*exp(-M[i]) 
  }
  
  data$N <-N
  S <- apply(data, 1, function(x) x[2]*x[3]*x[4])
  S0 <- sum(S)
  
  R <- 0.8*R0*h*ssb / (0.2*S0*(1-h) + ssb*(h-0.2))
  
  ll = dlnorm(rec, meanlog = log(R)-(c^2)/2, sdlog = c, log = T)
  
  -sum(ll)
}

LL_opt <- nlminb(start = log(c(10000, 0.2)),  objective = LL)
LL_opt

R0_est <- exp(LL_opt$par[1])

## Calculation of SO from R0 
## N
N <- rep(NA, 8)
N[1] <- R0_est
for (i in 1:8) { N[i+1] <- N[i]*exp(-M[i]) }

## S0   
data$N <- N
S <- apply(data, 1, function(x) x[2]*x[3]*x[4])
S0_est <- sum(S)

# ============================== plotting SRR

a <- (S0_est/R0_est) * (1-h)/(4*h)
b <- (5*h-1) / (4*h*R0_est)

sr1 <- function(x){ x / (a + b*x) }

s <- seq(0, 10e5, by = 1000)
plot(ssb, rec, bty = "l",  xlim = c(0, 1e5), ylim = c(0, 3e6),
     xlab = "ssb", ylab = "rec", main = "HAD SRR using the steepness parameter (fixed h)")
lines(s, sr1(s))
