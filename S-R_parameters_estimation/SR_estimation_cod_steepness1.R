#####################################################################################################
# estimation the stock recruitement parameters of Cod using the steepness h (fixed)
#####################################################################################################

## Age
age <- seq(from = 1,  to = 7)

## M
M <- c(0.512, 0.39496 ,0.47621, 0.42494, 0.45796, 0.33431, 0.33431)

## Mat
Mat <- c(0, 0.39, 0.87, 0.93, 1, 1, 1)

## Weight

W <- c(0.647, 1.310, 3.683, 6.700, 10.573, 11.453, 12.928)

data <- data.frame(M = M, Mat = Mat, W = W)
row.names(data) <- age


# ============================== Likelihood function : Estimation of R0  ============================== 

## loading data
load("species_data.RData")
COD_data <- sp_data[[2]]

lag <- 1
ssb <- COD_data$SSB
rec <- COD_data$recruitment

ssb <- ssb[-c((length(ssb)-lag + 1):length(ssb)) ]
rec <- rec[-c(1:lag)]

h <- 0.84 # value from Meyers et al. 1999

## likelihood function
LL <- function(par) {
  
  R0 <- exp(par[1])
  c <- exp(par[2])
  
  M <- M # vector of natural mortality by age 
  N <- rep(NA, 7)
  N[1] <- R0
  for (i in 1:6) { 
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

## S-R parameters

# Calculation of SO from R0 
## N
N <- rep(NA, 7)
N[1] <- R0_est
for (i in 1:6) { N[i+1] <- N[i]*exp(-M[i]) }

## S0   
data$N <- N
S <- apply(data, 1, function(x) x[2]*x[3]*x[4])
S0_est <- sum(S)


a <- (S0_est/R0_est) * (1-h)/(4*h)
b <- (5*h-1) / (4*h*R0_est)

sr1 <- function(x){ x / (a + b*x) }

s <- seq(0, 10e5, by = 100)
plot(ssb, rec, bty = "l",  xlim = c(0, 3e4), ylim = c(0, 3e4),
     xlab = "ssb", ylab = "rec", main = "COD SRR using the steepness parameter (fixed h)")
lines(s, sr1(s))






