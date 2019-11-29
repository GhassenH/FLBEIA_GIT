#####################################################################################################
# estimation the stock recruitement parameters of Whiting using the steepness h (fixed)
#####################################################################################################


## Weight at age
W_age <- read.csv("SR data Whiting from report WGCSE/weight_age.csv", row.names = 1)
colnames(W_age) <- c("0", seq(1:7))
w_age_2016 <- t(W_age[18,])
colnames(w_age_2016) <- "Weight"

## Mat at age
M_Mat_age <- read.csv("SR data Whiting from report WGCSE/M_Mat_age.csv")
M_Mat <- M_Mat_age[,-2]

## weight and Mat at age
Mat_W_data <- data.frame(Mat = M_Mat$Mat, Weight = w_age_2016)


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
# ============================== Likelihood function : Estimation of R0  ============================== 

## loading data
load("species_data.RData")
WHG_data <- sp_data[[1]]

ssb <- WHG_data$SSB
rec <- WHG_data$recruitment
h <- 0.81 # value from Meyers et al. 1999

## likelihood function
LL <- function(par) {
  
  R0 <- exp(par[1])
  c <- exp(par[2])
  
  M <- M # vector of natural mortality by age 
  N <- rep(NA, 7)
  N[1] <- R0
  for (i in 1:7) { 
    N[i+1] <- N[i]*exp(-M[i]) 
  }
  
  data$N <-N
  S <- apply(data, 1, function(x) x[1]*x[2]*x[3])
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
M <- M # vector of natural mortality by age 
N <- rep(NA, 7)
N[1] <- R0_est
for (i in 1:7) { N[i+1] <- N[i]*exp(-M[i]) }

## S0   
data$N <-N
S <- apply(data, 1, function(x) x[1]*x[2]*x[3])
S0_est <- sum(S)

a <- (S0_est/R0_est) * (1-h)/(4*h)
b <- (5*h-1) / (4*h*R0_est)

sr1 <- function(x){ x / (a + b*x) }

s <- seq(0, 10e5, by = 1000)
plot(ssb, rec, bty = "l",  xlim = c(0, 1e5), ylim = c(0, 3e6),
     xlab = "ssb", ylab = "rec")
lines(s, sr1(s))






