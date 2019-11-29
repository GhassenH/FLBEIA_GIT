#####################################################################################################
# estimation the stock recruitement parameters of Whiting using the steepness h
#####################################################################################################


# ============================== S0 ============================== 

## Weight at age
W_age <- read.csv("SR data Whiting from report WGCSE/weight_age.csv", row.names = 1)
colnames(W_age) <- c("0", seq(1:7))
w_age_2016 <- t(W_age[18,])
colnames(w_age_2016) <- "weight"

## Number at age
N_age <- read.csv("SR data Whiting from report WGCSE/n_age.csv", row.names = 1)
colnames(N_age) <- c("0", seq(1:7))
N_age_2016 <- t(N_age[10,])
colnames(N_age_2016) <- "N"

## Mat at age
M_Mat_age <- read.csv("SR data Whiting from report WGCSE/M_Mat_age.csv")
M_Mat <- M_Mat_age[,-2]

## weight, N, Mat at age
data <- data.frame(M_Mat, weight = w_age_2016, age = N_age_2016)

## S0
S0 <- apply(data, 1, function(x) x[2]*x[3]*x[4])
S0 <- sum(S0)

# ============================== Likelihood function : Estimation of R0  ============================== 

load("species_data.RData")
WHG_data <- sp_data[[1]]

ssb <- WHG_data$SSB
rec <- WHG_data$recruitment
h <- 0.81

LL <- function(par) {
  R0 <- exp(par[1])
  c <- exp(par[2])
  
  R <- 0.8*R0*h*ssb / (0.2*S0*(1-h) + ssb*(h-0.2))
  
  ll = dlnorm(rec, meanlog = log(R)-(c^2)/2, sdlog = c, log = T)
  
  -sum(ll)
}

LL_opt <- nlminb(start = log(c(10000, 0.2)),  objective = LL)
LL_opt

R0_est <- exp(LL_opt$par[1])

## S-R parameters
a <- (S0/R0_est) * (1-h)/(4*h)
b <- (5*h-1) / (4*h*R0_est)

sr1 <- function(x){ x / (a + b*x) }

s <- seq(0, 10e5, by = 1000)
plot(ssb, rec, bty = "l",  xlim = c(0, 1e5), ylim = c(0, 3e6),
     xlab = "ssb", ylab = "rec")
lines(s, sr1(s))

