library(icesSAG) 

# WHG_data <- getSAG(stock = "whg.27.7b-ce-k", year = 2017, data = "summary")

load("species_data.RData")
WHG_data <- sp_data[[1]]

ssb <- WHG_data$SSB
rec <- WHG_data$recruitment

plot(rec, ssb)

## ================= estimation of the Prior =================
lag <- 0
ssb.whg.2017 <- ssb[length(ssb)-lag]
rec.whg.2017 <- rec[length(rec)-lag]

# SPR <- ssb.whg.2017 / (rec.whg.2017*10^3) # spawing biomass resulting from each recruit
# M <- 0.3 # mortality
# MARR <- 30.8 # Maximum annual reproductive rate
# alpha.prior <- MARR / (SPR * (1-exp(-M)))
# alpha.prior

SPR <- ssb.whg.2017 / (rec.whg.2017) # spawing biomass resulting from each recruit
z <- 0.81
alpha.hat <- 4*z/(1-z)
aplha.prior.2 <- alpha.hat / SPR
aplha.prior.2

## ================= Likelihood function =================
LL <- function(par){
  a <- exp(par[1])
  b <- exp(par[2])
  c <- exp(par[3])
  # R <- a*ssb / (b+ssb)
  # R <- a*ssb*exp(-*ssb)
  R <- a*ssb /(1 + ((a/b) * ssb))
  
  
  ll = dlnorm(rec, meanlog = log(R)-(c^2)/2, sdlog = c, log = T)
  lp <- ll + dnorm(log(a), log(aplha.prior.2), 0.2) # adding a prior on b parameter to imporve the estimation
  -sum(lp)
}

# LL_opt <- optim(par = log(c(2, 70, .5)),  fn = LL)
LL_opt <- nlminb(start = log(c(25e1, 8e5, .5)),  objective = LL)
LL_opt

a <- exp(LL_opt$par[1])
b <- exp(LL_opt$par[2])
data <- data.frame(paramter_a = a, paramter_b = b)
# sr1 <- function(x){a*x / (b+x)}
sr1 <- function(x){a*x /(1 + ((a/b) * x))}

s <- seq(0, 10e5, by = 1000)
plot(ssb, rec, bty = "l",  xlim = c(0, 1e5), ylim = c(0, 3e6),
     xlab = "ssb", ylab = "rec")
lines(s, sr1(s))

