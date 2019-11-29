library(icesSAG) 

# WHG_data <- getSAG(stock = "whg.27.7b-ce-k", year = 2017, data = "summary")

load("species_data.RData")
WHG_data <- sp_data[[1]]

ssb <- WHG_data$SSB
rec <- WHG_data$recruitment


## ================= estimation of the Prior =================
lag <- 0
ssb.whg.2017 <- ssb[length(ssb)-lag]
rec.whg.2017 <- rec[length(rec)-lag]

SPR <- ssb.whg.2017 / (rec.whg.2017*10^3) # spawing biomass resulting from each recruit
M <- 0.3 # mortality
MARR <- 30.8 # Maximum annual reproductive rate

alpha.prior<- MARR / (SPR * (1-exp(-M)))
alpha.prior

## ================= Likelihood function =================
LL <- function(par){
  a <- exp(par[1])
  b <- exp(par[2])
  c <- exp(par[3])
  # R <- a*ssb / (b+ssb)
  R <- a*ssb*exp(-b*ssb)
  
  ll = dlnorm(rec, meanlog = log(R)-(c^2)/2, sdlog = c, log = T)
  lp <- ll + dnorm(log(a), log(alpha.prior), 0.5) # adding a prior on b parameter to imporve the estimation
  -sum(lp)
}

LL_opt <- optim(par = log(c(2, 70, .5)),  fn = LL)
LL_opt <- nlminb(start = log(c(25e5, 70000, .5)),  objective = LL)
LL_opt

a <- exp(LL_opt$par[1])
b <- exp(LL_opt$par[2])
data <- data.frame(paramter_a = a, paramter_b = b)
sr1 <- function(x){a*x / (b+x)}
s <- seq(0, 10e5, by = 1000)
plot(ssb, rec, bty = "l",  xlim = c(0, 1e5), ylim = c(0, 3e6),
     xlab = "ssb", ylab = "rec")
lines(s, sr1(s))





# ================= function ================= 
mortality <- function (i) {
  
## ================= estimation of the Prior =================
lag <- 0
ssb.whg.2017 <- ssb[length(ssb)-lag]
rec.whg.2017 <- rec[length(rec)-lag]

SPR <- ssb.whg.2017 / (rec.whg.2017*10^3) # spawing biomass resulting from each recruit
M <- i # mortality
MARR <- 30.8 # Maximum annual reproductive rate

alpha.prior<- MARR / (SPR * (1-exp(-M)))
alpha.prior

## ================= Likelihood function =================
LL <- function(par){
  a <- exp(par[1])
  b <- exp(par[2])
  c <- exp(par[3])
  R <- a*ssb / (b+ssb)
  # R <- a*ssb*exp(-b*ssb)
  
  ll = dlnorm(rec, meanlog = log(R)-(c^2)/2, sdlog = c, log = T)
  # lp <- ll + dnorm(log(b), log(1e4), 0.2) # adding a prior on b parameter to imporve the estimation
  lp <- ll + dnorm(log(a), log(alpha.prior), 0.2) # adding a prior on b parameter to imporve the estimation
  -sum(lp)
}

LL_opt <- optim(par = log(c(25e5, 70000, .5)),  fn = LL)
LL_opt <- nlminb(start = log(c(25e5, 70000, .5)),  objective = LL)
LL_opt

a <- exp(LL_opt$par[1])
b <- exp(LL_opt$par[2])
ab <- data.frame(a, b)
return(ab)

}

bb <- data.frame()
for (i in seq(0.1, 1, by = 0.05 )) {
 aa <- mortality(i)
 bb <- rbind(bb, aa)
 print(i)
}

s <- seq(0, 10e4, by = 1000)
aa <- data.frame(matrix(ncol = nrow(bb), nrow = length(s)))
colnames(aa) <- seq(1, nrow(bb))
for (i in 1:nrow(bb)){
  sr1 <- function(x){bb[i,1]*x / (bb[i,2]+x)} 
  aa[,i]<- sr1(s)
  print(i)
  print(aa)
} 


library(ggplot2)
library(reshape)
aa$M <- s 
aa.melt <- melt(aa, id.vars = "M")

p <- ggplot(aa.melt, aes(x = M, y = value, color = variable) ) +
  geom_line() +
  geom_point(WHG_data, mapping = aes(x = SSB, y = recruitment, color = "red"))
p




# ======================= new method to calculate alpha

alpha.est =(4*0.81)/(1-0.81)
SPR

alpha <- alpha.est/SPR
