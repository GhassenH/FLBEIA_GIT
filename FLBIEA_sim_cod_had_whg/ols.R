
q1 <- 0.1
q2 <- 0.05
m <- seq(0.2, 3, length = 15)

qi1 <- q1 * m
qj2 <- q2 * m

E <- 2

a <- 0.6

C <- matrix(NA, 15, 15)

## column is metier 1 - indexed by i
## row is metier 2 - indexed by j

B <- 5

## reconstructing the catches based on the cobb douglas equation C = q*E*B
for(i in 1:15){
    for(j in 1:15){
        C[j,i] <- (qi1[i] * a * E  + qj2[j] * (1-a) * E) * B 
    }
}
rownames(C) <- paste0("F2j", 1:15)
colnames(C) <- paste0("F1i", 1:15)

library(fields)
library(viridis)

image.plot(C, col = viridis(30))

## calculate index
Cdf <- data.frame(C = c(C),
                  F1 = rep(qi1 * a * E, each = 15),
                  F2 = rep(qj2 * (1-a) * E, times = 15))

## small correction on the x and y axis!
Cdf <- data.frame(C = c(C),
                  F1 = rep(qi1 , each = 15),
                  F2 = rep(qj2 , times = 15))

### index calculation
fit <- lm(C ~ -1 + F1 + F2, data = Cdf)

beta <- coef(fit)

ind <- beta[1] / (beta[1] + beta[2])

### using ols equations
X <- with(Cdf, cbind(F1, F2))
Y <- matrix(Cdf$C)

beta_ols <- solve(t(X) %*% X) %*% t(X) %*% Y

### what is this exactly? - see Princeton handout
y <- c(Y)      # catches
x <- Cdf$F1    # effort TR1 (q1*E1*multiplier i)
z <- Cdf$F2    # effort TR2 (q2*E2*multiplier j)


beta1 <- (cov(x, y) * var(z) - cov(z, y) * cov(x, z)) /
    (var(x) * var(z) - cov(x,z)^2) 

beta2 <- (cov(z, y) * var(x) - cov(x, y) * cov(z,x)) /
    (var(x) * var(z) - cov(x,z)^2) 

beta1 / (beta1 + beta2)

ind_equation <- (cov(x, y) * var(z) - cov(z, y) * cov(x, z)) /
    ((cov(x, y) * var(z) - cov(z, y) * cov(x, z)) + (cov(z, y) * var(x) - cov(x, y) * cov(z,x)))

