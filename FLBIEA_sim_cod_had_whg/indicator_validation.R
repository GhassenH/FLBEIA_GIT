##################################################################################################
# Validation of the indicator of interactions between métiers 1 and 2 by using FLBEIA outputs
# to calculate the indicator.
# Results : we can't use this method to validate the equation (I have the same result from all fishing strategies)
##################################################################################################

# Libraries
library(FLBEIA)
library(tidyr)
library(dplyr)
library(plyr)
library(beepr)
library(fields)
library(stringr)
library(viridis)

## FLBEIA simulation results
load("outputs_interaction_min_AM_2.RData") 
load("outputs_interaction_max_AM_2.RData") 
load("outputs_interaction_mean_AM_2.RData") 
load("outputs_interaction_previous_AM_2.RData") 

## catch, discards, discRat, landings and price.
load("outputs_metier_C_interaction_max_AM.RData")
# load("outputs_metier_C_interaction_min_AM.RData")
# load("outputs_metier_C_interaction_mean_AM.RData")
# load("outputs_metier_C_interaction_previous_AM.RData")

## The indicators are: effshare, effort, income and vcost.
# load("outputs_metier_eff_interaction_max_AM.RData")
# load("outputs_metier_eff_interaction_min_AM.RData")
# load("outputs_metier_eff_interaction_mean_AM.RData")
# load("outputs_metier_eff_interaction_previous_AM.RData")

# ================================== OLS Method ==================================

## exp data catch
cc_catch_max <- cc

## prepare data
catch_data_cod <- subset(cc_catch_max, indicator == "catch" & stock =="COD" & year == "2017")
catch_data_cod2 <- ddply(catch_data_cod, .(sim), summarize, sum=sum(value))
catch_data_cod2 <- catch_data_cod2 %>% separate(sim, c("m1", "m2"), "_")
catch_data_cod2$m1 <- str_remove(catch_data_cod2$m1, "m=")
catch_data_cod2$m2 <- str_remove(catch_data_cod2$m2, "m=")
catch_data_cod2 <- transform(catch_data_cod2, m1 = as.numeric(m1))
catch_data_cod2 <- transform(catch_data_cod2, m2 = as.numeric(m2))
catch_data_cod2 <- catch_data_cod2[order(catch_data_cod2$m1, catch_data_cod2$m2),]

C <- matrix(catch_data_cod2$sum, 15, 15)
image.plot(C, col = viridis(30))

q_multiplier <- seq(0.2, 3, by = 0.2)
m1 <- rep(q_multiplier, each = 15)
m2 <- rep(q_multiplier, times = 15)

Cdf_cod <- data.frame(C = catch_data_cod2$sum,  Eff1_multiplier = m1, Eff2_multiplier = m2)

## OLS method (what is this exactly? - see Princeton handout)
Y <- matrix(Cdf_cod$C)
y <- c(Y)      # catches
x <- Cdf_cod$Eff1_multiplier    # effort multiplier TR1 
z <- Cdf_cod$Eff2_multiplier    # effort multiplier TR2 

beta1 <- (cov(x, y) * var(z) - cov(z, y) * cov(x, z)) / (var(x) * var(z) - cov(x,z)^2) 
beta2 <- (cov(z, y) * var(x) - cov(x, y) * cov(z, x)) / (var(x) * var(z) - cov(x,z)^2) 

ind_ols <- abs(beta1) / (abs(beta1) + abs(beta2))

# ================================== linear model method ==================================

fit <- lm(C ~ Eff1_multiplier + Eff2_multiplier, data = Cdf_cod)
beta <- coef(fit)
ind_lm <- abs(beta[2]) / (abs(beta[2]) + abs(beta[3]))

# ind_ols are equal to ind_lm!! :)

