###############################################################################
# TITLE: recalculation of stock-recrutment paramters : 3 stocks (COD, HAD, WHG) and 2 fleets (TR1, TR2)
# Author : Ghassen Halouani
# Date : January 2019
# the srr parameters for the 3 species were estimated using likelihood approach
# and by adding a prior on the steepness parameter h. For the estimation we used
# the following beverton and holt srr : R = ssb/(alpha1 + beta1*ssb)
# However, FLBEIA is using the following srr : R = alpha2*ssb / (beta2 + ssb)
# This code is calculate alpha2 and beta2 from alpha1 and beta1
###############################################################################

# Function
convert_sr_par <- function(alpha1, beta1){
  sr1 <- function(x){ x / (alpha1 + beta1*x) }
  ssb <- seq(0, 10e5, by = 100)
  rec_est <- sr1(ssb)
  
  # new parameters alpha2 and beta2
  alpha2 <- 1/beta1
  
  beta2 <- (alpha2*ssb/rec_est)-ssb
  beta2 <- beta2[2]
  
  par <- c(alpha2, beta2)
  return(par)
}

# par 1 = alpha
# par 2 = beta

########## WHG ##########
alpha_whg <- 0.002618657
beta_whg <- 8.829825e-07

par_sr_whg <- convert_sr_par(alpha_whg, beta_whg)
  
########### COD ##########
alpha_cod <- 0.3820993
beta_cod  <-0.0001211078

par_sr_cod <- convert_sr_par(alpha_cod, beta_cod)

########### HAD ##########

alpha_had <- 0.02473398
beta_had  <-1.777957e-06

par_sr_had <- convert_sr_par(alpha_had, beta_had)

# ============================================================
# # Prepartion of stk_params.csv file
# 
# param <- c("a", "b")
# year <- seq(2000, 2016)
# unit <- "unique"
# season <- 1
# area <- "unique"
# iter <- 1
# data <- NA
# 
# # construction of the table
# param2 <- rep(param, times = length(year))
# year2  <- rep(year, each = length(param))
# 
# stk1_params <- data.frame(param=param2, year=year2, unit=unit,
#                           season=season, area=area, iter=iter,
#                           data=par_sr_cod)
# 
# stk2_params <- data.frame(param=param2, year=year2, unit=unit,
#                           season=season, area=area, iter=iter,
#                           data=par_sr_had)
# 
# stk3_params <- data.frame(param=param2, year=year2, unit=unit,
#                           season=season, area=area, iter=iter,
#                           data=par_sr_whg)
# 
# write.csv(stk1_params, "data/cod/stk1_params.csv", row.names = FALSE)
# write.csv(stk2_params, "data/had/stk2_params.csv", row.names = FALSE)
# write.csv(stk3_params, "data/whg/stk3_params.csv", row.names = FALSE)

# ============================================================
# Prepartion of stk_params.csv file

param <- c("a", "b")
year <- seq(2000, 2016)
year_projection <- seq(2017, 2030)
unit <- "unique"
season <- 1
area <- "unique"
iter <- 1
data <- NA

# construction of the table for 2000-2016
param2 <- rep(param, times = length(year))
year2  <- rep(year, each = length(param))

data_NA <-  data.frame(param=param2, year=year2, unit=unit,
                          season=season, area=area, iter=iter,
                          data=data)

# construction of the table for 2017-2030
param3 <- rep(param, times = length(year_projection))
year3  <- rep(year_projection, each = length(param))

stk1_params <- data.frame(param=param3, year=year3, unit=unit,
                          season=season, area=area, iter=iter,
                          data=par_sr_cod)
stk1_params <- rbind(data_NA, stk1_params)

stk2_params <- data.frame(param=param3, year=year3, unit=unit,
                          season=season, area=area, iter=iter,
                          data=par_sr_had)
stk2_params <- rbind(data_NA, stk2_params)

stk3_params <- data.frame(param=param3, year=year3, unit=unit,
                          season=season, area=area, iter=iter,
                          data=par_sr_whg)
stk3_params <- rbind(data_NA, stk3_params)

write.csv(stk1_params, "data/cod/stk1_params.csv", row.names = FALSE)
write.csv(stk2_params, "data/had/stk2_params.csv", row.names = FALSE)
write.csv(stk3_params, "data/whg/stk3_params.csv", row.names = FALSE)
