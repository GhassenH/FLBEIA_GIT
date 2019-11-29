####################################################################
# Prepare data for FLBEIA simulations
# Fishing effort by fleet and effort share for each fleet and métier
# Ghassen Halouani
# Februray 2019
######################################################################

## 1. Fishing effort by fleet 
age <- "all"
year <- seq(2000, 2016)
unit <- "unique"
season <- "all"
area <- "unique"
iter <- 1

eff_fleet <- read.csv("data/fleet component/effort by fleets.csv")
eff_fl1 <- subset(eff_fleet, fleets == "IRL_DTS_O10M")
eff_fl2 <- subset(eff_fleet, fleets == "GBR_DTS_O10M")
eff_fl3 <- subset(eff_fleet, fleets == "FRA_DTS_O10M")
eff_fl4 <- subset(eff_fleet, fleets == "OTH_DTS_O10M")

# Construction of the table
age2 <- rep(age, each = length(year))
year2 <- rep(year, times = length(age))
unit2 <- rep(unit, times = length(age2))
season2 <- rep(season, times = length(age2))
area2 <- rep(area, times = length(age2))
iter2 <- rep(iter, times = length(age2))
data_fl1 <- c(c(0,0,0), eff_fl1$nominal_effort) 
data_fl2 <- c(c(0,0,0), eff_fl2$nominal_effort)
data_fl3 <- c(c(0,0,0), eff_fl3$nominal_effort)
data_fl4 <- c(c(0,0,0), eff_fl4$nominal_effort)
 
# data_fl1 <- c(NA, NA, NA, eff_fl1$nominal_effort) 
# data_fl2 <- c(NA, NA, NA, eff_fl2$nominal_effort)
# data_fl3 <- c(NA, NA, NA, eff_fl3$nominal_effort)
# data_fl4 <- c(NA, NA, NA, eff_fl4$nominal_effort)

eff_fl1 <- data.frame(age = age2, year = year2, unit = unit2, 
                     season = season2, area = area2, 
                     iter = iter2, data = data_fl1)
eff_fl2 <- data.frame(age = age2, year = year2, unit = unit2, 
                      season = season2, area = area2, 
                      iter = iter2, data = data_fl2)
eff_fl3 <- data.frame(age = age2, year = year2, unit = unit2, 
                      season = season2, area = area2, 
                      iter = iter2, data = data_fl3)
eff_fl4 <- data.frame(age = age2, year = year2, unit = unit2, 
                      season = season2, area = area2, 
                      iter = iter2, data = data_fl4)
# Save data
write.csv(eff_fl1, "data/fleet component/fl1_effort.csv", row.names = FALSE)
write.csv(eff_fl2, "data/fleet component/fl2_effort.csv", row.names = FALSE)
write.csv(eff_fl3, "data/fleet component/fl3_effort.csv", row.names = FALSE)
write.csv(eff_fl4, "data/fleet component/fl4_effort.csv", row.names = FALSE)


## 2. Effort share by fleet and métiers 
age <- "all"
year <- seq(2000, 2016)
unit <- "unique"
season <- "all"
area <- "unique"
iter <- 1

eff_share <- read.csv("data/fleet component/effort share TR1_TR2.csv")

# transform the percentage into values between 0 and 1
eff_share$TR1_eff_share <- eff_share$TR1_eff_share/100 
eff_share$TR2_eff_share <- eff_share$TR2_eff_share/100 

eff_share1 <- subset(eff_share, fleets == "IRL_DTS_O10M")
eff_share2 <- subset(eff_share, fleets == "GBR_DTS_O10M")
eff_share3 <- subset(eff_share, fleets == "FRA_DTS_O10M")
eff_share4 <- subset(eff_share, fleets == "OTH_DTS_O10M")

# Construction of the table
age2 <- rep(age, each = length(year))
year2 <- rep(year, times = length(age))
unit2 <- rep(unit, times = length(age2))
season2 <- rep(season, times = length(age2))
area2 <- rep(area, times = length(age2))
iter2 <- rep(iter, times = length(age2))

share_fl1_TR1 <- c(NA, NA, NA, eff_share1$TR1_eff_share)
share_fl2_TR1 <- c(NA, NA, NA, eff_share2$TR1_eff_share)
share_fl3_TR1 <- c(NA, NA, NA, eff_share3$TR1_eff_share)
share_fl4_TR1 <- c(NA, NA, NA, eff_share4$TR1_eff_share)
share_fl1_TR2 <- c(NA, NA, NA, eff_share1$TR2_eff_share)
share_fl2_TR2 <- c(NA, NA, NA, eff_share2$TR2_eff_share)
share_fl3_TR2 <- c(NA, NA, NA, eff_share3$TR2_eff_share)
share_fl4_TR2 <- c(NA, NA, NA, eff_share4$TR2_eff_share)

effshare_fl1_met1 <- data.frame(age = age2, year = year2, unit = unit2, 
                                season = season2, area = area2, 
                                iter = iter2, data = share_fl1_TR1)
effshare_fl2_met1 <- data.frame(age = age2, year = year2, unit = unit2, 
                                season = season2, area = area2, 
                                iter = iter2, data = share_fl2_TR1)
effshare_fl3_met1 <- data.frame(age = age2, year = year2, unit = unit2, 
                                season = season2, area = area2, 
                                iter = iter2, data = share_fl3_TR1)
effshare_fl4_met1 <- data.frame(age = age2, year = year2, unit = unit2, 
                                season = season2, area = area2, 
                                iter = iter2, data = share_fl4_TR1)

effshare_fl1_met2 <- data.frame(age = age2, year = year2, unit = unit2, 
                                season = season2, area = area2, 
                                iter = iter2, data = share_fl1_TR2)
effshare_fl2_met2 <- data.frame(age = age2, year = year2, unit = unit2, 
                                season = season2, area = area2, 
                                iter = iter2, data = share_fl2_TR2)
effshare_fl3_met2 <- data.frame(age = age2, year = year2, unit = unit2, 
                                season = season2, area = area2, 
                                iter = iter2, data = share_fl3_TR2)
effshare_fl4_met2 <- data.frame(age = age2, year = year2, unit = unit2, 
                                season = season2, area = area2, 
                                iter = iter2, data = share_fl4_TR2)

# Save data
write.csv(effshare_fl1_met1, "data/fleet component/fl1.met1_effshare.csv", row.names = FALSE)
write.csv(effshare_fl2_met1, "data/fleet component/fl2.met1_effshare.csv", row.names = FALSE)
write.csv(effshare_fl3_met1, "data/fleet component/fl3.met1_effshare.csv", row.names = FALSE)
write.csv(effshare_fl4_met1, "data/fleet component/fl4.met1_effshare.csv", row.names = FALSE)
write.csv(effshare_fl1_met2, "data/fleet component/fl1.met2_effshare.csv", row.names = FALSE)
write.csv(effshare_fl2_met2, "data/fleet component/fl2.met2_effshare.csv", row.names = FALSE)
write.csv(effshare_fl3_met2, "data/fleet component/fl3.met2_effshare.csv", row.names = FALSE)
write.csv(effshare_fl4_met2, "data/fleet component/fl4.met2_effshare.csv", row.names = FALSE)
