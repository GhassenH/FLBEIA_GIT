####################################################################
# Prepare data for FLBEIA simulations
# tac by species
# Ghassen Halouani
# Februray 2019
######################################################################

age <- "all"
year <- seq(2000, 2016)
unit <- 1
season <- 1
area <- "unique"
iter <- 1

data_tac_stk1 <- read.csv("data/tac data/cod_raw_tac_data.csv")[,2]
data_tac_stk2 <- read.csv("data/tac data/had_raw_tac_data.csv")[,2]
data_tac_stk3 <- read.csv("data/tac data/whg_raw_tac_data.csv")[,2]

# Construction of the table
age2 <- rep(age, times = length(year))
year2 <- rep(year, each = length(age))
unit2 <- rep(unit, times = length(age2))
season2 <- rep(season, times = length(age2))
area2 <- rep(area, times = length(age2))
iter2 <- rep(iter, times = length(age2))


stk1_tac <- data.frame(age = age2, year = year2, unit = unit2, season = season2, area = area2, iter = iter2, data = data_tac_stk1)
stk2_tac <- data.frame(age = age2, year = year2, unit = unit2, season = season2, area = area2, iter = iter2, data = data_tac_stk2)
stk3_tac <- data.frame(age = age2, year = year2, unit = unit2, season = season2, area = area2, iter = iter2, data = data_tac_stk3)

proj <- data.frame(age = age, year = seq(2017, 2030), unit = 1, season = 1, area = "unique", iter = 1, data = "NA")

stk1_tac <- rbind(stk1_tac, proj)
stk2_tac <- rbind(stk2_tac, proj)
stk3_tac <- rbind(stk3_tac, proj)

# Save data
write.csv(stk1_tac, "data/tac data/stk1.tac.csv", row.names = FALSE)
write.csv(stk2_tac, "data/tac data/stk2.tac.csv", row.names = FALSE)
write.csv(stk3_tac, "data/tac data/stk3.tac.csv", row.names = FALSE)


