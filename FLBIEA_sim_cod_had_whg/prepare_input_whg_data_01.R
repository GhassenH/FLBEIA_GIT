####################################################################
# Prepare data for FLBEIA simulations
# Ghassen Halouani
# January 2019
######################################################################
library(tidyr)

### whg ###
# =================== number at age  =================== 
age <- seq(0, 7)
year <- seq(2000, 2016)
unit <- 1
season <- "all"
area <- "unique"
iter <- 1
data_n <- read.csv("data/whg/raw_data_number_at_age_whg.csv")
# aa <- data.frame(year = 2000:2006, Age_0=0, Age_1=0, Age_2=0, Age_3=0, Age_4=0, Age_5=0, Age_6=0, Age_7=0)
aa <- data.frame(year = 2000:2006, Age_0=NA, Age_1=NA, Age_2=NA, Age_3=NA, Age_4=NA, Age_5=NA, Age_6=NA, Age_7=NA)
data_n <- rbind(aa,data_n)
data_n_2000 <- data_n[data_n$year>=2000,]
data_n_2000_g <- gather(data_n_2000, key = year) 
data_n_2000_g2 <- data_n_2000_g[,2]
# data_n_2000_g2 <- data_n_2000_g2*1000 # adding unit

# Construction of the table
age2 <- rep(age, each = length(year))
year2 <- rep(year, times = length(age))
unit2 <- rep(unit, times = length(age2))
season2 <- rep(season, times = length(age2))
area2 <- rep(area, times = length(age2))
iter2 <- rep(iter, times = length(age2))
data2 <- rep(data_n_2000_g2)

stk3_n <- data.frame(age = age2, year = year2, unit = unit2, 
                     season = season2, area = area2, 
                     iter = iter2, data = data2)

# Save data
write.csv(stk3_n, "data/whg/stk3_n.csv", row.names = FALSE)

# =================== natural mortality m at age  =================== 
age <- seq(0, 7)
year <- seq(2000, 2016)
unit <- 1
season <- "all"
area <- "unique"
iter <- 1
data_m <- read.csv("data/whg/raw_data_m_whg.csv")
data_m <- data_m[,2]

# Construction of the table
age2 <- rep(age, times = length(year))
year2 <- rep(year, each = length(age))
unit2 <- rep(unit, times = length(age2))
season2 <- rep(season, times = length(age2))
area2 <- rep(area, times = length(age2))
iter2 <- rep(iter, times = length(age2))
data2 <- rep(data_m, times =  length(year))

stk3_m <- data.frame(age = age2, year = year2, unit = unit2, 
                     season = season2, area = area2, 
                     iter = iter2, data = data2)

# Save data
write.csv(stk3_m, "data/whg/stk3_m.csv", row.names = FALSE)

# =================== spwn at age  =================== 
age <- seq(0, 7)
year <- seq(2000, 2016)
unit <- 1
season <- "all"
area <- "unique"
iter <- 1
data <- 0

# Construction of the table
age2 <- rep(age, times = length(year))
year2 <- rep(year, each = length(age))
unit2 <- rep(unit, times = length(age2))
season2 <- rep(season, times = length(age2))
area2 <- rep(area, times = length(age2))
iter2 <- rep(iter, times = length(age2))
data2 <- data

stk3_spwn <- data.frame(age = age2, year = year2, unit = unit2, 
                        season = season2, area = area2, 
                        iter = iter2, data = data2)

# Save data
write.csv(stk3_spwn, "data/whg/stk3_spwn.csv", row.names = FALSE)

# =================== mat at age  =================== 
age <- seq(0, 7)
year <- seq(2000, 2016)
unit <- 1
season <- "all"
area <- "unique"
iter <- 1
data_mat <- read.csv("data/whg/raw_data_mat_whg.csv")
data_mat <- data_mat[,2]

# Construction of the table
age2 <- rep(age, times = length(year))
year2 <- rep(year, each = length(age))
unit2 <- rep(unit, times = length(age2))
season2 <- rep(season, times = length(age2))
area2 <- rep(area, times = length(age2))
iter2 <- rep(iter, times = length(age2))
data2 <- data_mat

stk3_mat <- data.frame(age = age2, year = year2, unit = unit2, 
                       season = season2, area = area2, 
                       iter = iter2, data = data2)

# Save data
write.csv(stk3_mat, "data/whg/stk3_mat.csv", row.names = FALSE)

# =================== wt at age  =================== 
age <- seq(0, 7)
year <- seq(2000, 2016)
unit <- 1
season <- "all"
area <- "unique"
iter <- 1
data_wt <- read.csv("data/whg/raw_data_wt_whg.csv")
data_wt_2000 <- data_wt[data_wt$year>=2000,]
data_wt_2000_g <- gather(data_wt_2000, key = year) 
data_wt_2000_g2 <- data_wt_2000_g[,2]

# Construction of the table
age2 <- rep(age, each = length(year))
year2 <- rep(year, times = length(age))
unit2 <- rep(unit, times = length(age2))
season2 <- rep(season, times = length(age2))
area2 <- rep(area, times = length(age2))
iter2 <- rep(iter, times = length(age2))
data2 <- rep(data_wt_2000_g2)

stk3_wt <- data.frame(age = age2, year = year2, unit = unit2, 
                      season = season2, area = area2, 
                      iter = iter2, data = data2)

write.csv(stk3_wt, "data/whg/stk3_wt.csv", row.names = FALSE)

# =================== ssb and rec  =================== 

age <- 0
year <- seq(2000, 2016)
unit <- 1
season <- "all"
area <- "unique"
iter <- 1
data <- read.csv("data/whg/raw_data_rec_ssb_whg.csv") 
data_2000 <- data[data$year>=2000,]


stk3_rec <- data.frame(age = age, year = year, unit = unit, 
                       season = season, area = area, 
                       iter = iter, data = data_2000$rec)

stk3_ssb <- data.frame(age = "all", year = year, unit = unit, 
                       season = season, area = area, 
                       iter = iter, data = data_2000$ssb)

write.csv(stk3_rec, "data/whg/stk3_rec.csv", row.names = FALSE)
write.csv(stk3_ssb, "data/whg/stk3_ssb.csv", row.names = FALSE)

# =================== uncertainty  ===================

age <- 0
year <- seq(2000, 2030)
unit <- 1
season <- "all"
area <- "unique"
iter <- 1
data <- 1


stk3_uncertainty <- data.frame(age = age, year = year, unit = unit, 
                               season = season, area = area, 
                               iter = iter, data = data)

write.csv(stk3_uncertainty, "data/whg/stk3_uncertainty.csv", row.names = FALSE)

# =================== proportion  ===================

age <- "all"
year <- seq(2000, 2030)
unit <- 1
season <- "all"
area <- "unique"
iter <- 1
data <- 1


stk3_proportion <- data.frame(age = age, year = year, unit = unit, 
                               season = season, area = area, 
                               iter = iter, data = data)

write.csv(stk3_proportion, "data/whg/stk3_proportion.csv", row.names = FALSE)

