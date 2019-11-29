####################################################################
# Prepare data for FLBEIA simulations
# Ghassen Halouani
# January 2019
######################################################################
library(tidyr)

### had ###
# =================== number at age  =================== 
age <- seq(0, 8)
year <- seq(2000, 2016)
unit <- 1
season <- "all"
area <- "unique"
iter <- 1
data_n <- read.csv("data/had/raw_data_number_at_age_had.csv")
data_n_2000 <- data_n[data_n$year>=2000,]
data_n_2000_g <- gather(data_n_2000, key = year) 
data_n_2000_g2 <- data_n_2000_g[,2]
# data_n_2000_g2 <- data_n_2000_g2 * 1000

# Construction of the table
age2 <- rep(age, each = length(year))
year2 <- rep(year, times = length(age))
unit2 <- rep(unit, times = length(age2))
season2 <- rep(season, times = length(age2))
area2 <- rep(area, times = length(age2))
iter2 <- rep(iter, times = length(age2))
data2 <- rep(data_n_2000_g2)

stk2_n <- data.frame(age = age2, year = year2, unit = unit2, 
                     season = season2, area = area2, 
                     iter = iter2, data = data2)

# Save data
write.csv(stk2_n, "data/had/stk2_n.csv", row.names = FALSE)

# =================== natural mortality m at age  =================== 
age <- seq(0, 8)
year <- seq(2000, 2016)
unit <- 1
season <- "all"
area <- "unique"
iter <- 1
data_m <- read.csv("data/had/raw_data_m_had.csv")
data_m <- data_m[,2]

# Construction of the table
age2 <- rep(age, times = length(year))
year2 <- rep(year, each = length(age))
unit2 <- rep(unit, times = length(age2))
season2 <- rep(season, times = length(age2))
area2 <- rep(area, times = length(age2))
iter2 <- rep(iter, times = length(age2))
data2 <- data_m

stk2_m <- data.frame(age = age2, year = year2, unit = unit2, 
                     season = season2, area = area2, 
                     iter = iter2, data = data2)

# Save data
write.csv(stk2_m, "data/had/stk2_m.csv", row.names = FALSE)

# =================== spwn at age  =================== 
age <- seq(0, 8)
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

stk2_spwn <- data.frame(age = age2, year = year2, unit = unit2, 
                        season = season2, area = area2, 
                        iter = iter2, data = data2)

# Save data
write.csv(stk2_spwn, "data/had/stk2_spwn.csv", row.names = FALSE)

# =================== mat at age  =================== 
age <- seq(0, 8)
year <- seq(2000, 2016)
unit <- 1
season <- "all"
area <- "unique"
iter <- 1
data_mat <- read.csv("data/had/raw_data_mat_had.csv")
data_mat <- data_mat[,2]

# Construction of the table
age2 <- rep(age, times = length(year))
year2 <- rep(year, each = length(age))
unit2 <- rep(unit, times = length(age2))
season2 <- rep(season, times = length(age2))
area2 <- rep(area, times = length(age2))
iter2 <- rep(iter, times = length(age2))
data2 <- data_mat

stk2_mat <- data.frame(age = age2, year = year2, unit = unit2, 
                       season = season2, area = area2, 
                       iter = iter2, data = data2)

# Save data
write.csv(stk2_mat, "data/had/stk2_mat.csv", row.names = FALSE)

# =================== wt  =================== 
age <- seq(0, 8)
year <- seq(2000, 2016)
unit <- 1
season <- "all"
area <- "unique"
iter <- 1
data_wt <- read.csv("data/had/raw_data_wt_had.csv")
data_wt <- data_wt[,2]

# Construction of the table
age2 <- rep(age, times = length(year))
year2 <- rep(year, each = length(age))
unit2 <- rep(unit, times = length(age2))
season2 <- rep(season, times = length(age2))
area2 <- rep(area, times = length(age2))
iter2 <- rep(iter, times = length(age2))
data2 <- data_wt

stk2_wt <- data.frame(age = age2, year = year2, unit = unit2, 
                      season = season2, area = area2, 
                      iter = iter2, data = data2)

# Save data
write.csv(stk2_wt, "data/had/stk2_wt.csv", row.names = FALSE)

### had ###
# =================== wt at age  =================== 
age <- seq(0, 8)
year <- seq(2000, 2016)
unit <- 1
season <- "all"
area <- "unique"
iter <- 1
data_wt <- read.csv("data/had/raw_data_wt_at_age_had.csv")
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

stk2_wt <- data.frame(age = age2, year = year2, unit = unit2, 
                     season = season2, area = area2, 
                     iter = iter2, data = data2)

# Save data
write.csv(stk2_wt, "data/had/stk2_wt.csv", row.names = FALSE)

# =================== ssb and rec  =================== 

age <- 0
year <- seq(2000, 2016)
unit <- 1
season <- "all"
area <- "unique"
iter <- 1
data <- read.csv("data/had/raw_data_rec_ssb_had.csv") 
data_2000 <- data[data$year>=2000,]


stk2_rec <- data.frame(age = age, year = year, unit = unit, 
                       season = season, area = area, 
                       iter = iter, data = data_2000$rec)

stk2_ssb <- data.frame(age = "all", year = year, unit = unit, 
                       season = season, area = area, 
                       iter = iter, data = data_2000$ssb)

write.csv(stk2_rec, "data/had/stk2_rec.csv", row.names = FALSE)
write.csv(stk2_ssb, "data/had/stk2_ssb.csv", row.names = FALSE)

# =================== uncertainty  =================== 

age <- 0
year <- seq(2000, 2030)
unit <- 1
season <- "all"
area <- "unique"
iter <- 1
data <- 1


stk2_uncertainty <- data.frame(age = age, year = year, unit = unit, 
                       season = season, area = area, 
                       iter = iter, data = data)

write.csv(stk2_uncertainty, "data/had/stk2_uncertainty.csv", row.names = FALSE)


# =================== proportion  =================== 

age <- "all"
year <- seq(2000, 2030)
unit <- 1
season <- "all"
area <- "unique"
iter <- 1
data <- 1


stk2_proportion <- data.frame(age = age, year = year, unit = unit, 
                               season = season, area = area, 
                               iter = iter, data = data)

write.csv(stk2_proportion, "data/had/stk2_proportion.csv", row.names = FALSE)
