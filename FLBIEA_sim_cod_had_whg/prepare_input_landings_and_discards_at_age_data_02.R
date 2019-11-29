####################################################################
# Prepare data for FLBEIA simulations
# land_disc and discards by species, fleet and métier
# Ghassen Halouani
# Februray 2019
######################################################################

## 1. land_disc by species, fleet and métiers 
age <- seq(0, 11)
year <- seq(2003, 2016)
unit <- 1
season <- 1
area <- "unique"
iter <- 1

# read data
land_disc <- read.csv("data/landings and discards/landings and discards at age.csv")

# subset landings
land_fl1_cod_met1 <- subset(land_disc, fleets == "IRL_DTS_O10M" & species == "COD" & regulated.gear == "TR1" & type == "l")
land_fl2_cod_met1 <- subset(land_disc, fleets == "GBR_DTS_O10M" & species == "COD" & regulated.gear == "TR1" & type == "l")
land_fl3_cod_met1 <- subset(land_disc, fleets == "FRA_DTS_O10M" & species == "COD" & regulated.gear == "TR1" & type == "l")
land_fl4_cod_met1 <- subset(land_disc, fleets == "OTH_DTS_O10M" & species == "COD" & regulated.gear == "TR1" & type == "l")

land_fl1_had_met1 <- subset(land_disc, fleets == "IRL_DTS_O10M" & species == "HAD" & regulated.gear == "TR1" & type == "l")
land_fl2_had_met1 <- subset(land_disc, fleets == "GBR_DTS_O10M" & species == "HAD" & regulated.gear == "TR1" & type == "l")
land_fl3_had_met1 <- subset(land_disc, fleets == "FRA_DTS_O10M" & species == "HAD" & regulated.gear == "TR1" & type == "l")
land_fl4_had_met1 <- subset(land_disc, fleets == "OTH_DTS_O10M" & species == "HAD" & regulated.gear == "TR1" & type == "l")

land_fl1_whg_met1 <- subset(land_disc, fleets == "IRL_DTS_O10M" & species == "WHG" & regulated.gear == "TR1" & type == "l")
land_fl2_whg_met1 <- subset(land_disc, fleets == "GBR_DTS_O10M" & species == "WHG" & regulated.gear == "TR1" & type == "l")
land_fl3_whg_met1 <- subset(land_disc, fleets == "FRA_DTS_O10M" & species == "WHG" & regulated.gear == "TR1" & type == "l")
land_fl4_whg_met1 <- subset(land_disc, fleets == "OTH_DTS_O10M" & species == "WHG" & regulated.gear == "TR1" & type == "l")

land_fl1_cod_met2 <- subset(land_disc, fleets == "IRL_DTS_O10M" & species == "COD" & regulated.gear == "TR2" & type == "l")
land_fl2_cod_met2 <- subset(land_disc, fleets == "GBR_DTS_O10M" & species == "COD" & regulated.gear == "TR2" & type == "l")
land_fl3_cod_met2 <- subset(land_disc, fleets == "FRA_DTS_O10M" & species == "COD" & regulated.gear == "TR2" & type == "l")
land_fl4_cod_met2 <- subset(land_disc, fleets == "OTH_DTS_O10M" & species == "COD" & regulated.gear == "TR2" & type == "l")

land_fl1_had_met2 <- subset(land_disc, fleets == "IRL_DTS_O10M" & species == "HAD" & regulated.gear == "TR2" & type == "l")
land_fl2_had_met2 <- subset(land_disc, fleets == "GBR_DTS_O10M" & species == "HAD" & regulated.gear == "TR2" & type == "l")
land_fl3_had_met2 <- subset(land_disc, fleets == "FRA_DTS_O10M" & species == "HAD" & regulated.gear == "TR2" & type == "l")
land_fl4_had_met2 <- subset(land_disc, fleets == "OTH_DTS_O10M" & species == "HAD" & regulated.gear == "TR2" & type == "l")

land_fl1_whg_met2 <- subset(land_disc, fleets == "IRL_DTS_O10M" & species == "WHG" & regulated.gear == "TR2" & type == "l")
land_fl2_whg_met2 <- subset(land_disc, fleets == "GBR_DTS_O10M" & species == "WHG" & regulated.gear == "TR2" & type == "l")
land_fl3_whg_met2 <- subset(land_disc, fleets == "FRA_DTS_O10M" & species == "WHG" & regulated.gear == "TR2" & type == "l")
land_fl4_whg_met2 <- subset(land_disc, fleets == "OTH_DTS_O10M" & species == "WHG" & regulated.gear == "TR2" & type == "l")

# subset discards
disc_fl1_cod_met1 <- subset(land_disc, fleets == "IRL_DTS_O10M" & species == "COD" & regulated.gear == "TR1" & type == "d")
disc_fl2_cod_met1 <- subset(land_disc, fleets == "GBR_DTS_O10M" & species == "COD" & regulated.gear == "TR1" & type == "d")
disc_fl3_cod_met1 <- subset(land_disc, fleets == "FRA_DTS_O10M" & species == "COD" & regulated.gear == "TR1" & type == "d")
disc_fl4_cod_met1 <- subset(land_disc, fleets == "OTH_DTS_O10M" & species == "COD" & regulated.gear == "TR1" & type == "d")

disc_fl1_had_met1 <- subset(land_disc, fleets == "IRL_DTS_O10M" & species == "HAD" & regulated.gear == "TR1" & type == "d")
disc_fl2_had_met1 <- subset(land_disc, fleets == "GBR_DTS_O10M" & species == "HAD" & regulated.gear == "TR1" & type == "d")
disc_fl3_had_met1 <- subset(land_disc, fleets == "FRA_DTS_O10M" & species == "HAD" & regulated.gear == "TR1" & type == "d")
disc_fl4_had_met1 <- subset(land_disc, fleets == "OTH_DTS_O10M" & species == "HAD" & regulated.gear == "TR1" & type == "d")

disc_fl1_whg_met1 <- subset(land_disc, fleets == "IRL_DTS_O10M" & species == "WHG" & regulated.gear == "TR1" & type == "d")
disc_fl2_whg_met1 <- subset(land_disc, fleets == "GBR_DTS_O10M" & species == "WHG" & regulated.gear == "TR1" & type == "d")
disc_fl3_whg_met1 <- subset(land_disc, fleets == "FRA_DTS_O10M" & species == "WHG" & regulated.gear == "TR1" & type == "d")
disc_fl4_whg_met1 <- subset(land_disc, fleets == "OTH_DTS_O10M" & species == "WHG" & regulated.gear == "TR1" & type == "d")

disc_fl1_cod_met2 <- subset(land_disc, fleets == "IRL_DTS_O10M" & species == "COD" & regulated.gear == "TR2" & type == "d")
disc_fl2_cod_met2 <- subset(land_disc, fleets == "GBR_DTS_O10M" & species == "COD" & regulated.gear == "TR2" & type == "d")
disc_fl3_cod_met2 <- subset(land_disc, fleets == "FRA_DTS_O10M" & species == "COD" & regulated.gear == "TR2" & type == "d")
disc_fl4_cod_met2 <- subset(land_disc, fleets == "OTH_DTS_O10M" & species == "COD" & regulated.gear == "TR2" & type == "d")

disc_fl1_had_met2 <- subset(land_disc, fleets == "IRL_DTS_O10M" & species == "HAD" & regulated.gear == "TR2" & type == "d")
disc_fl2_had_met2 <- subset(land_disc, fleets == "GBR_DTS_O10M" & species == "HAD" & regulated.gear == "TR2" & type == "d")
disc_fl3_had_met2 <- subset(land_disc, fleets == "FRA_DTS_O10M" & species == "HAD" & regulated.gear == "TR2" & type == "d")
disc_fl4_had_met2 <- subset(land_disc, fleets == "OTH_DTS_O10M" & species == "HAD" & regulated.gear == "TR2" & type == "d")

disc_fl1_whg_met2 <- subset(land_disc, fleets == "IRL_DTS_O10M" & species == "WHG" & regulated.gear == "TR2" & type == "d")
disc_fl2_whg_met2 <- subset(land_disc, fleets == "GBR_DTS_O10M" & species == "WHG" & regulated.gear == "TR2" & type == "d")
disc_fl3_whg_met2 <- subset(land_disc, fleets == "FRA_DTS_O10M" & species == "WHG" & regulated.gear == "TR2" & type == "d")
disc_fl4_whg_met2 <- subset(land_disc, fleets == "OTH_DTS_O10M" & species == "WHG" & regulated.gear == "TR2" & type == "d")

# Construction of the table
age2 <- rep(age, each = length(year))
year2 <- rep(year, times = length(age))
unit2 <- rep(unit, times = length(age2))
season2 <- rep(season, times = length(age2))
area2 <- rep(area, times = length(age2))
iter2 <- rep(iter, times = length(age2))

# Construction of the table of the missing years (2000 - 2002)
age3 <- rep(age, 3)
year3 <- rep(seq(2000, 2002), each = length(age))
unit3 <- rep(unit, times = length(age3))
season3 <- rep(season, times = length(age3))
area3 <- rep(area, times = length(age3))
iter3 <- rep(iter, times = length(age3))
data3 <- rep(0, each = length(age3))

missing_years <- data.frame(age = age3, year = year3, unit = unit3, season = season3, area = area3, iter = iter3, data = data3)

# function to reorgonize data for fleets 1, 2 and 3 
create_table <- function(data){
  a <- data.frame(age = age2, year = year2, unit = unit2, season = season2, 
                  area = area2, iter = iter2, data = data$value)
  a <- rbind(missing_years, a)
  a <- a[with(a, order(age, year)),]
  return(a)
}

file2read2 <- list(land_fl1_cod_met1, land_fl2_cod_met1, land_fl3_cod_met1, 
                   land_fl1_had_met1, land_fl2_had_met1, land_fl3_had_met1,  
                   land_fl1_whg_met1, land_fl2_whg_met1, land_fl3_whg_met1, 
                   land_fl1_cod_met2, land_fl2_cod_met2, land_fl3_cod_met2,  
                   land_fl1_had_met2, land_fl2_had_met2, land_fl3_had_met2,  
                   land_fl1_whg_met2, land_fl2_whg_met2, land_fl3_whg_met2, 
                   disc_fl1_cod_met1, disc_fl2_cod_met1, disc_fl3_cod_met1, 
                   disc_fl1_had_met1, disc_fl2_had_met1, disc_fl3_had_met1,  
                   disc_fl1_whg_met1, disc_fl2_whg_met1, disc_fl3_whg_met1, 
                   disc_fl1_cod_met2, disc_fl2_cod_met2, disc_fl3_cod_met2,  
                   disc_fl1_had_met2, disc_fl2_had_met2, disc_fl3_had_met2,  
                   disc_fl1_whg_met2, disc_fl2_whg_met2, disc_fl3_whg_met2) 

file2write2 <- list("fl1.met1.stk1_landings.n.csv", "fl2.met1.stk1_landings.n.csv", "fl3.met1.stk1_landings.n.csv", 
                    "fl1.met1.stk2_landings.n.csv", "fl2.met1.stk2_landings.n.csv", "fl3.met1.stk2_landings.n.csv",  
                    "fl1.met1.stk3_landings.n.csv", "fl2.met1.stk3_landings.n.csv", "fl3.met1.stk3_landings.n.csv",  
                    "fl1.met2.stk1_landings.n.csv", "fl2.met2.stk1_landings.n.csv", "fl3.met2.stk1_landings.n.csv", 
                    "fl1.met2.stk2_landings.n.csv", "fl2.met2.stk2_landings.n.csv", "fl3.met2.stk2_landings.n.csv",  
                    "fl1.met2.stk3_landings.n.csv", "fl2.met2.stk3_landings.n.csv", "fl3.met2.stk3_landings.n.csv", 
                    "fl1.met1.stk1_discards.n.csv", "fl2.met1.stk1_discards.n.csv", "fl3.met1.stk1_discards.n.csv", 
                    "fl1.met1.stk2_discards.n.csv", "fl2.met1.stk2_discards.n.csv", "fl3.met1.stk2_discards.n.csv",  
                    "fl1.met1.stk3_discards.n.csv", "fl2.met1.stk3_discards.n.csv", "fl3.met1.stk3_discards.n.csv",  
                    "fl1.met2.stk1_discards.n.csv", "fl2.met2.stk1_discards.n.csv", "fl3.met2.stk1_discards.n.csv", 
                    "fl1.met2.stk2_discards.n.csv", "fl2.met2.stk2_discards.n.csv", "fl3.met2.stk2_discards.n.csv",  
                    "fl1.met2.stk3_discards.n.csv", "fl2.met2.stk3_discards.n.csv", "fl3.met2.stk3_discards.n.csv") 

for (i in 1:length(file2read2)) {
  c <- create_table(file2read2[[i]])
  write.csv(c, paste("data/landings and discards/", file2write2[i], sep=""), row.names = FALSE)
  print(i)
}

# function to reorgonize data for fleet 4 (other countries since the tables are incomplete)
create_table_fl4 <- function(data) {
  
  # the missing years 
  year4 <- seq(2000, 2016)
  year5 <- as.numeric(levels(factor(data$year)))
  year4 <- year4[!(year4 %in% year5)]
  age4 <- rep(age, each = length(year4))
  unit4 <- rep(unit, times = length(age4))
  season4 <- rep(season, times = length(age4))
  area4 <- rep(area, times = length(age4))
  iter4 <- rep(iter, times = length(age4))
  data4 <- rep(0, each = length(age4))
  missing_years4 <- data.frame(age = age4, year = year4, unit = unit4, season = season4, area = area4, iter = iter4, data = data4)
  
  # Construction of the table of the data
  age5 <- rep(age, each =length(year5))
  unit5 <- rep(unit, times = length(age5))
  season5 <- rep(season, times = length(age5))
  area5 <- rep(area, times = length(age5))
  iter5 <- rep(iter, times = length(age5))
  data5 <- data$value
  data5 <- data.frame(age = age5, year = year5, unit = unit5, season = season5, area = area5, iter = iter5, data = data5)
  
  cc2 <- rbind(missing_years4, data5) # recompose teh data frame (data + missing data)
  cc2 <- cc2[with(cc2, order(age, year)),] # reordering the data
  return(cc2)
}

file2read2_fl4   <- list(land_fl4_cod_met1, land_fl4_cod_met2,
                         land_fl4_had_met1, land_fl4_had_met2, 
                         land_fl4_whg_met1, land_fl4_whg_met1, 
                         disc_fl4_cod_met1, disc_fl4_cod_met2, 
                         disc_fl4_had_met1, disc_fl4_had_met2, 
                         disc_fl4_whg_met1, disc_fl4_whg_met1)

file2write2_fl4  <- list("fl4.met1.stk1_landings.n.csv", "fl4.met2.stk1_landings.n.csv", 
                         "fl4.met1.stk2_landings.n.csv", "fl4.met2.stk2_landings.n.csv",
                         "fl4.met1.stk3_landings.n.csv", "fl4.met2.stk3_landings.n.csv",
                         "fl4.met1.stk1_discards.n.csv", "fl4.met2.stk1_discards.n.csv", 
                         "fl4.met1.stk2_discards.n.csv", "fl4.met2.stk2_discards.n.csv",
                         "fl4.met1.stk3_discards.n.csv", "fl4.met2.stk3_discards.n.csv")  

for (i in 1:length(file2read2_fl4)) {
  c <- create_table_fl4(file2read2_fl4[[i]])
  write.csv(c, paste("data/landings and discards/", file2write2_fl4[i], sep=""), row.names = FALSE)
  print(i)
}

