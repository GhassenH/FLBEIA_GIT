######################################################################################
# preparing indices
######################################################################################

library(FLCore)

# COD index by age
indices <- read.csv("data/indices/cod_index_age.csv", row.names = 1)
missing <- data.frame(matrix(0, ncol= ncol(indices), nrow = 3)) #years 2000, 2001 and 2002 are missing
colnames(missing) <- colnames(indices)
indices2 <- rbind(missing, indices)
indices3 <- t(as.matrix(indices2))
indices4 <- FLQuant(indices3, dimnames=list(age=1:7, year = 2000:2016))
FL_indices <- FLIndex(index = indices4)
# plot(FL_indices)

# COD index biomass
indices_b <- apply(indices, 1, sum)
indices_b2 <- FLQuant(indices_b, dimnames=list(age="all", year = 2000:2016))
FL_indices_B <- FLIndexBiomass(index = indices_b2)


# whg index by biomass
indices_whg <- read.csv("data/indices/whg_index_age.csv", row.names = 1)
# COD index biomass
indices_whg_b <- apply(indices_whg, 1, sum, na.rm = T)
indices_whg_b2 <- FLQuant(indices_whg_b, dimnames=list(age="all", year = 2000:2016))
FL_indices_B <- FLIndexBiomass(index = indices_whg_b2)

FL_indices_B@index
