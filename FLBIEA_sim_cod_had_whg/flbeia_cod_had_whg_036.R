###############################################################################
# TITLE: FLBEIA simulation : 3 stocks (COD, HAD, WHG) and 2 fleets (TR1, TR2)
# 
# Author : Ghassen Halouani
# 
# Data : from ICES WGCSE REPORT 2017 and stecf website (https://stecf.jrc.ec.europa.eu/web/stecf/dd/effort/graphs-annex)
# 
# Created : 13-03-2019
#
# Stocks : COD, HAD, WHG
# 
# Fleets : IRE = Irish Demersal trawl
#          UK = UK Demersal trawl
#          FRA = French Demersal trawl
#          OTH = Other countries Demersal trawl
# 
# Métiers : TR1 = mesh >100mm (targeting cod, had, whg)
#           TR2 = mesh > 70mm and < 100 mm (targeting nephrops) 
# 
# ---------------                  Notes                  ---------------------
# Capacity : in this example, the capacity is equal to the nominal effort * 3
###############################################################################

# Libraries
library(FLCore) 
library(FLAssess)
library(FLash)
library(FLFleet)
library(FLXSA)
library(FLBEIA) 
library(ggplotFL)
library(spict)
library(ggplot2)

#########################################
##### 1.0 Set Simulation parameters #####
#########################################

# Time
first.yr <- 2000
proj.yr  <- 2017 
last.yr  <- 2030  
yrs      <- c(first.yr=first.yr, proj.yr=proj.yr, last.yr=last.yr)
proj.avg.yrs <- c(2013:2016)

# Names 
fls   <- c("IRE", "UK", "FRA", "OTH")
stks  <- c("COD", "HAD", "WHG")

IRE.mets      <- c("TR1","TR2")
IRE.TR1.stks <- c("COD", "HAD", "WHG")
IRE.TR2.stks <- c("COD", "HAD", "WHG")

UK.mets      <- c("TR1", "TR2")
UK.TR1.stks <- c("COD", "HAD", "WHG")
UK.TR2.stks <- c("COD", "HAD", "WHG")

FRA.mets      <- c("TR1", "TR2")
FRA.TR1.stks <- c("COD", "HAD", "WHG")
FRA.TR2.stks <- c("COD", "HAD", "WHG")

OTH.mets      <- c("TR1", "TR2")
OTH.TR1.stks <- c("COD", "HAD", "WHG")
OTH.TR2.stks <- c("COD", "HAD", "WHG")

# Iteration
ni <- 1

# Saison
ns <- 1

# Age
COD.age.min <- 1
COD.age.max <- 7
COD.unit    <- 1

HAD.age.min <- 0
HAD.age.max <- 8
HAD.unit    <- 1

WHG.age.min <- 0
WHG.age.max <- 7
WHG.unit    <- 1

# =============================================================
#                      -1- OPERATING MODEL 
# =============================================================

############################################
##### 1.1 Biological component : biols ##### 
############################################

# COD input parameters
COD_n.flq     <- as.FLQuant(read.csv(file = "data/cod/stk1_n.csv"))
COD_m.flq     <- as.FLQuant(read.csv(file = "data/cod/stk1_m.csv"))
COD_spwn.flq  <- as.FLQuant(read.csv(file = "data/cod/stk1_spwn.csv"))
COD_mat.flq   <- as.FLQuant(read.csv(file = "data/cod/stk1_mat.csv"))
COD_fec.flq   <- COD_mat.flq
COD_fec.flq[] <- 1
COD_wt.flq    <- as.FLQuant(read.csv(file = "data/cod/stk1_wt.csv"))

COD_range.min       <- 1
COD_range.max       <- 7
COD_range.plusgroup <- 7
COD_range.minyear   <- 2000
COD_range.minfbar   <- 2
COD_range.maxfbar   <- 5

# HAD input parameters
HAD_n.flq     <- as.FLQuant(read.csv(file = "data/had/stk2_n.csv"))
HAD_m.flq     <- as.FLQuant(read.csv(file = "data/had/stk2_m.csv"))
HAD_spwn.flq  <- as.FLQuant(read.csv(file = "data/had/stk2_spwn.csv"))
HAD_mat.flq   <- as.FLQuant(read.csv(file = "data/had/stk2_mat.csv"))
HAD_fec.flq   <- HAD_mat.flq
HAD_fec.flq[] <- 1
HAD_wt.flq    <- as.FLQuant(read.csv(file = "data/had/stk2_wt.csv"))

HAD_range.min       <- 0
HAD_range.max       <- 8
HAD_range.plusgroup <- 8
HAD_range.minyear   <- 2000
HAD_range.minfbar   <- 3
HAD_range.maxfbar   <- 5                        

# WHG input parameters
WHG_n.flq     <- as.FLQuant(read.csv(file = "data/whg/stk3_n.csv"))
WHG_m.flq     <- as.FLQuant(read.csv(file = "data/whg/stk3_m.csv"))
WHG_spwn.flq  <- as.FLQuant(read.csv(file = "data/whg/stk3_spwn.csv"))
WHG_mat.flq   <- as.FLQuant(read.csv(file = "data/whg/stk3_mat.csv"))
WHG_fec.flq   <- WHG_mat.flq
WHG_fec.flq[] <- 1
WHG_wt.flq    <- as.FLQuant(read.csv(file = "data/whg/stk3_wt.csv"))

WHG_range.min       <- 0
WHG_range.max       <- 7
WHG_range.plusgroup <- 7
WHG_range.minyear   <- 2000
WHG_range.minfbar   <- 2
WHG_range.maxfbar   <- 5

# Projection: 
COD_biol.proj.avg.yrs  <- proj.avg.yrs
HAD_biol.proj.avg.yrs  <- proj.avg.yrs
WHG_biol.proj.avg.yrs  <- proj.avg.yrs

######  FLBEIA input object: biols ###### 
stks.data <- list(COD=ls(pattern="^COD"), HAD=ls(pattern="^HAD"), WHG=ls(pattern="^WHG")) 
biols    <- create.biols.data(yrs,ns,ni,stks.data)
# plotFLBiols(biols,pdfnm="outputs/biols_input")

################################################# 
##### 1.2 Biological component : biols.ctrl ##### 
################################################# 

growth.model     <- c("ASPG", "ASPG", "ASPG")
biols.ctrl       <- create.biols.ctrl (stksnames = stks, growth.model = growth.model)

##############################################
######## 1.3 Stock-recruitements: SRs ######## 
##############################################

########  cod
COD_sr.model        <- "bevholt"
COD_params.n        <- 2
COD_params.array    <- xtabs2(data ~ param + year + season + iter, data = read.csv(file = "data/cod/stk1_params.csv"), 
                              exclude=NULL, na.action = na.pass)  # create an array (of COD_params) from a a table
COD_params.name     <- c("a", "b") 

# another method to fill the array with srr parameter
# a <- 8257.106
# b <- 3155.035
# params <- array(c(a,b), dim=c(2,31,1,1), dimnames=list(param=c("a","b"), year=as.character(first.yr:last.yr), season=1,iter=1))
# COD_params.array    <- params         

COD_rec.flq         <- as.FLQuant(read.csv(file = "data/cod/stk1_rec.csv"))           
COD_ssb.flq         <- as.FLQuant(read.csv(file = "data/cod/stk1_ssb.csv"))             
COD_uncertainty.flq <- as.FLQuant(read.csv(file = "data/cod/stk1_uncertainty.csv"))     
COD_proportion.flq  <- as.FLQuant(read.csv(file = "data/cod/stk1_proportion.csv")) # Recruitment distribution in each time step. For details see FLSRsim    
COD_prop.avg.yrs    <- ac(2014:2016) # a vector with the years to calculate the average of COD_proportion.flq
COD_timelag.matrix  <- matrix(c(1,1),2,ns, dimnames = list(c("year", "season"), season = 1:ns)) # Timelag between the spawning an recruitment (time.lag.yr, time.lag.ns)

######## had
HAD_sr.model        <- "bevholt"
HAD_params.n        <- 2
HAD_params.array    <- xtabs2(data ~ param + year + season + iter, 
                               data=read.csv(file = "data/had/stk2_params.csv"),
                               exclude=NULL,
                               na.action=na.pass)  # create an array (of HAD_params) from a a table
HAD_params.name     <- c("a", "b") 
HAD_rec.flq         <- as.FLQuant(read.csv(file = "data/had/stk2_rec.csv"))
HAD_ssb.flq         <- as.FLQuant(read.csv(file = "data/had/stk2_ssb.csv"))            
HAD_uncertainty.flq <- as.FLQuant(read.csv(file = "data/had/stk2_uncertainty.csv"))     
HAD_proportion.flq  <- as.FLQuant(read.csv(file = "data/had/stk2_proportion.csv")) # Recruitment distribution in each time step. For details see FLSRsim    
HAD_prop.avg.yrs    <- ac(2014:2016) # a vector with the years to calculate the average of HAD_proportion.flq
HAD_timelag.matrix  <- matrix(c(0,1),2,ns, dimnames = list(c("year", "season"), season = 1:ns)) # Timelag between the spawning an recruitment (time.lag.yr, time.lag.ns)

######## whg
WHG_sr.model        <- "bevholt"
WHG_params.n        <- 2
WHG_params.array    <- xtabs2(data ~ param + year + season + iter, 
                               data=read.csv(file = "data/whg/stk3_params.csv"),
                               exclude=NULL,
                               na.action=na.pass)  # create an array (of WHG_params) from a a table
WHG_params.name     <- c("a", "b") 
WHG_rec.flq         <- as.FLQuant(read.csv(file = "data/whg/stk3_rec.csv"))             
WHG_ssb.flq         <- as.FLQuant(read.csv(file = "data/whg/stk3_ssb.csv"))             
WHG_uncertainty.flq <- as.FLQuant(read.csv(file = "data/whg/stk3_uncertainty.csv"))     
WHG_proportion.flq  <- as.FLQuant(read.csv(file = "data/whg/stk3_proportion.csv")) # Recruitment distribution in each time step. For details see FLSRsim    
WHG_prop.avg.yrs    <- ac(2014:2016) # a vector with the years to calculate the average of WHG_proportion.flq
WHG_timelag.matrix  <- matrix(c(0,1),2,ns, dimnames = list(c("year", "season"), season = 1:ns)) # Timelag between the spawning an recruitment (time.lag.yr, time.lag.ns)

## adding uncertainties : sd estimation from likelihood function of the fit of SRR
# error_COD <- rlnorm(31, 0, 0.82)
# error_HAD <- rlnorm(31, 0, 0.93)
# error_WHG <- rlnorm(31, 0, 0.39)

# adding uncertainties : estimation from white noise, arima model
error_COD <- arima.sim(n=31,model=list(order=c(0,0,0)),  innov = rlnorm(31, mean = -0.3510179, sd =  0.82))
error_HAD <- arima.sim(n=31,model=list(order=c(0,0,0)),  innov = rlnorm(31, mean = -0.3576565, sd = 0.98))
error_WHG <- arima.sim(n=31,model=list(order=c(0,0,0)),  innov = rlnorm(31, mean = -0.07378541, sd = 0.40))

COD_uncertainty.flq[,,,,] <- error_COD
HAD_uncertainty.flq[,,,,] <- error_HAD
WHG_uncertainty.flq[,,,,] <- error_WHG

####### FLBEIA input object: SRs ###### 
stks.data <- list(COD = ls(pattern = "^COD"), HAD = ls(pattern = "^HAD"), WHG = ls(pattern = "^WHG")) 
SRs       <- create.SRs.data(yrs, ns, ni, stks.data)

########################################
##### 1.4 Fleet component: Fleets  ##### 
########################################

# Fishing effort by fleet
IRE_effort.flq <- as.FLQuant(read.csv(file = "data/fleet component/fl1_effort.csv"))
UK_effort.flq <- as.FLQuant(read.csv(file = "data/fleet component/fl2_effort.csv"))
FRA_effort.flq <- as.FLQuant(read.csv(file = "data/fleet component/fl3_effort.csv"))
OTH_effort.flq <- as.FLQuant(read.csv(file = "data/fleet component/fl4_effort.csv"))

# Fishing capacity by fleet
IRE_capacity.flq <- IRE_effort.flq * 3
UK_capacity.flq <- UK_effort.flq * 3
FRA_capacity.flq <- FRA_effort.flq * 3
OTH_capacity.flq <- OTH_effort.flq * 3

# Fishing crewshare by fleet
IRE_crewshare.flq <- as.FLQuant(read.csv(file = "data/economic_data/IRL_crewshare.csv"))
UK_crewshare.flq <- as.FLQuant(read.csv(file = "data/economic_data/GBR_crewshare.csv"))
FRA_crewshare.flq <- as.FLQuant(read.csv(file = "data/economic_data/FRA_crewshare.csv"))

# Fishing fcost by fleet
IRE_fcost.flq <- as.FLQuant(read.csv(file = "data/economic_data/IRL_fcost.csv"))
UK_fcost.flq <- as.FLQuant(read.csv(file = "data/economic_data/GBR_fcost.csv"))
FRA_fcost.flq <- as.FLQuant(read.csv(file = "data/economic_data/FRA_fcost.csv"))
FRA_fcost.flq <- as.FLQuant(read.csv(file = "data/economic_data/FRA_fcost.csv"))

# Effort share between fleets
IRE.TR1_effshare.flq <- as.FLQuant(read.csv(file = "data/fleet component/fl1.met1_effshare.csv"))
UK.TR1_effshare.flq <- as.FLQuant(read.csv(file = "data/fleet component/fl2.met1_effshare.csv"))
FRA.TR1_effshare.flq <- as.FLQuant(read.csv(file = "data/fleet component/fl3.met1_effshare.csv"))
OTH.TR1_effshare.flq <- as.FLQuant(read.csv(file = "data/fleet component/fl4.met1_effshare.csv"))

IRE.TR2_effshare.flq <- as.FLQuant(read.csv(file = "data/fleet component/fl1.met2_effshare.csv"))
UK.TR2_effshare.flq <- as.FLQuant(read.csv(file = "data/fleet component/fl2.met2_effshare.csv"))
FRA.TR2_effshare.flq <- as.FLQuant(read.csv(file = "data/fleet component/fl3.met2_effshare.csv"))
OTH.TR2_effshare.flq <- as.FLQuant(read.csv(file = "data/fleet component/fl4.met2_effshare.csv"))

## Landings and discards weight
# COD : landings and discards by fleet (unit = n)
IRE.TR1.COD_landings.n.flq <- as.FLQuant(read.csv(file = "data/landings and discards/fl1.met1.stk1_landings.n.csv"))[2:8]
IRE.TR2.COD_landings.n.flq <- as.FLQuant(read.csv(file = "data/landings and discards/fl1.met2.stk1_landings.n.csv"))[2:8]
IRE.TR1.COD_discards.n.flq <- as.FLQuant(read.csv(file = "data/landings and discards/fl1.met1.stk1_discards.n.csv"))[2:8]
IRE.TR2.COD_discards.n.flq <- as.FLQuant(read.csv(file = "data/landings and discards/fl1.met2.stk1_discards.n.csv"))[2:8]

UK.TR1.COD_landings.n.flq <- as.FLQuant(read.csv(file = "data/landings and discards/fl2.met1.stk1_landings.n.csv"))[2:8]
UK.TR2.COD_landings.n.flq <- as.FLQuant(read.csv(file = "data/landings and discards/fl2.met2.stk1_landings.n.csv"))[2:8]
UK.TR1.COD_discards.n.flq <- as.FLQuant(read.csv(file = "data/landings and discards/fl2.met1.stk1_discards.n.csv"))[2:8]
UK.TR2.COD_discards.n.flq <- as.FLQuant(read.csv(file = "data/landings and discards/fl2.met2.stk1_discards.n.csv"))[2:8]

FRA.TR1.COD_landings.n.flq <- as.FLQuant(read.csv(file = "data/landings and discards/fl3.met1.stk1_landings.n.csv"))[2:8]
FRA.TR2.COD_landings.n.flq <- as.FLQuant(read.csv(file = "data/landings and discards/fl3.met2.stk1_landings.n.csv"))[2:8]
FRA.TR1.COD_discards.n.flq <- as.FLQuant(read.csv(file = "data/landings and discards/fl3.met1.stk1_discards.n.csv"))[2:8]
FRA.TR2.COD_discards.n.flq <- as.FLQuant(read.csv(file = "data/landings and discards/fl3.met2.stk1_discards.n.csv"))[2:8]

OTH.TR1.COD_landings.n.flq <- as.FLQuant(read.csv(file = "data/landings and discards/fl4.met1.stk1_landings.n.csv"))[2:8]
OTH.TR2.COD_landings.n.flq <- as.FLQuant(read.csv(file = "data/landings and discards/fl4.met2.stk1_landings.n.csv"))[2:8]
OTH.TR1.COD_discards.n.flq <- as.FLQuant(read.csv(file = "data/landings and discards/fl4.met1.stk1_discards.n.csv"))[2:8]
OTH.TR2.COD_discards.n.flq <- as.FLQuant(read.csv(file = "data/landings and discards/fl4.met2.stk1_discards.n.csv"))[2:8]

# HAD : landings and discards by fleet (unit = n)
IRE.TR1.HAD_landings.n.flq <- as.FLQuant(read.csv(file = "data/landings and discards/fl1.met1.stk2_landings.n.csv"))[1:9]
IRE.TR2.HAD_landings.n.flq <- as.FLQuant(read.csv(file = "data/landings and discards/fl1.met2.stk2_landings.n.csv"))[1:9]
IRE.TR1.HAD_discards.n.flq <- as.FLQuant(read.csv(file = "data/landings and discards/fl1.met1.stk2_discards.n.csv"))[1:9]
IRE.TR2.HAD_discards.n.flq <- as.FLQuant(read.csv(file = "data/landings and discards/fl1.met2.stk2_discards.n.csv"))[1:9]

UK.TR1.HAD_landings.n.flq <- as.FLQuant(read.csv(file = "data/landings and discards/fl2.met1.stk2_landings.n.csv"))[1:9]
UK.TR2.HAD_landings.n.flq <- as.FLQuant(read.csv(file = "data/landings and discards/fl2.met2.stk2_landings.n.csv"))[1:9]
UK.TR1.HAD_discards.n.flq <- as.FLQuant(read.csv(file = "data/landings and discards/fl2.met1.stk2_discards.n.csv"))[1:9]
UK.TR2.HAD_discards.n.flq <- as.FLQuant(read.csv(file = "data/landings and discards/fl2.met2.stk2_discards.n.csv"))[1:9]

FRA.TR1.HAD_landings.n.flq <- as.FLQuant(read.csv(file = "data/landings and discards/fl3.met1.stk2_landings.n.csv"))[1:9]
FRA.TR2.HAD_landings.n.flq <- as.FLQuant(read.csv(file = "data/landings and discards/fl3.met2.stk2_landings.n.csv"))[1:9]
FRA.TR1.HAD_discards.n.flq <- as.FLQuant(read.csv(file = "data/landings and discards/fl3.met1.stk2_discards.n.csv"))[1:9]
FRA.TR2.HAD_discards.n.flq <- as.FLQuant(read.csv(file = "data/landings and discards/fl3.met2.stk2_discards.n.csv"))[1:9]

OTH.TR1.HAD_landings.n.flq <- as.FLQuant(read.csv(file = "data/landings and discards/fl4.met1.stk2_landings.n.csv"))[1:9]
OTH.TR2.HAD_landings.n.flq <- as.FLQuant(read.csv(file = "data/landings and discards/fl4.met2.stk2_landings.n.csv"))[1:9]
OTH.TR1.HAD_discards.n.flq <- as.FLQuant(read.csv(file = "data/landings and discards/fl4.met1.stk2_discards.n.csv"))[1:9]
OTH.TR2.HAD_discards.n.flq <- as.FLQuant(read.csv(file = "data/landings and discards/fl4.met2.stk2_discards.n.csv"))[1:9]

# WHG : landings and discards by fleet (unit = n)
IRE.TR1.WHG_landings.n.flq <- as.FLQuant(read.csv(file = "data/landings and discards/fl1.met1.stk3_landings.n.csv"))[1:8]
IRE.TR2.WHG_landings.n.flq <- as.FLQuant(read.csv(file = "data/landings and discards/fl1.met2.stk3_landings.n.csv"))[1:8]
IRE.TR1.WHG_discards.n.flq <- as.FLQuant(read.csv(file = "data/landings and discards/fl1.met1.stk3_discards.n.csv"))[1:8]
IRE.TR2.WHG_discards.n.flq <- as.FLQuant(read.csv(file = "data/landings and discards/fl1.met2.stk3_discards.n.csv"))[1:8]

UK.TR1.WHG_landings.n.flq <- as.FLQuant(read.csv(file = "data/landings and discards/fl2.met1.stk3_landings.n.csv"))[1:8]
UK.TR2.WHG_landings.n.flq <- as.FLQuant(read.csv(file = "data/landings and discards/fl2.met2.stk3_landings.n.csv"))[1:8]
UK.TR1.WHG_discards.n.flq <- as.FLQuant(read.csv(file = "data/landings and discards/fl2.met1.stk3_discards.n.csv"))[1:8]
UK.TR2.WHG_discards.n.flq <- as.FLQuant(read.csv(file = "data/landings and discards/fl2.met2.stk3_discards.n.csv"))[1:8]

FRA.TR1.WHG_landings.n.flq <- as.FLQuant(read.csv(file = "data/landings and discards/fl3.met1.stk3_landings.n.csv"))[1:8]
FRA.TR2.WHG_landings.n.flq <- as.FLQuant(read.csv(file = "data/landings and discards/fl3.met2.stk3_landings.n.csv"))[1:8]
FRA.TR1.WHG_discards.n.flq <- as.FLQuant(read.csv(file = "data/landings and discards/fl3.met1.stk3_discards.n.csv"))[1:8]
FRA.TR2.WHG_discards.n.flq <- as.FLQuant(read.csv(file = "data/landings and discards/fl3.met2.stk3_discards.n.csv"))[1:8]

OTH.TR1.WHG_landings.n.flq <- as.FLQuant(read.csv(file = "data/landings and discards/fl4.met1.stk3_landings.n.csv"))[1:8]
OTH.TR2.WHG_landings.n.flq <- as.FLQuant(read.csv(file = "data/landings and discards/fl4.met2.stk3_landings.n.csv"))[1:8]
OTH.TR1.WHG_discards.n.flq <- as.FLQuant(read.csv(file = "data/landings and discards/fl4.met1.stk3_discards.n.csv"))[1:8]
OTH.TR2.WHG_discards.n.flq <- as.FLQuant(read.csv(file = "data/landings and discards/fl4.met2.stk3_discards.n.csv"))[1:8]

# Price by fleet, métier and stk 
IRE.TR1.COD_price.flq <- as.FLQuant(read.csv(file = "data/economic_data/COD_IRL_price.csv"))
IRE.TR2.COD_price.flq <- as.FLQuant(read.csv(file = "data/economic_data/COD_IRL_price.csv"))
FRA.TR1.COD_price.flq <- as.FLQuant(read.csv(file = "data/economic_data/COD_FRA_price.csv"))
FRA.TR2.COD_price.flq <- as.FLQuant(read.csv(file = "data/economic_data/COD_FRA_price.csv"))
UK.TR1.COD_price.flq <- as.FLQuant(read.csv(file = "data/economic_data/COD_GBR_price.csv"))
UK.TR2.COD_price.flq <- as.FLQuant(read.csv(file = "data/economic_data/COD_GBR_price.csv"))
OTH.TR1.COD_price.flq <- as.FLQuant(read.csv(file = "data/economic_data/COD_OTH_price.csv"))
OTH.TR2.COD_price.flq <- as.FLQuant(read.csv(file = "data/economic_data/COD_OTH_price.csv"))

IRE.TR1.HAD_price.flq <- as.FLQuant(read.csv(file = "data/economic_data/HAD_IRL_price.csv"))
IRE.TR2.HAD_price.flq <- as.FLQuant(read.csv(file = "data/economic_data/HAD_IRL_price.csv"))
FRA.TR1.HAD_price.flq <- as.FLQuant(read.csv(file = "data/economic_data/HAD_FRA_price.csv"))
FRA.TR2.HAD_price.flq <- as.FLQuant(read.csv(file = "data/economic_data/HAD_FRA_price.csv"))
UK.TR1.HAD_price.flq <- as.FLQuant(read.csv(file = "data/economic_data/HAD_GBR_price.csv"))
UK.TR2.HAD_price.flq <- as.FLQuant(read.csv(file = "data/economic_data/HAD_GBR_price.csv"))
OTH.TR1.HAD_price.flq <- as.FLQuant(read.csv(file = "data/economic_data/HAD_OTH_price.csv"))
OTH.TR2.HAD_price.flq <- as.FLQuant(read.csv(file = "data/economic_data/HAD_OTH_price.csv"))

IRE.TR1.WHG_price.flq <- as.FLQuant(read.csv(file = "data/economic_data/WHG_IRL_price.csv"))
IRE.TR2.WHG_price.flq <- as.FLQuant(read.csv(file = "data/economic_data/WHG_IRL_price.csv"))
FRA.TR1.WHG_price.flq <- as.FLQuant(read.csv(file = "data/economic_data/WHG_FRA_price.csv"))
FRA.TR2.WHG_price.flq <- as.FLQuant(read.csv(file = "data/economic_data/WHG_FRA_price.csv"))
UK.TR1.WHG_price.flq <- as.FLQuant(read.csv(file = "data/economic_data/WHG_GBR_price.csv"))
UK.TR2.WHG_price.flq <- as.FLQuant(read.csv(file = "data/economic_data/WHG_GBR_price.csv"))
OTH.TR1.WHG_price.flq <- as.FLQuant(read.csv(file = "data/economic_data/WHG_OTH_price.csv"))
OTH.TR2.WHG_price.flq <- as.FLQuant(read.csv(file = "data/economic_data/WHG_OTH_price.csv"))

## Projection
# Projection IRE
IRE_proj.avg.yrs         <- proj.avg.yrs
IRE.TR1_proj.avg.yrs     <- proj.avg.yrs
IRE.TR2_proj.avg.yrs     <- proj.avg.yrs
IRE.TR1.COD_proj.avg.yrs <- proj.avg.yrs
IRE.TR1.HAD_proj.avg.yrs <- proj.avg.yrs
IRE.TR1.WHG_proj.avg.yrs <- proj.avg.yrs
IRE.TR2.COD_proj.avg.yrs <- proj.avg.yrs
IRE.TR2.HAD_proj.avg.yrs <- proj.avg.yrs
IRE.TR2.WHG_proj.avg.yrs <- proj.avg.yrs

# Projection UK
UK_proj.avg.yrs         <- proj.avg.yrs
UK.TR1_proj.avg.yrs     <- proj.avg.yrs
UK.TR2_proj.avg.yrs     <- proj.avg.yrs
UK.TR1.COD_proj.avg.yrs <- proj.avg.yrs
UK.TR1.HAD_proj.avg.yrs <- proj.avg.yrs
UK.TR1.WHG_proj.avg.yrs <- proj.avg.yrs
UK.TR2.COD_proj.avg.yrs <- proj.avg.yrs
UK.TR2.HAD_proj.avg.yrs <- proj.avg.yrs
UK.TR2.WHG_proj.avg.yrs <- proj.avg.yrs

# Projection FRA
FRA_proj.avg.yrs         <- proj.avg.yrs
FRA.TR1_proj.avg.yrs     <- proj.avg.yrs
FRA.TR2_proj.avg.yrs     <- proj.avg.yrs
FRA.TR1.COD_proj.avg.yrs <- proj.avg.yrs
FRA.TR1.HAD_proj.avg.yrs <- proj.avg.yrs
FRA.TR1.WHG_proj.avg.yrs <- proj.avg.yrs
FRA.TR2.COD_proj.avg.yrs <- proj.avg.yrs
FRA.TR2.HAD_proj.avg.yrs <- proj.avg.yrs
FRA.TR2.WHG_proj.avg.yrs <- proj.avg.yrs

# Projection OTH
OTH_proj.avg.yrs         <- proj.avg.yrs
OTH.TR1_proj.avg.yrs     <- proj.avg.yrs
OTH.TR2_proj.avg.yrs     <- proj.avg.yrs
OTH.TR1.COD_proj.avg.yrs <- proj.avg.yrs
OTH.TR1.HAD_proj.avg.yrs <- proj.avg.yrs
OTH.TR1.WHG_proj.avg.yrs <- proj.avg.yrs
OTH.TR2.COD_proj.avg.yrs <- proj.avg.yrs
OTH.TR2.HAD_proj.avg.yrs <- proj.avg.yrs
OTH.TR2.WHG_proj.avg.yrs <- proj.avg.yrs

###### FLBEIA input object: fleets ###### 
fls.data  <- list(IRE = ls(pattern="^IRE"), UK = ls(pattern="^UK"), FRA = ls(pattern="^FRA"), OTH = ls(pattern="^OTH")) 
stks.data <- list(COD = ls(pattern = "^COD"), HAD = ls(pattern = "^HAD"), WHG = ls(pattern = "^WHG"))
fleets   <- create.fleets.data(yrs, ns, ni, fls.data, stks.data)
# plotFLFleets(fleets, pdfnm ="outputs/fleets_inputs")

#############################################
##### 1.6 Fleet component: fleets.ctrl  ##### 
############################################# 

# number of stocks per fleet
n.fls.stks      <- c(3, 3, 3, 3)  # number of stocks per fleet
fls.stksnames   <- rep(c("COD", "HAD", "WHG"), 4)
effort.models   <- c("SMFB", "SMFB","SMFB", "SMFB") # effort model per fleet

# restriction.fl : Alternative values are ’catch’ or ’landings’. Assigned value depends on 
# wether the efforts are calculated according to catch or landings restriction
restriction.IRE  <- "catch" 
restriction.UK   <- "catch"
restriction.FRA  <- "catch"
restriction.OTH  <- "catch"

# The fleet will stop fishing when the catch quota of any of the stocks is exhausted
effort.restr.IRE <- "mean" 
effort.restr.UK  <- "mean"  
effort.restr.FRA <- "mean" 
effort.restr.OTH <- "mean" 
 
catch.models     <- rep("CobbDouglasAge", 12)
capital.models   <- rep("fixedCapital", 4)
price.models     <- NULL                            

flq.COD <- FLQuant(dimnames = list(age = "all", year = first.yr:last.yr, unit = COD.unit, season = 1:ns, iter = 1:ni)) 

fleets.ctrl <- create.fleets.ctrl(fls = fls, n.fls.stks = n.fls.stks, fls.stksnames = fls.stksnames,
                                  effort.models = effort.models, catch.models = catch.models,
                                  capital.models = capital.models, price.models = price.models, flq = flq.COD, 
                                  effort.restr.IRE = effort.restr.IRE, restriction.IRE = restriction.IRE,
                                  effort.restr.UK = effort.restr.UK, restriction.UK = restriction.UK,
                                  effort.restr.FRA = effort.restr.FRA, restriction.FRA = restriction.FRA,
                                  effort.restr.OTH = effort.restr.OTH, restriction.OTH = restriction.OTH)

fleets.ctrl$IRE$restriction <- "catch" # (values : "catch" or "landings") Assigned value depends on wether the efforts are calculated according to catch or landings restriction
fleets.ctrl$UK$restriction  <- "catch"
fleets.ctrl$FRA$restriction <- "catch"
fleets.ctrl$OTH$restriction <- "catch"

# =========================================================================
#                       -2- Management procedure model  
# =========================================================================

#######################################
##### 2.1 Observation component   ##### 
#######################################
# population
stkObs.models  <- c("age2bioDat", "age2bioDat", "age2bioDat")
flq.COD        <- FLQuant(dimnames = list(age = 'all', year = first.yr:last.yr, unit = COD.unit, season = 1:ns, iter = 1:ni))
flq.HAD        <- FLQuant(dimnames = list(age = 'all', year = first.yr:last.yr, unit = HAD.unit, season = 1:ns, iter = 1:ni))
flq.WHG        <- FLQuant(dimnames = list(age = 'all', year = first.yr:last.yr, unit = WHG.unit, season = 1:ns, iter = 1:ni))
n.stks.inds    <- c(1, 1, 1)
stks.indsnames <- c("idBioCOD", "idBioHAD", "idBioWHG")
indObs.models	 <- c("bioInd", "bioInd", "bioInd")

obs.ctrl <- create.obs.ctrl(stksnames = stks,  stkObs.models = stkObs.models, n.stks.inds = n.stks.inds, 
                            stks.indsnames = stks.indsnames, indObs.models = indObs.models, 
                            flq.COD = flq.COD, flq.HAD = flq.HAD, flq.WHG = flq.WHG)

cv.error <- 0.2
error_land <- rlnorm(prod(dim(flq.COD)[-1]), 0, sqrt(log(1+cv.error^2)))
error_disc <- rlnorm(prod(dim(flq.COD)[-1]), 0, sqrt(log(1+cv.error^2)))

# COD
obs.ctrl$COD$stkObs$land.bio.error     <- FLQuant(error_land, dimnames=list(quant="COD", year = 2000:2030))
obs.ctrl$COD$stkObs$disc.bio.error     <- FLQuant(error_disc, dimnames=list(quant="COD", year = 2000:2030))
obs.ctrl$COD$stkObs$TAC.ovrsht         <- FLQuant(1, dimnames=list(quant="COD", year = 2000:2030))

# HAD
obs.ctrl$HAD$stkObs$land.bio.error     <- FLQuant(error_land, dimnames=list(quant="HAD", year = 2000:2030))
obs.ctrl$HAD$stkObs$disc.bio.error     <- FLQuant(error_disc, dimnames=list(quant="HAD", year = 2000:2030))
obs.ctrl$HAD$stkObs$TAC.ovrsht         <- FLQuant(1, dimnames=list(quant="HAD", year = 2000:2030))

cv.error <- 0.2
error_land <- rlnorm(prod(dim(flq.COD)[-1]), 0, sqrt(log(1+cv.error^2)))
error_disc <- rlnorm(prod(dim(flq.COD)[-1]), 0, sqrt(log(1+cv.error^2)))

# WHG
obs.ctrl$WHG$stkObs$land.bio.error     <- FLQuant(error_land, dimnames=list(quant="WHG", year = 2000:2030))
obs.ctrl$WHG$stkObs$disc.bio.error     <- FLQuant(error_disc, dimnames=list(quant="WHG", year = 2000:2030))
obs.ctrl$WHG$stkObs$TAC.ovrsht         <- FLQuant(1, dimnames=list(quant="WHG", year = 2000:2030))

####################################
#####   2.2 Index component    ##### 
####################################

# COD biomass index
flq   <- quantSums(biols[[1]]@n*biols[[1]]@wt)
unc   <- id <- q <- flq
unc[] <- rlnorm(prod(dim(flq)), 0, 0.2)
q[]   <- rep(runif(dim(flq)[1], 1e-05/2, 1e-05*5), dim(flq)[2])
id    <- flq*unc*q

COD_indices <- "idBioCOD"
COD_idBioCOD_index.flq  <- id
COD_idBioCOD_index.q.flq <- q
COD_idBioCOD_index.var.flq <- unc
COD_idBioCOD_range.min <- 0
COD_idBioCOD_range.max <- 0
COD_idBioCOD_range.minyear <- first.yr
COD_idBioCOD_range.maxyear <- proj.yr-1
COD_idBioCOD_type <- "FLIndexBiomass"

# HAD biomass index
flq   <- quantSums(biols[[2]]@n*biols[[2]]@wt)
unc   <- id <- q <- flq
unc[] <- rlnorm(prod(dim(flq)), 0, 0.2)
q[]   <- rep(runif(dim(flq)[1], 1e-05/2, 1e-05*5), dim(flq)[2])
id    <- flq*unc*q

HAD_indices <- "idBioHAD"
HAD_idBioHAD_index.flq  <- id
HAD_idBioHAD_index.q.flq <- q
HAD_idBioHAD_index.var.flq <- unc
HAD_idBioHAD_range.min <- 0
HAD_idBioHAD_range.max <- 0
HAD_idBioHAD_range.minyear <- first.yr
HAD_idBioHAD_range.maxyear <- proj.yr-1
HAD_idBioHAD_type <- "FLIndexBiomass"

# WHG biomass index
flq   <- quantSums(biols[[3]]@n*biols[[3]]@wt)
unc   <- id <- q <- flq
unc[] <- rlnorm(prod(dim(flq)), 0, 0.1)
q[]   <- rep(runif(dim(flq)[1], 1e-05/2, 1e-05*5), dim(flq)[2])
q[]   <- rep(1.7259e-05, dim(flq)[2]) #min
id    <- flq*unc*q

WHG_indices <- "idBioWHG"
WHG_idBioWHG_index.flq  <- id
WHG_idBioWHG_index.q.flq <- q
WHG_idBioWHG_index.var.flq <- unc
WHG_idBioWHG_range.min <- 0
WHG_idBioWHG_range.max <- 0
WHG_idBioWHG_range.minyear <- first.yr
WHG_idBioWHG_range.maxyear <- proj.yr-1
WHG_idBioWHG_type <- "FLIndexBiomass"

stks.data <- list(COD = ls(pattern="^COD"), HAD = ls(pattern="^HAD"), WHG = ls(pattern="^WHG")) 
indices_ctrl <- create.indices.data(yrs, ns, ni, stks.data)

indices_ctrl$WHG$idBioWHG@index[,c(1:7)] <- rnorm(7, mean(indices_ctrl$WHG$idBioWHG@index, na.rm=T), 0.2)
indices_ctrl$WHG$idBioWHG@index

####################################
##### 2.3 Assessment component ##### 
####################################

assess.models    <- rep("NoAssessment", 3)
assess.ctrl      <- create.assess.ctrl(stksnames = stks, assess.models = assess.models)

assess.ctrl[["COD"]]$work_w_Iter <- TRUE # The assessment model works with iters => all the iterations are 'adjusted' in one run.
assess.ctrl[["COD"]]$assess.model  <- "spict2flbeia" 
assess.ctrl[["COD"]]$harvest.units <- "f"

assess.ctrl[["HAD"]]$work_w_Iter <- TRUE
assess.ctrl[["HAD"]]$assess.model  <- "spict2flbeia" 
assess.ctrl[["HAD"]]$harvest.units <- "f"

assess.ctrl[["WHG"]]$work_w_Iter <- TRUE
assess.ctrl[["WHG"]]$assess.model  <- "spict2flbeia"
assess.ctrl[["WHG"]]$harvest.units <- "f"

####################################################
##### 2.4 Management advice component : advice ##### 
####################################################

COD_advice.TAC.flq   <- as.FLQuant(read.csv(file = "data/tac data/stk1.tac.csv"))
HAD_advice.TAC.flq   <- as.FLQuant(read.csv(file = 'data/tac data/stk2.tac.csv'))
WHG_advice.TAC.flq   <- as.FLQuant(read.csv(file = 'data/tac data/stk3.tac.csv'))

COD_advice.avg.yrs   <- c(2014:2016)
HAD_advice.avg.yrs   <- c(2014:2016)
WHG_advice.avg.yrs   <- c(2014:2016)

###### FLBEIA input object: advice ###### 
# List containing information on management advice; total allowable catch ‘TAC’ and quota share.)
stks.data <- list(COD = ls(pattern="^COD"), HAD = ls(pattern="^HAD"), WHG = ls(pattern="^WHG")) 
advice   <- create.advice.data(yrs, ns, ni, stks.data, fleets)

#########################################################
##### 2.5 Management advice component : advice.ctrl ##### 
#########################################################

HCR.models   <- c("IcesHCR", "IcesHCR", "IcesHCR") # HCR model by stock

ref.pts.COD <- matrix(c(0.35, 7300, 10300), 3, ni, dimnames = list(c("Fmsy", "Blim", "Btrigger"), 1:ni))
ref.pts.HAD <- matrix(c(0.40, 6700, 10000), 3, ni, dimnames = list(c("Fmsy", "Blim", "Btrigger"), 1:ni))
ref.pts.WHG <- matrix(c(0.52, 25000, 35000), 3, ni, dimnames = list(c("Fmsy", "Blim", "Btrigger"), 1:ni))

advice.ctrl  <- create.advice.ctrl(stksnames = stks, 
                                   HCR.models =  HCR.models, 
                                   ref.pts.COD = ref.pts.COD, 
                                   ref.pts.HAD = ref.pts.HAD, 
                                   ref.pts.WHG = ref.pts.WHG, 
                                   first.yr = first.yr,
                                   last.yr = last.yr)

advice.ctrl[["COD"]][["sr"]] <- list()
advice.ctrl[["HAD"]][["sr"]] <- list()
advice.ctrl[["WHG"]][["sr"]] <- list()

advice.ctrl[["COD"]][["sr"]][["model"]] <- "geomean"
advice.ctrl[["HAD"]][["sr"]][["model"]] <- "geomean"
advice.ctrl[["WHG"]][["sr"]][["model"]] <- "geomean"

# advice.ctrl[["COD"]][["sr"]][["years"]] <- c(y.rm = 2, num.years = 10) # remove the last two years and use the 10 years before (method used in ices working group). by default it takes all the time serie

advice.ctrl$COD$AdvCatch <- rep(TRUE, length(first.yr:last.yr))   #TRUE advice in catches, FALSE advice in landings
advice.ctrl$HAD$AdvCatch <- rep(TRUE, length(first.yr:last.yr))   
advice.ctrl$WHG$AdvCatch <- rep(TRUE, length(first.yr:last.yr))   

names(advice.ctrl$COD$AdvCatch) <- as.character((first.yr:last.yr))
names(advice.ctrl$HAD$AdvCatch) <- as.character((first.yr:last.yr))
names(advice.ctrl$WHG$AdvCatch) <- as.character((first.yr:last.yr))

###########################
###### 2.6 main.ctrl ###### 
###########################

main.ctrl           <- list()
main.ctrl$sim.years <- c(initial = proj.yr, final = last.yr)

# ==============================================================
#                      -3- Running FLBEIA
# ==============================================================

#Rename objects
multiAdv <- advice      
multiAdvC <- advice.ctrl 
multiAssC <- assess.ctrl 
multiBio <-  biols       
multiBioC <- biols.ctrl  
multiFl <- fleets      
multiFlC <- fleets.ctrl 
multiMainC <- main.ctrl  
multiObsC <-  obs.ctrl
multiSR <- SRs
multiInd <- indices_ctrl
# multiCv <- covars 
# multiCvC <- covars.ctrl

# save(multiBio, multiSR, fleets = multiFl, multiAdv, multiMainC, multiBioC, multiFlC, multiObsC, multiAssC, multiAdvC, file = "multi_Ghassen_exp_V2.RData")

####### Run FLBEIA ####### 
multiRes <- FLBEIA(biols = multiBio,       # FLBiols object with 2 FLBiol element for COD.
             SRs = multiSR,        # A list with 1 FLSRSim object for COD.
             fleets = multiFl,        # FLFleets object with on fleet.
             # covars = multiCv,         # covars not used
             indices = multiInd,         # indices not used (otherwise NULL)
             advice = multiAdv,       # A list with two elements 'TAC' and 'quota.share'
             main.ctrl = multiMainC,     # A list with one element to define the start and end of the simulation.
             biols.ctrl = multiBioC,      # A list with one element to select the model to simulate the stock dynamics.
             fleets.ctrl = multiFlC,       # A list with several elements to select fleet dynamic models and store additional parameters.
             covars.ctrl = NULL,         # multiCvC covars control not used 
             obs.ctrl = multiObsC,      # A list with one element to define how the stock observed ("PerfectObs").
             assess.ctrl = multiAssC,      # A list with one element to define how the stock assessment model used ("NoAssessment").
             advice.ctrl = multiAdvC)       # A list with one element to define how the TAC advice is obtained ("IcesHCR").

# save simulations outputs
save(multiRes, file = "FLBEIA_outputs.RData")

# plotFLBiols(multiRes$biols, pdfnm = "outputs/biols outputs")
# plotFLFleets(multiRes$fleets, pdfnm = "outputs/fleets outputs")
# plotfltStkSum(multiRes, pdfnm = "outputs/stk outputs")
# plotEco(multIREs,pdfnm = "outputs/eco outputs")

# ============= plots to check
# COD plot data prepartion
COD.mp0 <- multiRes$stocks[["COD"]]
COD.om0 <- FLBEIA:::perfectObs(multiRes$biols[["COD"]], multiRes$fleets, year = dim(multiRes$biols[["COD"]]@n)[2])

COD.opm.st <- as.data.frame(COD.om0@stock)
COD.opm.st$model <- "operating model"
COD.opm.st$type <- "stock"
COD.mpm.st <- as.data.frame(COD.mp0@stock)
COD.mpm.st$model <- "management procedure model"
COD.mpm.st$type <- "stock"

COD.opm.c <- as.data.frame(COD.om0@catch)
COD.opm.c$model <- "operating model"
COD.opm.c$type <- "catch"
COD.mpm.c <- as.data.frame(COD.mp0@catch)
COD.mpm.c$model <- "management procedure model"
COD.mpm.c$type <- "catch"

hr_COD <- apply(COD.om0@harvest, 2, mean)
COD.opm.hr <- as.data.frame(hr_COD)
COD.opm.hr$model <- "operating model"
COD.opm.hr$type <- "harvest"
COD.mpm.hr <- as.data.frame(COD.mp0@harvest)
COD.mpm.hr$model <- "management procedure model"
COD.mpm.hr$type <- "harvest"

om_mpm_COD <- rbind(COD.opm.st, COD.mpm.st, COD.opm.c, COD.mpm.c, COD.opm.hr, COD.mpm.hr)
om_mpm_COD$species <- "COD" 

# HAD plot data prepartion
HAD.mp0 <- multiRes$stocks[["HAD"]]
HAD.om0 <- FLBEIA:::perfectObs(multiRes$biols[["HAD"]], multiRes$fleets, year = dim(multiRes$biols[["HAD"]]@n)[2])

HAD.opm.st <- as.data.frame(HAD.om0@stock)
HAD.opm.st$model <- "operating model"
HAD.opm.st$type <- "stock"
HAD.mpm.st <- as.data.frame(HAD.mp0@stock)
HAD.mpm.st$model <- "management procedure model"
HAD.mpm.st$type <- "stock"

HAD.opm.c <- as.data.frame(HAD.om0@catch)
HAD.opm.c$model <- "operating model"
HAD.opm.c$type <- "catch"
HAD.mpm.c <- as.data.frame(HAD.mp0@catch)
HAD.mpm.c$model <- "management procedure model"
HAD.mpm.c$type <- "catch"

hr_HAD <- apply(HAD.om0@harvest, 2, mean)
HAD.opm.hr <- as.data.frame(hr_HAD)
HAD.opm.hr$model <- "operating model"
HAD.opm.hr$type <- "harvest"
HAD.mpm.hr <- as.data.frame(HAD.mp0@harvest)
HAD.mpm.hr$model <- "management procedure model"
HAD.mpm.hr$type <- "harvest"

om_mpm_HAD <- rbind(HAD.opm.st, HAD.mpm.st, HAD.opm.c, HAD.mpm.c, HAD.opm.hr, HAD.mpm.hr)
om_mpm_HAD$species <- "HAD" 

# WHG plot data prepartion
WHG.mp0 <- multiRes$stocks[["WHG"]]
WHG.om0 <- FLBEIA:::perfectObs(multiRes$biols[["WHG"]], multiRes$fleets, year = dim(multiRes$biols[["WHG"]]@n)[2])

WHG.opm.st <- as.data.frame(WHG.om0@stock)
WHG.opm.st$model <- "operating model"
WHG.opm.st$type <- "stock"
WHG.mpm.st <- as.data.frame(WHG.mp0@stock)
WHG.mpm.st$model <- "management procedure model"
WHG.mpm.st$type <- "stock"

WHG.opm.c <- as.data.frame(WHG.om0@catch)
WHG.opm.c$model <- "operating model"
WHG.opm.c$type <- "catch"
WHG.mpm.c <- as.data.frame(WHG.mp0@catch)
WHG.mpm.c$model <- "management procedure model"
WHG.mpm.c$type <- "catch"

hr_WHG <- apply(WHG.om0@harvest, 2, mean)
WHG.opm.hr <- as.data.frame(hr_WHG)
WHG.opm.hr$model <- "operating model"
WHG.opm.hr$type <- "harvest"
WHG.mpm.hr <- as.data.frame(WHG.mp0@harvest)
WHG.mpm.hr$model <- "management procedure model"
WHG.mpm.hr$type <- "harvest"

om_mpm_WHG <- rbind(WHG.opm.st, WHG.mpm.st, WHG.opm.c, WHG.mpm.c, WHG.opm.hr, WHG.mpm.hr)
om_mpm_WHG$species <- "WHG" 

om_mpm_COD_HAD_WHG <- rbind(om_mpm_COD, om_mpm_HAD, om_mpm_WHG)

p <- ggplot(om_mpm_COD_HAD_WHG, aes(x = year, y = data, color = model)) +
  geom_line() +
  scale_color_manual(values =c("skyblue3", "tomato"))+
  ylab("Tonnes") +
  theme_light() +
  geom_vline(xintercept = 2016, color = "grey20", linetype = "dotted") +
  facet_wrap(species~type, scales = "free", ncol = 3) +
  theme(legend.position = "bottom", legend.title = element_blank()) 
p

# ggsave(plot = p, filename = "om_mpm__f_plot2.pdf", path = "outputs/", width = 24, height = 18, units = "cm")
