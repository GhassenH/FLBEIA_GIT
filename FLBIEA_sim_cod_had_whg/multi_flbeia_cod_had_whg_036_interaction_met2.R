##################################################################################################################
# Script to run flbeia model wihth different fishing effort on 3 fleets : IRE, FR, UK
# April 2019
# Ghassen Halouani
# m_q1 : TR1, m_q2 : TR2
##################################################################################################################

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
library(tidyr)
library(dplyr)
library(plyr)

load("outputs_interaction_met2.RData") # FLBEIA simulation results
load("outputs_stk_df_met_interaction3.RData") # Results in dataframe format
# save(aa, bb, cc, file = "outputs_stk_df_met_interaction3.RData")

q_multiplier <- seq(0.2, 3, by = 0.2)
multiplier <- c("m=1", "m=2", "m=3", "m=4", "m=5", "m=6", "m=7", "m=8", "m=9", "m=10", 
                "m=11", "m=12", "m=13", "m=14", "m=15")


##############################################################################################
# source("function_run_flbeia_cod_had_whg_036_interaction_met.R")
source("function_flbeia_36_interact_met_PO.R")
  
  # ==================== simulations without errors ==================== 
  # simulating different quatchability q
  n <- 0
  outputs <- list()
  q_multiplier <- seq(0.2, 3, by = 0.2)
  for (i in 1:length(q_multiplier)) { 
    for (j in 1:length(q_multiplier)) { 
        a <- "error"
        while (a == "error") {
        tryCatch({
        a <- run_flbeia(q_m1 = q_multiplier[i], q_m2 = q_multiplier[j])
        n <- n+1
        outputs[[n]] <- a 
        names(outputs)[n] <- paste("m=", i, "_", "m=", j, sep="") }, error = function(e){})
       }
      print(paste("======================================================", " run i", i, "======================================================"))
      print(paste("======================================================", " run j", j, "======================================================"))
    }
  }
  # save(outputs, file = "outputs_interaction_met2.RData")
  # save(outputs, file = "outputs_interaction_min_AM.RData")
  # save(outputs, file = "outputs_interaction_max_AM.RData")
  # save(outputs, file = "outputs_interaction_previous_AM.RData")
  # save(outputs, file = "outputs_interaction_mean_AM.RData")
  save(outputs, file = "outputs_interaction_min_PO2new.RData")
  
# ============================================== parallel loop ==========================================================
# 
# q_multiplier <- seq(0.5, 1, by = 0.5)
# q_m1 <- rep(q_multiplier, each = length(q_multiplier))
# q_m2 <- rep(q_multiplier, times = length(q_multiplier))
# 
# 
# library(doParallel)
# n <- 0
# outputs <- list()
# effort <- names(outputs)
# registerDoParallel(cores=2)
# stime <- system.time({
#   
# par_sim <- foreach(i = 1:length(q_m1)) %dopar% {
#     a <- "error"
#     while (a == "error") {
#       tryCatch({
#         a <- run_flbeia(q_m1 = q_m1[i], q_m2 = q_m2[i])
#         outputs[[i]] <- a 
#         names(outputs)[n] <- paste("m=", i, "_", "m=", j, sep="") }, error = function(e){})
#      }
#     outputs[i]
#   }
# })
# stime
# 
# for (i in 1 : length(q_m1)) {
# names(par_sim)[i] <- paste("m=", q_m1[i], "_", "m=", q_m2[i], sep="")
# }
# 
# save(outputs, file = "outputs_interaction_met2.RData")
##############################################################################################

## Praparing dataframe to plot fleets results ### don't run
aa <- data.frame()
effort <- names(outputs)
aa <- data.frame()
for (i in 1:length(effort)) {
  s2_flt_Stk <- fltStkSum(outputs[[i]]) # flt outputs by stk
  s2_flt_Stk$sim <-effort[i]
  names(outputs[[i]])
  aa <-rbind(aa, s2_flt_Stk)
  print(i)
  print(effort[i])
}
save(aa, file = "outputs_stk_df_met_interaction2.RData")

############################################### stk plots
aa2 <- separate(data = aa, col = sim, into = c("eff_1", "eff_2"), sep = "_", remove = F)
join_table1 <- data.frame(eff_1 = multiplier, q_multiplier1 = q_multiplier)
join_table2 <- data.frame(eff_2 = multiplier, q_multiplier2 = q_multiplier)
aa2 <- left_join(aa2, join_table1, by = "eff_1")
aa2 <- left_join(aa2, join_table2, by = "eff_2")

stk_fleet <- subset(aa2, indicator == "catch") # quota
stk_fleet$year <- as.Date(stk_fleet$year, format = "%Y", origin = "2000")

# Fleets outputs by fleet for stock 1
plot_flt_stk1 <- ggplot(stk_fleet, aes(x = year, y = value, color = sim)) +
  geom_line(alpha=0.07) +
  # scale_colour_manual(values = mypalette, labels = F_multiplier, name="Catchability\nmultiplier")+
  geom_vline(aes(xintercept=as.Date("2016-06-01", origin = "2000-01-01")), lty = "dotted", color = "grey30") +
  ylab("") +
  theme(legend.position = "none") +
  # facet_wrap(stock ~ fleet, scales = "free_y", ncol = 4) +
  facet_grid(fleet ~ stock, scales = "free_y") +
  ggtitle("Catch by fleet and stock")
plot_flt_stk1

############################################### biols plots
### Praparing dataframe to plot fleets results ### no run
effort <- names(outputs)
bb <- data.frame()
for (i in 1:length(effort)) {
  s2_flt_Stk <- bioSum(outputs[[i]]) # flt outputs by stk
  s2_flt_Stk$sim <-effort[i]
  # names(outputs[[i]])
  bb <-rbind(bb, s2_flt_Stk)
  print(i)
  print(effort[i])
}

bb2 <- separate(data = bb, col = sim, into = c("eff_1", "eff_2"), sep = "_", remove = F)
join_table1 <- data.frame(eff_1 = multiplier, q_multiplier1 = q_multiplier)
join_table2 <- data.frame(eff_2 = multiplier, q_multiplier2 = q_multiplier)
bb2 <- left_join(bb2, join_table1, by = "eff_1")
bb2 <- left_join(bb2, join_table2, by = "eff_2")

biols_outputs <- subset(bb2, indicator %in% c("biomass", "landings" ))
biols_outputs$year <- as.Date(as.character(biols_outputs$year), format = "%Y", origin = "2000-01-01")

plot_bio <- ggplot(biols_outputs, aes (x = year, y = value, color = as.factor(sim))) +
  geom_line(alpha = 0.2) +
  # scale_colour_manual(values = mypalette, labels= q_multiplier, name="Catchability\nmultiplier") +
  geom_vline(aes(xintercept=as.Date("2016-06-01", origin = "2000-01-01")), lty = "dotted", color = "grey30") +
  ylab("") +
  theme(legend.position="none") +
  facet_wrap(stock ~ indicator, scales = "free_y", ncol = 2) +
  ggtitle("Biols outputs by stock")
plot_bio

# Highlight TR1
biols_outputs_met1 <- subset(biols_outputs, q_multiplier2 == 1)
mypalette <- colorRampPalette(c("cadetblue1", "deepskyblue4"))
eff_q_multiplier1 <- levels(factor(biols_outputs_met1$q_multiplier1))
mypalette <- mypalette(length(eff_q_multiplier1))
mypalette[which(eff_q_multiplier1==1)] <- "orange"

plot_flt_stk1 <- ggplot(biols_outputs_met1, aes(x = year, y = value, color = sim)) +
  geom_line() +
  scale_colour_manual(values = mypalette, labels = eff_q_multiplier1, name="q_multiplier \nTR1")+
  geom_vline(aes(xintercept=as.Date("2017-06-01", origin = "2000-01-01")), lty = "dotted", color = "grey30") +
  ylab("") +
  theme(legend.position="right") +
  # facet_wrap(stock ~ indicator, scales = "free_y", ncol = 2) +
  facet_grid(indicator ~ stock, scales = "free_y") +
  ggtitle("Varying the catchability of TR1")
plot_flt_stk1

# Highlight TR2
biols_outputs_met2 <- subset(biols_outputs, q_multiplier1 == 1)
mypalette <- colorRampPalette(c("cadetblue1", "deepskyblue4"))
eff_q_multiplier2 <- levels(factor(biols_outputs_met2$q_multiplier2))
mypalette <- mypalette(length(eff_q_multiplier2))
mypalette[which(eff_q_multiplier2==1)] <- "orange"

plot_flt_stk1 <- ggplot(biols_outputs_met2, aes(x = year, y = value, color = sim)) +
  geom_line() +
  scale_colour_manual(values = mypalette, labels = eff_q_multiplier2, name="q_multiplier \nTR2")+
  geom_vline(aes(xintercept=as.Date("2017-06-01", origin = "2000-01-01")), lty = "dotted", color = "grey30") +
  ylab("") +
  theme(legend.position="right") +
  # facet_wrap(stock ~ indicator, scales = "free_y", ncol = 2) +
  facet_grid(indicator ~ stock, scales = "free_y") +
  ggtitle("Varying the catchability of TR2")
plot_flt_stk1
  
############################################### advice plots
### Praparing dataframe to plot fleets results ###
cc <- data.frame()
for (i in 1:length(effort)) {
  s2_flt_Stk <- advSum(outputs[[i]]) # flt outputs by stk
  s2_flt_Stk$sim <-effort[i]
  names(outputs[[i]])
  cc <-rbind(cc, s2_flt_Stk)
  print(i)
  print(effort[i])
}

#-------------------------------- optional parallel loop begin for advice plot --------------------------------
# library(doParallel)
# effort <- names(outputs)
# registerDoParallel(cores=2)
# stime <- system.time({
# dd <- foreach(i = 1:length(effort))  %dopar% {
#   s2_flt_Stk <- advSum(outputs[[i]]) # flt outputs by stk
#   s2_flt_Stk$sim <-effort[i]
#   s2_flt_Stk
#  }
# })
# stime
# 
# dddf2 <- do.call(rbind.data.frame, dd) # good convert data from list to dataframe 
# dddf3 <- ldply (dd, data.frame) # good convert data from list to dataframe
# -------------------------------- optional parallel loop end --------------------------------

cc2 <- separate(data = cc, col = sim, into = c("eff_1", "eff_2"), sep = "_", remove = F)
join_table1 <- data.frame(eff_1 = multiplier, q_multiplier1 = q_multiplier)
join_table2 <- data.frame(eff_2 = multiplier, q_multiplier2 = q_multiplier)
cc2 <- left_join(cc2, join_table1, by = "eff_1")
cc2 <- left_join(cc2, join_table2, by = "eff_2")

adv_outputs <- subset(cc2, indicator %in% c( "discards", "discRat", "quotaUpt", "tac"))
adv_outputs$year <- as.Date(as.character(adv_outputs$year), format = "%Y", origin = "2000-01-01")

plot_bio <- ggplot(adv_outputs, aes (x = year, y = value, color = as.factor(sim))) +
  geom_line(alpha = 0.2) +
  geom_vline(aes(xintercept=as.Date("2017-06-01", origin = "2000-01-01")), lty = "dotted", color = "grey30") +
  ylab("") +
  theme(legend.position="none") +
  # facet_wrap(stock ~ indicator, scales = "free_y", ncol = 2) +
  facet_grid(indicator ~ stock, scales = "free_y") +
  ggtitle("Advice plots by stock _ All simulations")
plot_bio

# Highlight TR1
adv_outputs_met1 <- subset(adv_outputs, q_multiplier2 == 1)
mypalette <- colorRampPalette(c("cadetblue1", "deepskyblue4"))
eff_q_multiplier1 <- levels(factor(adv_outputs_met1$q_multiplier1))
mypalette <- mypalette(length(eff_q_multiplier1))
mypalette[which(eff_q_multiplier1==1)] <- "orange"

plot_flt_stk1 <- ggplot(adv_outputs_met1, aes(x = year, y = value, color = sim)) +
  geom_line() +
  scale_colour_manual(values = mypalette, labels = eff_q_multiplier1, name="q_multiplier \nTR1")+
  geom_vline(aes(xintercept=as.Date("2017-06-01", origin = "2000-01-01")), lty = "dotted", color = "grey30") +
  ylab("") +
  theme(legend.position="right") +
  # facet_wrap(stock ~ indicator, scales = "free_y", ncol = 2) +
  facet_grid(indicator ~ stock, scales = "free_y") +
  ggtitle("Varying the catchability of TR1")
plot_flt_stk1

# Highlight TR2
adv_outputs_met2 <- subset(adv_outputs, q_multiplier1 == 1)
mypalette <- colorRampPalette(c("cadetblue1", "deepskyblue4"))
eff_q_multiplier2 <- levels(factor(adv_outputs_met2$q_multiplier2))
mypalette <- mypalette(length(eff_q_multiplier2))
mypalette[which(eff_q_multiplier2==1)] <- "orange"

plot_flt_stk1 <- ggplot(adv_outputs_met2, aes(x = year, y = value, color = sim)) +
  geom_line() +
  scale_colour_manual(values = mypalette, labels = eff_q_multiplier2, name="q_multiplier \nTR2")+
  geom_vline(aes(xintercept=as.Date("2017-06-01", origin = "2000-01-01")), lty = "dotted", color = "grey30") +
  ylab("") +
  theme(legend.position="right") +
  facet_grid(indicator ~ stock, scales = "free_y") +
  ggtitle("Varying the catchability of TR2")
plot_flt_stk1

############################################### fleets   plots
### Praparing dataframe to plot fleets results ### no run
effort <- names(outputs)
cc <- data.frame()
for (i in 1:length(effort)) {
  s2_flt_Stk <- fltSum(outputs[[i]]) # flt outputs by stk
  s2_flt_Stk$sim <-effort[i]
  # names(outputs[[i]])
  cc <-rbind(cc, s2_flt_Stk)
  print(i)
  print(effort[i])
}

save(cc, file = "outputs_fleet_df_met_interaction.RData")

cc2 <- separate(data = cc, col = sim, into = c("eff_1", "eff_2"), sep = "_", remove = F)
join_table1 <- data.frame(eff_1 = multiplier, q_multiplier1 = q_multiplier)
join_table2 <- data.frame(eff_2 = multiplier, q_multiplier2 = q_multiplier)
cc2 <- left_join(cc2, join_table1, by = "eff_1")
  cc2 <- left_join(cc2, join_table2, by = "eff_2")

flt_outputs <- subset(cc2, indicator %in% c("effort", "landings" ))
flt_outputs$year <- as.Date(as.character(flt_outputs$year), format = "%Y", origin = "2000-01-01")

plot_flt <- ggplot(flt_outputs, aes (x = year, y = value, color = as.factor(sim))) +
  geom_line(alpha = 0.2) +
  # scale_colour_manual(values = mypalette, labels= q_multiplier, name="Catchability\nmultiplier") +
  geom_vline(aes(xintercept=as.Date("2016-06-01", origin = "2000-01-01")), lty = "dotted", color = "grey30") +
  ylab("") +
  theme(legend.position="none") +
  facet_wrap(fleet ~ indicator, scales = "free_y", ncol = 2) +
  ggtitle("Biols outputs by stock")
plot_flt
