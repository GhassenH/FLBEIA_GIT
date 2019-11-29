##################################################################################################################
# title : Summarizing MSE outputus 
# Author : Ghassen Halouani
# Date of creation : June 2019
# Script and data info: 
# 1 - the scripts extract and organize outputs of multi effort simulations (from RData files)
# 2 - plot the results
# 3 - calculate indicators of technical interactions
##################################################################################################################

library(FLBEIA)
library(ggplot2)
library(mgcv)
library(plotly)
library(tidyr)
library(dplyr)
library(mgcv)
library(mgcViz)
library(gridExtra)
library(ggforce)
library(RColorBrewer)
library(reshape)


## --- 3. load outputs (dataframe format) ---

load("output_analysis/outputs_max_AM_df.RData")
load("output_analysis/outputs_min_AM_df.RData")
load("output_analysis/outputs_mean_AM_df.RData")
load("output_analysis/outputs_previous_AM_df.RData")
load("output_analysis/outputs_min_PO_df.RData")
load("output_analysis/outputs_array_all_AM_data.RData")

## --- 4. organizing data ---

q_multiplier <- seq(0.2, 3, by = 0.2)
multiplier <- c("m=1", "m=2", "m=3", "m=4", "m=5", "m=6", "m=7", "m=8", "m=9", "m=10", "m=11", "m=12", "m=13", "m=14", "m=15")

tidy_data <- function(bb) {
  bb2 <- separate(data = bb, col = sim, into = c("eff_1", "eff_2"), sep = "_", remove = F)
  join_table1 <- data.frame(eff_1 = multiplier, q_multiplier1 = q_multiplier)
  join_table2 <- data.frame(eff_2 = multiplier, q_multiplier2 = q_multiplier)
  bb2 <- left_join(bb2, join_table1, by = "eff_1")
  bb2 <- left_join(bb2, join_table2, by = "eff_2")
  return(bb2)
}

outputs_min_AM_df2 <- tidy_data(outputs_min_AM_df$biols)
outputs_min_AM_df2$eff.rest <- "min"
outputs_min_AM_df2$assessment <- "assessment model"

outputs_max_AM_df2 <- tidy_data(outputs_max_AM_df$biols)
outputs_max_AM_df2$eff.rest <- "max"
outputs_max_AM_df2$assessment <- "assessment model"

outputs_mean_AM_df2 <- tidy_data(outputs_mean_AM_df$biols)
outputs_mean_AM_df2$eff.rest <- "mean"
outputs_mean_AM_df2$assessment <- "assessment model"

outputs_previous_AM_df2 <- tidy_data(outputs_previous_AM_df$biols)
outputs_previous_AM_df2$eff.rest <- "previous"
outputs_previous_AM_df2$assessment <- "assessment model"

outputs_min_PO_df2 <- tidy_data(outputs_min_PO_df$biols)
outputs_min_PO_df2$eff.rest <- "min"
outputs_min_PO_df2$assessment <- "perfect observation"

all_biols <- rbind(outputs_min_AM_df2, outputs_max_AM_df2, outputs_mean_AM_df2, outputs_previous_AM_df2, outputs_min_PO_df2)

## --- 5. plotting outputs & some simple analysis ---
# --- 5.1  averaging projection years ---

# selecting projecting years
indicators <- c("biomass", "catch", "f", "ssb", "rec", "discards", "landings")
eff_restriction <- "max"
assessment_component <- "assessment model"
# assessment_component <- "perfect observation"
all_biols2 <- subset(all_biols, indicator %in% indicators & eff.rest == eff_restriction & assessment == assessment_component)

ind <- "biomass"
stk <- "WHG"

# averaging data by multiplier across the years (year > 2017)
all_biols3 <- subset(all_biols2, year > 2017 & indicator == ind & stock == stk)
all_biols4 <- aggregate(value ~ sim + q_multiplier1 + q_multiplier2, data = all_biols3, FUN = mean)
ref_sc <- subset(all_biols4, sim == "m=5_m=5")$value
all_biols4$value2 <- all_biols4$value / ref_sc

# --- 5.2 2D heatmap plot ---

interaction_plot1 <- ggplot(all_biols4, aes(x = q_multiplier1, y = q_multiplier2)) +
  geom_raster(aes(fill = value2), interpolate = F) +
  # geom_abline(intercept = 0, slope = 1, color="white", linetype="dotted", size=1)+
  scale_fill_viridis_c(labels = c("min", "ref", "max"), breaks = c(min(all_biols4$value2), 1, max(all_biols4$value2))) +
  ggtitle(paste(stk, "/", ind, "/", eff_restriction, "/", assessment_component))+
  theme_minimal() +
  # annotate("point", x = 1, y = 1, colour = "white", size = 3) +
  annotate("text", x = 1, y = 1, colour = "white", size = 5, label = c("ref"), fontface = "bold") +  
  annotate("segment", x = 1, xend = 1, y = 0, yend = 0.9, colour = "white", size=0.5, alpha=0.75, linetype = "dashed") +
  annotate("segment", x = 0, xend = 0.9, y = 1, yend = 1, colour = "white", size=0.5, alpha=0.75, linetype = "dashed") +
  theme(legend.title=element_blank(),  legend.key.height = unit(3, 'lines'),  legend.spacing.x = unit(1, 'lines')) 
interaction_plot1

# --- 5.2 3D plot + fitting a gam model ---

q_multiplier1 <- seq(0.2, 3, by = 0.2)
q_multiplier2 <- seq(0.2, 3, by = 0.2)
data <- all_biols4$value2 # value : real data, value2 : relative data

dat <- data.frame(q_m_TR1=all_biols4$q_multiplier1, q_m_TR2=all_biols4$q_multiplier2, value=data) # data to fit
ref <- subset(dat, q_m_TR1 == 1 & q_m_TR2 == 1)$value # value of the reference scenario, no changes in fishing mortality

# fit a linear model
interaction_lm <- lm(data = dat, value ~ q_m_TR1 + q_m_TR2) 
a <- summary(interaction_lm)

interaction_plot11 <- interaction_plot1 +
  geom_abline(intercept = interaction_lm$coefficients[1], slope=interaction_lm$coefficients[2]+interaction_lm$coefficients[3], color="white") +
  geom_abline(intercept = 0, slope=interaction_lm$coefficients[2]+interaction_lm$coefficients[3], color="white") 
interaction_plot11

# fit a gam model
interaction_gam <- gam(data = dat, value ~ s(q_m_TR1, q_m_TR2, bs="tp" ))
plot(interaction_gam)

int.gam.viz <- getViz(interaction_gam)
plotgamViz <- plot(sm(int.gam.viz, 1)) + l_fitRaster() + l_fitContour() + l_points() 
plotgamViz + ggtitle("Smooth effects") # it is possible to use all ggplot2 functions

# predict values of the grid 
grid <- dat[, -3]
vals <- predict(interaction_lm, newdata = grid)

# form matrix and give to plotly
m2 <- matrix(vals, nrow = length(q_multiplier1), ncol = length(q_multiplier2), byrow = T)

plot3d <- plot_ly() %>%
  add_surface(x = q_multiplier1, y = q_multiplier2, z = m2, showscale = F) %>%
  add_trace(x=dat$q_m_TR1, y=dat$q_m_TR2, z=dat$value, type="scatter3d", mode = "markers", 
            color = data, size = 1, showlegend = F) %>%
  add_trace(x=1, y=1, z=ref, marker = list(color = "red", symbol = "circle"), size = 1, 
            type = "scatter3d", mode = "markers", showlegend = F) %>%
  layout(title = "Stock = Whiting \n Indicator = Biomass \n Effort restriction = Previous",
         scene = list(xaxis = list(title = 'Effort multplier métier 1'),
                      yaxis = list(title = 'Effort multplier métier 2'),
                      zaxis = list(title = 'Relative biomass'),
                      camera = list(eye = list(x = 0.75, y = 2, z = 1), zoom = 1)))
plot3d

## save plotly outputs 
# Sys.setenv("plotly_username" = "Ghassen_Halouani")
# Sys.setenv("plotly_api_key" = "jWbjXfy3diOEoEUfSNcu")
# plotly_IMAGE(plot3d, format = "png", out_file = "whg_b_previous_linear.png", width = 800, height = 650, scale =3)

# ========= plot to display the two slopes used to calculate the indicator ===========
names(dat)[3] <- "output" 
dat.melt <- melt(dat, id.vars = "output")

plot_slope <- ggplot(dat.melt, aes(x = value, y = output, color = variable)) +
  geom_point() +
  geom_smooth(method = "lm", fill = NA) +
  xlab("q multplier") +
  ylab("indicator") +
  # ylim(0, max(dat.melt$output)) +
  theme_classic() +
  theme(legend.title = element_blank()) +
  ggtitle(paste(stk, "/", ind, "/", eff_restriction, "/", assessment_component))
plot_slope
# ====================

# mgcViz plot
b <- getViz(interaction_gam)
plot(sm(b, 1)) + l_fitRaster() + l_fitContour() + l_points()
plotRGL(sm(b, 1), fix = c("z" = 0), residuals = TRUE)

## --- 6. analysis across dimensions  ---

# dimension of data.array : row, column, indicator, year, stk, effort restriction 
# 3- indicator : 1 = "biomass",  2 = "catch", 3 = "discards", 4 = "f", 5 = "landings", 6 = "rec", 7 = "ssb" 
# 4- year 2000 : 2030
# 5- stk : 1 = "COD", 2 = "HAD", 3 = "WHG"
# 6- eff : 1 = "max", 2 = "mean", 3 = "min", 4 = "previous"

# --- 6.2 reading and ploting directly from the array (projected years from 2018) + User friendly to selected ind, sp, eff.rest
# selection of parameters to plot
ind <- c("biomass", "catch", "discards", "f", "landings", "rec", "ssb")
year <- 2000 : 2030
stk <- c("COD", "HAD", "WHG")
eff <-c("max", "mean", "min", "previous")

eff.rest <- "mean"
species <- "HAD"
indicator <- "biomass"

d3 <- which(ind == indicator)
d5 <- which(stk == species)
d6 <- which(eff == eff.rest)

# template for data just to re-use q_multipliers 1 and 1 and year column
sel.temp <- data.array[,,d3, c(19:31), d5, d6]
test1.temp <- apply(sel.temp, c(1,2), mean)
data.select.template <- data.frame(test1.temp)

sel <- data.array[,,d3, c(19:31), d5, d6]
test1 <- apply(sel, c(1,2), mean)
data.select <- data.frame(test1)
names(data.select) <- c("year", "value", "q_multiplier1", "q_multiplier2")
data.select[, c(1,3,4)] <- data.select.template[, c(1,3,4)]

interaction_plot3 <- ggplot(data.select, aes(x = q_multiplier1, y = q_multiplier2)) +
  geom_raster(aes(fill = value), interpolate = F) +
  scale_fill_viridis_c(labels = c("min", "ref", "max"), breaks = c(min(data.select$value), data.select[65,2], max(data.select$value))) +
  ggtitle(paste(species, "/", indicator, "/", eff.rest))+
  theme_minimal() +
  annotate("text", x = 1, y = 1, colour = "white", size = 5, label = c("ref"), fontface = "bold") +  
  annotate("segment", x = 1, xend = 1, y = 0, yend = 0.9, colour = "white", size=0.5, alpha=0.75, linetype = "dashed") +
  annotate("segment", x = 0, xend = 0.9, y = 1, yend = 1, colour = "white", size=0.5, alpha=0.75, linetype = "dashed") +
  theme(legend.title=element_blank(),  legend.key.height = unit(3, 'lines'),  legend.spacing.x = unit(1, 'lines')) 
interaction_plot3

# --- 6.3 correlation plot : one example ---
sp1 <- which(stk == "COD")
sp2<- which(stk == "HAD")
indic <- which(ind == "f")
eff.r <- which(eff == "previous")
tab <- data.frame(matrix(nrow = 225, ncol = 1))

for (i in 1:225) {
  set1 <- data.array[i, 2, indic,, sp1, eff.r]
  set2 <- data.array[i, 2, indic,, sp2, eff.r]
  tab[i,1] <- cor(set1, set2, use="complete.obs", method = "spearman")
  print(i)
}

data.select[,2] <- tab
interaction_plot4 <- ggplot(data.select, aes(x = q_multiplier1, y = q_multiplier2)) +
  geom_raster(aes(fill = value), interpolate = F) +
  scale_fill_viridis_c(labels = c("min", "ref", "max"), breaks = c(min(data.select$value), data.select[65,2], max(data.select$value))) +
  ggtitle(paste(ind[indic], "/", eff[eff.r], "/", "correlation (", stk[sp1], ",", stk[sp2],")", sep=" "))+
  theme_minimal() +
  annotate("text", x = 1, y = 1, colour = "white", size = 5, label = c("ref"), fontface = "bold") +
  annotate("segment", x = 1, xend = 1, y = 0, yend = 0.9, colour = "white", size=0.5, alpha=0.75, linetype = "dashed") +
  annotate("segment", x = 0, xend = 0.9, y = 1, yend = 1, colour = "white", size=0.5, alpha=0.75, linetype = "dashed") +
  theme(legend.title=element_blank(),  legend.key.height = unit(3, 'lines'),  legend.spacing.x = unit(1, 'lines'))
interaction_plot4

# --- 6.31 ALL correlation plots
# preparing combinaison of plots
ind <- c("biomass", "catch", "discards", "f", "landings", "rec", "ssb")
eff <-c("max", "mean", "min", "previous")
cor.data <- expand.grid(ind, eff)
cor.data <- do.call("rbind", replicate(3, cor.data, simplify = FALSE))
sp.cor1 <- c(rep("COD", length(ind)*length(eff)), rep("COD", length(ind)*length(eff)), rep("HAD", length(ind)*length(eff)))
sp.cor2 <- c(rep("HAD", length(ind)*length(eff)), rep("WHG", length(ind)*length(eff)), rep("WHG", length(ind)*length(eff)))
cor.data$stk1 <- sp.cor1
cor.data$stk2 <- sp.cor2
names(cor.data) <- c("ind", "eff", "stk1", "stk2")

# calculating the correlations
tab2 <- data.frame(matrix(nrow = 225*84, ncol = 1))
n <- 0
for (i in 1:84) {
  for (j in 1:225) {
    set1 <- data.array[j, 2, which(ind == cor.data[i,1]),, which(stk == cor.data[i,3]), which(eff ==cor.data[i,2])]
    set2 <- data.array[j, 2, which(ind == cor.data[i,1]),, which(stk == cor.data[i,4]), which(eff ==cor.data[i,2])]
    n <- n+1
    tab2[n,1] <- cor(set1, set2, use="complete.obs", method = "spearman")
    print(n)
  }
}
names(tab2) <- "val"

# preparing data to plot
aa <- cor.data[rep(seq_len(nrow(cor.data)), each=225),] # metadata of each row (each correlation)

eff1 <- rep(data.select.template[,3], 84) #effort to plot for the heatmap
eff2 <- rep(data.select.template[,4], 84)

# constructing the final dataset to plot 
data.all.cor <- data.frame(tab2, eff1 = eff1, eff2 = eff2, aa)
names(data.all.cor) <-c("val", "eff1", "eff2", "ind", "eff", "stk1", "stk2")

# data.select.all <- subset(data.all.cor, stk1 == "COD" & stk2 == "WHG" & ind == "rec" & eff == "previous")
species1 <- "COD"
species2 <-"HAD"
data.select.all2 <- subset(data.all.cor, stk1 == species1 & stk2 == species2)

interaction_plot41 <- ggplot(data.select.all2, aes(x = eff1, y = eff2)) +
  geom_raster(aes(fill = val), interpolate = F) +
  # scale_fill_viridis_c(labels = c("min", "max"), breaks = c(min(data.select.all2$val), max(data.select.all2$val))) +
  scale_fill_viridis_c(labels = c(paste(round(min(data.select.all2$val),2)), 0, paste(round(max(data.select.all2$val),2) )), 
                       breaks = c(min(data.select.all2$val), 0, max(data.select.all2$val))) +
  theme_minimal() +
  facet_grid(ind ~ eff, scales = "free") +
  ggtitle(paste("Correlation ", species1, "-", species2)) +
  theme(legend.title=element_blank(),  legend.key.height = unit(3, 'lines'),  legend.spacing.x = unit(1, 'lines'))
interaction_plot41

ind <- c("biomass", "catch", "discards", "f", "landings", "rec", "ssb")
eff <-c("max", "mean", "min", "previous")
stk <- c("COD", "HAD", "WHG")
grid21 <- data.frame(expand.grid(1:7, 1:4))

cor_coef <- data.frame(matrix(nrow=nrow(grid21), ncol = 8))
for (i in 1:nrow(cor.data)) {
  data1 <- subset(data.all.cor, ind == cor.data[i,1] & eff == cor.data[i,2] & stk1 == cor.data[i,3] & stk2==cor.data[i,4])
  cor_lm <- lm(data = data1, val ~ eff1 + eff2) 
  cor_coef[i,1] <-  as.character(cor.data[i,1])
  cor_coef[i,2] <-  as.character(cor.data[i,2])
  cor_coef[i,3] <-  cor.data[i,3]
  cor_coef[i,4] <-  cor.data[i,4]
  cor_coef[i,5] <- cor_lm$coefficients[2]
  cor_coef[i,6] <- cor_lm$coefficients[3]
  print(i)
  a <- summary(cor_lm)
  if (a$coefficients[2,4] < 0.05) {cor_coef[i,7] <- 1
  } else cor_coef[i,7] <- 0
  
  if (a$coefficients[3,4] < 0.05) {cor_coef[i,8] <- 1
  } else cor_coef[i,8] <- 0
}

# adding information into the table
cor_coef <- cor_coef %>% mutate (signif = case_when(X7==1 & X8==1 ~ "*", X7==0 | X8==0 ~ "0" ))
cor_coef$sum_ind <- cor_coef$X5 + cor_coef$X6
cor_coef$cor <- paste(cor_coef$X3, cor_coef$X4, sep= " - ")

# Good indicator and good plot
cor.slope.plot1 <- ggplot(cor_coef, aes(x = X1, y = sum_ind, color = signif)) +
  geom_errorbar(aes(ymin = X5, ymax = X6), width = 0.1) +
  geom_pointrange(aes(ymin = X5, ymax = X6)) +
  scale_color_manual(values = c("tomato", "deepskyblue3"), label = c("*", "0")) +
  # facet_wrap(X3 ~X2, scales = "free_y", ncol = 3) +
  facet_grid(X2 ~ cor) +
  theme(legend.title=element_blank()) +
  geom_hline(yintercept = 0, color = "grey40", linetype = "dashed") +
  ylab("Strategy") +
  xlab("Stock") +
  ggtitle("Indicator corresponds to the sum of slope1 and slope2 (correlation plots)")
cor.slope.plot1

# ggsave(cor.slope.plot1, filename = "indicator_outpouts_sum_slope_correlation_plots.pdf", width = 30, height = 20, units = "cm")

## --- 7. generation of the Atlats of outputs ---
## plotting all results
# facet grid plot
grid3 <- data.frame(expand.grid(1:7, 1:3, 1:4))
names(grid3) <- c("indicator", "stk",  "eff.rest")

sel3 <- data.frame()
for (i in 1: nrow(grid3)) {
  sel <- data.array[,,grid3[i,1], c(19:31), grid3[i,2], grid3[i,3]]
  sel2 <- data.frame(apply(sel, c(1,2), mean))
  # sel2[,2] <- scale(sel2[,2])
  sel2$ind <- ind[grid3[i,1]]
  sel2$stk <- stk[grid3[i,2]]
  sel2$eff.rest <- eff[grid3[i,3]]
  sel3 <- rbind(sel2, sel3)
  print(i)                                                                                           
}

names(sel3) <- c("year", "value", "eff1", "eff2", "ind", "stk", "eff.rest")
sel3$eff1 <- eff1
sel3$eff2 <- eff2
sel4 <- subset(sel3, stk == "COD")

# ggplot()+  geom_curve(aes(x = 0.2, y = 0.2, xend = 0.2, yend = 3), curvature = 1, color = "lightblue", ncp = 100, size = 2) 

# -- Plot using Extra.grid package to use different scales --
grid4 <- data.frame(expand.grid(ind, eff))
names(grid4) <- c("indicator",  "eff.rest")

plot_list <- list()
for (i in 1 : nrow (grid4)) {
  sel4 <- subset(sel3, stk == "COD" & ind == grid4[i,1] & eff.rest == grid4[i,2])
  interaction_plot5 <- ggplot(sel4, aes(x = eff1, y = eff2)) +
    geom_raster(aes(fill = value), interpolate = F) +
    scale_fill_viridis_c(labels = c("min", "ref", "max"), breaks = c(min(sel4$value), data.select[65,2], max(sel4$value))) +
    ggtitle(paste("COD", "/", grid4[i,1], "/", grid4[i,2]))+
    theme_minimal() +
    theme(legend.title=element_blank(),  legend.position="none")
  # print(interaction_plot5) 
  plot_list[[i]] <- interaction_plot5
  print(i)
}

cod.plots <- do.call(grid.arrange, c(plot_list, list(ncol = 7)))

pdf("whg_outputs_plot.pdf", width = 16, height = 9 ) 
do.call(grid.arrange, c(plot_list, list(ncol = 7)))
dev.off() 

## --- 8. technical interactions indicators ---
# --- 8.1 technical interactions indicators :  slope1/slope2 indicator ---

ind <- c("biomass", "catch", "discards", "f", "landings", "rec", "ssb")
eff <-c("max", "mean", "min", "previous")
stk <- c("COD", "HAD", "WHG")

coef_tab <- data.frame(matrix(nrow=nrow(grid3), ncol = 6))
for (i in 1:nrow(grid3)) {
  sel <- data.array[,,grid3[i,1], c(19:31), grid3[i,2], grid3[i,3]]
  sel2 <- data.frame(apply(sel, c(1,2), mean))
  sel2[,3] <- sel[,3,1]
  sel2[,4] <- sel[,4,1]
  names(sel2) <- c("year", "value", "eff1", "eff2")
  sel2$value <- scale(sel2$value)
  interaction_lm2 <- lm(data = sel2, value ~ eff1 + eff2) 
  coef_tab[i,1] <- ind[grid3[i,1]]
  coef_tab[i,2] <- stk[grid3[i,2]]
  coef_tab[i,3] <- eff[grid3[i,3]]
  coef_tab[i,4] <- interaction_lm2$coefficients[2]
  coef_tab[i,5] <- interaction_lm2$coefficients[3]
  coef_tab[i,6] <- interaction_lm2$coefficients[2] / interaction_lm2$coefficients[3]
  
  a <- summary(interaction_lm2)
  if (a$coefficients[2,4] < 0.05) {coef_tab[i,7] <- 1
  } else coef_tab[i,7] <- 0
  
  if (a$coefficients[3,4] < 0.05) {coef_tab[i,8] <- 1
  } else coef_tab[i,8] <- 0
  
  print(i)
}

# adding information into the table
coef_tab <- coef_tab %>% mutate (signe_slope1 = case_when(X4 < 0 ~ "Negative", X4 > 0 ~ "Positive"), 
                                 high_slope = case_when(X4^2 > X5^2 ~ "higher", X4^2 < X5^2 ~ "lower"),
                                 signif = case_when(V7==1 & V8==1 ~ "*", V7==0 | V8==0 ~ "0" ))

coef_tab$ind2 <- (coef_tab$X4 + coef_tab$X5)   
coef_tab$ind3 <- log(abs(coef_tab$X6))   


# slope1 / slope2 all informations
ind.slope.plot <- ggplot(coef_tab, aes(x = X1, y = X6, label = signif, size = sqrt(abs(X6)))) +
  geom_point(aes(color = signif, shape= high_slope))+
  geom_text(hjust = 2, size = 4) +
  scale_color_manual(values = c("tomato", "deepskyblue3"), label = c("opposite", "same")) +
  scale_shape_manual(values = c(16, 17), label = c("slope1 > slope2", "slope1 < slope2")) +
  geom_hline(yintercept = c(-1, 0, 1), linetype = "dashed", color = "grey40")+
  guides(size = FALSE, 
         shape = guide_legend(title = NULL),
         colour = guide_legend(title = "signe") ) +
  facet_wrap(X3 ~X2, scales = "free_y", ncol = 3) +
  xlab("Strategy") +
  ylab("Stock") +
  ggtitle(expression(paste("indicator = ", slope1/slope2)))
ind.slope.plot

# same plot simplified
ind.slope.plot <- ggplot(coef_tab, aes(x = X1, y = X6, size = sqrt(X6^2))) +
  geom_point(aes(color = signif, shape= high_slope))+
  scale_color_manual(values = c("tomato", "deepskyblue3"), label = c("*", "0")) +
  scale_shape_manual(values = c(16, 17), label = c("slope1 > slope2", "slope1 < slope2")) +
  geom_hline(yintercept = c(-1, 0, 1), linetype = "dashed", color = "grey40")+
  guides(size = FALSE, 
         shape = guide_legend(title = NULL),
         colour = guide_legend(title = "signif") ) +
  facet_wrap(X3 ~X2, scales = "free_y", ncol = 3) +
  xlab("Strategy") +
  ylab("Stock") +
  ggtitle(expression(paste("indicator = ", slope1/slope2)))
ind.slope.plot

# ggsave(ind.slope.plot, filename = "indicator_outpouts_slope1_slope2.pdf", width = 30, height = 20, units = "cm")

# plot slope1 = axis x, slope 2 = axis y
ind.slope.plot2 <- ggplot(coef_tab, aes(x = X4, y = X5, color = X3, shape = X2)) +
  geom_point(size = 3) +
  # geom_text(vjust = 1.5, size = 3) +
  guides(shape = guide_legend(title = "Stock")) +
  geom_hline(yintercept = 0, color = "grey40") +
  geom_vline(xintercept = 0, color = "grey40") +
  # facet_grid(~X1) +
  facet_wrap(~X1, ncol = 3) +
  xlab("slope1") +
  ylab("slope2")
ind.slope.plot2

# ggsave(ind.slope.plot2, filename = "indicator_outpouts_slopes_axis2.pdf", width = 35, height = 40, units = "cm")

# more simple plot 
ind.slope.plot3 <- ggplot(coef_tab, aes(x = X1, y = ind2, color = signif)) +
  geom_errorbar(aes(ymin = X4, ymax = X5), width = 0.1) +
  geom_pointrange(aes(ymin = X4, ymax = X5)) +
  scale_color_manual(values = c("tomato", "deepskyblue3"), label = c("*", "0")) +
  # facet_wrap(X3 ~X2, scales = "free_y", ncol = 3) +
  facet_grid(X3 ~X2, scales = "free_y") +
  theme(legend.title=element_blank()) +
  geom_hline(yintercept = 0, color = "grey40", linetype = "dashed") +
  ylab("Strategy") +
  xlab("Stock") +
  ggtitle("Indicator corresponds to the sum of slope1 and slope2")
ind.slope.plot3

# ggsave(ind.slope.plot3, filename = "indicator_outpouts_sum_slope2.pdf", width = 30, height = 20, units = "cm")

# Indicator corresponds to the effect of the slopes 1/2 
coef_tab$ind4 <- (abs(coef_tab$X4)/ (abs(coef_tab$X4)+abs(coef_tab$X5))) * 100

ind.slope.plot31 <- ggplot(coef_tab, aes(x = X1, y = ind4, shape = signif, size = ind4)) +
  # geom_point(colour = "grey40", size = 5, alpha = 0.5)+
  geom_point(aes(colour = ind4), size = 4)+
  scale_color_viridis_c(name = "Interaction \n driven by", breaks = c(5, 95), labels = c("TR2", "TR1"))+
  facet_grid(X3 ~X2) +
  # theme(legend.title=element_blank()) +
  geom_hline(yintercept = 50, color = "grey40", linetype = "dashed") +
  ylab("Strategy") +
  xlab("Stock") +
  ggtitle("Indicator corresponds to slope1/(slope2+slope1)*100")
ind.slope.plot31

# ggsave(ind.slope.plot31, filename = "indicator_outpouts_percentage_effect.pdf", width = 30, height = 25, units = "cm")

# circular plot1 slope1/slpoe2
newplot2 <- ggplot(coef_tab) + 
  geom_hline(yintercept = 0, color = "grey40") +
  geom_abline(aes(intercept = 0, slope = X6, color = X3, linetype = signif)) +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  geom_arc(aes(x0 = 0, y0 = 0, r = 1, start = 0*pi, end = 2*pi), color = "grey60") +
  geom_abline(intercept = 0, slope = 1, color = "grey60", linetype = "dashed") +
  geom_abline(intercept = 0, slope = -1, color = "grey60", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "grey40") +
  facet_grid(X2 ~ X1) +
  guides(colour = guide_legend(title = "Effort restriction") ) +
  ggtitle("Indicator : slope eff1 / slope eff2") +
  theme_minimal()+
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())
newplot2

# ggsave(newplot2, filename = "indicator_circle_slope1_over_slope2.pdf", width = 23, height = 18, units = "cm")

## Biomass/Catch indicator
b.data <- subset(coef_tab, X1 == "biomass")
c.data <- subset(coef_tab, X1 == "catch")

b.c.data <- b.data[, c(4,5)] / c.data[, c(4,5)]
b.c.data <- data.frame(b.c.data, X2 = b.data$X2, X3 = b.data$X3 )
b.c.data$X6 <- b.c.data$X4 + b.c.data$X5

ind.slope.plot311 <- ggplot(b.c.data, aes(x = X3, y = X6)) +
  geom_errorbar(aes(ymin = X4, ymax = X5), width = 0.1, color = "deepskyblue3") +
  geom_pointrange(aes(ymin = X4, ymax = X5), color = "deepskyblue3") +
  # scale_color_manual(values = c("tomato", "deepskyblue3"), label = c("*", "0")) +
  # geom_point(size = 3) +
  # geom_point(aes(color = cut(X6, c(-90000, 0, 90000))), size = 2)+
  # scale_color_manual(values = c("tomato", "deepskyblue3"), label = c("+", "-")) +
  facet_grid(.~X2) +
  theme(legend.title=element_text("Signif.")) +
  geom_hline(yintercept = 0, color = "grey40", linetype = "dashed") +
  ylab("Indicator slope") +
  xlab("Stock") +
  ggtitle("Indicator B/C corresponds to log (slope1 + slope2)")
ind.slope.plot311

# --- 8.2 technical interactions indicators :  degree of freedom indicator ---

ind <- c("biomass", "catch", "discards", "f", "landings", "rec", "ssb")
eff <-c("max", "mean", "min", "previous")
stk <- c("COD", "HAD", "WHG")

coef_tab2 <- data.frame(matrix(nrow=nrow(grid3), ncol = 5))
for (i in 1:nrow(grid3)) {
  sel3 <- data.array[,,grid3[i,1], c(19:31), grid3[i,2], grid3[i,3]]
  sel4 <- data.frame(apply(sel3, c(1,2), mean))
  names(sel4) <- c("year", "value", "eff1", "eff2")
  # interaction_lm2 <- lm(data = sel2, value ~ eff1 + eff2) 
  interaction_gam <- gam(data = sel4, value ~ s(eff1, eff2, bs="tp" ))
  
  coef_tab2[i,1] <- ind[grid3[i,1]]
  coef_tab2[i,2] <- stk[grid3[i,2]]
  coef_tab2[i,3] <- eff[grid3[i,3]]
  coef_tab2[i,4] <- summary(interaction_gam)$edf
  coef_tab2[i,5] <- summary(interaction_gam)$s.table[1,4]
  
  a <- summary(interaction_gam)$s.table[1,4]
  if (a < 0.05) {coef_tab2[i,6] <- 1
  } else coef_tab2[i,6] <- 0
  
  print(i)
}

ind.slope.plot4 <- ggplot(coef_tab2, aes(x = X1, y = X4, colour = factor(V6), size = X4)) +
  geom_point() +
  scale_color_manual(values = c("tomato", "deepskyblue3"), label = c("0", "*")) +
  guides(size = guide_legend(title = "Indicator"),
         colour = guide_legend(title = "Signif")) +
  facet_grid(X3 ~X2, scales = "free_y") +
  ylab("Strategy") +
  xlab("Stock") +
  ggtitle("Indicator corresponds to the degree of freedom of smooth terms in the GAM model: indicator ~ s(eff1, eff2)")
ind.slope.plot4

# ggsave(ind.slope.plot4, filename = "indicator_outpouts_degree_of_freedom.pdf", width = 30, height = 20, units = "cm")


# ============= Speedometer plot ============= 

pal <- brewer.pal(n = 4, name = "Dark2")

coef_tab3 <- coef_tab
coef_tab3$angle <- coef_tab3$ind4*(180/100)
coef_tab3$rad <- coef_tab3$angle*(pi/180)

# circular plot1 slope1/slpoe2
newplot3 <- ggplot(coef_tab3, aes(color = X3)) + 
  scale_x_continuous(limits = c(-1.2, 1.2), breaks = c(-1, 0, 1), labels = c("low", "high \n \n intensity of the interaction", "low")) +
  # scale_y_continuous( expand = c(0, 0.025)) +
  geom_segment(aes(x = 0, xend = cos(rad), y = 0, yend = sin(rad)), size = 0.9) +
  # scale_color_manual(name="Effort restriction strategies", values = pal) +
  scale_color_brewer(name="Effort restriction strategies", palette = "Set2") +
  geom_arc(aes(x0 = 0, y0 = 0, r = 1, start = -pi*0.5, end = 0.5*pi), color = "grey50", 
           linetype = "solid", size = 0.25,  arrow = arrow(ends = "both", length = unit(0.05, "npc"))) +
  facet_grid(X1 ~ X2) +
  geom_segment(aes(x = -1, xend = 1, y = 0, yend = 0),  color = "grey50", size = 0.25) +
  # geom_segment(aes(x = 0, xend = -1, y = 0, yend = 0), color = "grey60", size = 0.25) +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 1),  linetype = "dashed", color = "grey50", size = 0.25) +
  annotate("text", label = "TR 2", x = 1.05, y = 0.2, angle = 280, size = 3, color = "grey30", fontface = "bold")+
  annotate("text", label = "TR 1", x = -1.05, y = 0.2, angle = 80, size = 3, color = "grey30", fontface = "bold")+
  xlab("") +
  ylab("") +
  theme_minimal() +
  guides(colour = guide_legend(title.position = "top")) +
  theme( axis.text.y = element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
         legend.position = "bottom", legend.title.align	= 0.5)
newplot3

ggsave(newplot3, filename = "indicator_interactions_TR1-TR2.pdf", width = 26, height = 30, units = "cm")

