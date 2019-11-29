##################################################################################################################
# Using gganiamte to plot FLBEIA outputs
# October 2019 
##################################################################################################################

library(ggplot2)
library(tidyr)
library(dplyr)
library(gganimate)


## --- 1. load outputs (dataframe format) ---

load("output_analysis/outputs_max_AM_df.RData")
load("output_analysis/outputs_min_AM_df.RData")
load("output_analysis/outputs_mean_AM_df.RData")
load("output_analysis/outputs_previous_AM_df.RData")
load("output_analysis/outputs_min_PO_df.RData")
load("output_analysis/outputs_array_all_AM_data.RData")

## --- 2. organizing data ---

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

## --- 3. plotting outputs  ---
## selecting projecting years
indicators <- c("biomass", "catch", "f", "ssb", "rec", "discards", "landings")
eff_restriction <- "max"
assessment_component <- "assessment model"
# assessment_component <- "perfect observation"
all_biols2 <- subset(all_biols, indicator %in% indicators & eff.rest == eff_restriction & assessment == assessment_component)

b_data <- subset(all_biols2, stock %in% c("COD", "HAD") & indicator == "biomass" & eff.rest == eff_restriction & assessment == assessment_component)
b_data <-b_data[,c(2,3,6,7)]
b_data2 <- spread(b_data, key = "stock", value = "value")

## example 
p <- ggplot(b_data2, aes(x = COD, y = HAD)) +
  geom_point(alpha = 0.3, color = "black")
p

anim <- p + transition_states(year, transition_length = 2, state_length = 2) +
  ggtitle('Year {closest_state}', subtitle = 'Frame {frame} of {nframes}')
anim

## plotting all data 

all_biols_spread <- spread(all_biols, key = "stock", value = "value")
all_biols_spread2 <- subset(all_biols_spread, year > 2016 & indicator %in% c("biomass", "catch", "discards", "rec"))

# cod-had
p <- ggplot(all_biols_spread2, aes(x = COD, y = HAD, color = year)) +
  facet_wrap(eff.rest ~ indicator, scales = "free") +
  scale_colour_viridis_c() +
  geom_point(alpha = 0.2) +
  ggtitle("COD - HAD") 
p

# cod-whg
p <- ggplot(all_biols_spread2, aes(x = COD, y = WHG, color = year)) +
  facet_wrap(eff.rest ~ indicator, scales = "free") +
  scale_colour_viridis_c() +
  geom_point(alpha = 0.2)+
  ggtitle("COD - WHG") 
p

# whg-had
p <- ggplot(all_biols_spread2, aes(x = HAD, y = WHG, color = year)) +
  facet_wrap(eff.rest ~ indicator, scales = "free") +
  scale_colour_viridis_c() +
  geom_point(alpha = 0.2) +
  ggtitle("HAD - WHG") 
p

## --- 4. animated plots ---
## 4.1 By years and q_multiplier1
p <- ggplot(all_biols_spread2, aes(x = COD, y = HAD, col = q_multiplier1)) +
  facet_wrap(.~ indicator, scales = "free") +
  scale_colour_viridis_c()+
  geom_point()
p

anim_year_qmTR1 <- p + transition_states(year, transition_length = 2, state_length = 2) +
  ggtitle('Year {closest_state}')
anim_year_qmTR1

anim_save(anim_year_qmTR1, filename = "outputs_by_years.gif", width = 20, height = 18, unit = "cm", res = 200)

## 4.2 By years and q_multiplier2
p <- ggplot(all_biols_spread2, aes(x = COD, y = HAD, col = q_multiplier2)) +
  facet_wrap(.~ indicator, scales = "free") +
  scale_colour_viridis_c()+
  geom_point()
p

anim_year_qmTR2 <- p + transition_states(year, transition_length = 2, state_length = 2) +
  ggtitle('Year {closest_state}')
anim_year_qmTR2

anim_save(anim_year_qmTR2, filename = "outputs_by_years_qmTR2.gif", width = 20, height = 18, unit = "cm", res = 200)

## 4.3 By q_multiplier1
p <- ggplot(all_biols_spread2, aes(x = COD, y = HAD, col = q_multiplier2)) +
  facet_wrap(.~ indicator, scales = "free") +
  scale_colour_viridis_c()+
  geom_point()
p

anim_q_multiplier1 <- p + transition_states(q_multiplier1, transition_length = 2, state_length = 2) +
  ggtitle('q multiplier TR1 {closest_state}')
anim_q_multiplier1

anim_save(anim_q_multiplier1, filename = "outputs_by_q_multiplier1.gif", width = 20, height = 18, unit = "cm", res = 200)

## 4.4 By q_multiplier2
p <- ggplot(all_biols_spread2, aes(x = COD, y = HAD, col = q_multiplier1)) +
  facet_wrap(.~ indicator, scales = "free") +
  scale_colour_viridis_c()+
  geom_point()
p

anim_q_multiplier2 <- p + transition_states(q_multiplier2, transition_length = 2, state_length = 2) +
  ggtitle('q multiplier TR1 {closest_state}')
anim_q_multiplier2

anim_save(anim_q_multiplier2, filename = "outputs_by_q_multiplier2.gif", width = 20, height = 18, unit = "cm", res = 200)

