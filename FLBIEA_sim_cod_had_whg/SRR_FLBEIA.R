# ============================================================================================
# plotting ssb and rec by species from the outputs of FLBEIA model for the period 2000 - 2030
# data from om and mpm 
# ============================================================================================

library(ggplot2)
library(FLBEIA)

## load data
load("FLBEIA_outputs.RData")

## species
sp <- "HAD"

## extacting and cleaning data to plot. (om = operating model, p = projection)
rec_p <- multiRes$SRs[[sp]]@rec
ssb_p <- multiRes$SRs[[sp]]@ssb

year <- 2000:2030
type <- c(rep("om", 17), rep("p", 14)) 
data <- data.frame(ssb = as.data.frame(ssb_p)$data, rec = as.data.frame(rec_p)$data, year = year, type = type)

## plotting SRR
sr_plot1 <- ggplot(data, aes(x = ssb, y = rec, label = year)) +
  geom_point(color = "grey40") +
  geom_path(aes(color = year), size = 1) +
  scale_color_gradientn(colours = terrain.colors(10)) +
  geom_text(size = 3) +
  ggtitle(sp) +
  theme_minimal() 
sr_plot1

##################### SRR Plots by species #####################

# COD parameter
sp <- "COD"
rec_p <- multiRes$SRs[[sp]]@rec
ssb_p <- multiRes$SRs[[sp]]@ssb

year <- 2000:2030
type <- c(rep("om", 17), rep("p", 14)) 
data <- data.frame(ssb = as.data.frame(ssb_p)$data, rec = as.data.frame(rec_p)$data, year = year, type = type)

a <- 8257.106
b <- 3155.035
sr <- function(x) {(a * x) / (b + x)}

sr_COD <- ggplot(data = data, aes(x = ssb, y = rec, color = type, label = year)) +
  geom_point() +
  geom_text(check_overlap = TRUE, hjust=0.5, vjust=-0.5, size = 3) +
  theme_minimal() +
  ggtitle(sp) +
  stat_function(fun = sr, col = "grey")
sr_COD

# WHG parameter
sp <- "WHG"
rec_p <- multiRes$SRs[[sp]]@rec
ssb_p <- multiRes$SRs[[sp]]@ssb

year <- 2000:2030
type <- c(rep("om", 17), rep("p", 14)) 
data <- data.frame(ssb = as.data.frame(ssb_p)$data, rec = as.data.frame(rec_p)$data, year = year, type = type)

a <- 1132525.277
b <- 2965.695
sr <- function(x) {(a * x) / (b + x)}

sr_WHG <- ggplot(data = data, aes(x = ssb, y = rec, color = type, label = year)) +
  geom_point() +
  geom_text(check_overlap = TRUE, hjust=0.5, vjust=-0.5, size = 3) +
  theme_minimal() +
  ggtitle(sp) +
  stat_function(fun = sr, col = "grey") 
sr_WHG


# HAD parameter
sp <- "HAD"
rec_p <- multiRes$SRs[[sp]]@rec
ssb_p <- multiRes$SRs[[sp]]@ssb

year <- 2000:2030
type <- c(rep("om", 17), rep("p", 14)) 
data <- data.frame(ssb = as.data.frame(ssb_p)$data, rec = as.data.frame(rec_p)$data, year = year, type = type)

a <- 562443.30
b <- 13911.46
sr <- function(x) {(a * x) / (b + x)}

sr_HAD <- ggplot(data = data, aes(x = ssb, y = rec, color = type, label = year)) +
  geom_point() +
  geom_text(check_overlap = TRUE, hjust=0.5, vjust=-0.5, size = 3) +
  theme_minimal() +
  ggtitle(sp) +
  stat_function(fun = sr, col = "grey") +
  xlim(0,1.5e5)
sr_HAD
