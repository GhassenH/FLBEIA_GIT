

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


source("function_run_flbeia_cod_had_whg_036.R")


m = 1
run_flbeia(m)


s3 <- list()
m_multiplier <- seq(0.5, 2, by = 0.25)
for (i in 1:length(m_multiplier)) {
  a <- run_flbeia(q_1 = m_multiplier[i], q_2 = 1, F_1 = 1, F_2 = 1)
  s3[[i]] <- a 
  print(i)
}


# simulating different catchabilities q and fishing effort
n <- 0
outputs <- list()
# q_multiplier <- seq(0.5, 2.5, by = 0.5)
F_multiplier <- seq(0, 2, by = 0.5)
for (i in 1:length(F_multiplier)) { 
  for (j in 1:length(F_multiplier)) { 
    for (h in 1:length(F_multiplier)) {
      a <- run_flbeia(m1 = F_multiplier[i], m2 = F_multiplier[j], m3 = F_multiplier[h])
      n <- n+1
      outputs[[n]] <- a 
      # names(outputs)[n] <- paste("mutipliers_", "m=",i, "m=,"j, "m=", h, sep="")
      print(i)
      print(j)
      print(h)
    }
  }
}

i <- rep (seq(0.5, 2.5, by = 0.5), each = 3*3)
j <- rep ( rep(seq(0.5, 2.5, by = 0.5), times = 3), each = 3)
h <- rep (seq(0.5, 2.5, by = 0.5), times = 3*3)
data <- data.frame(m1 = i, m2 = j, m3 = h)

hh <- lapply(data, run_flbeia(m1 = data[1], m2 = data[2], m3 = data[3]))


# ========================================


# simulating different catchabilities q and fishing effort
n <- 0
outputs <- list()
# q_multiplier <- seq(0.5, 2.5, by = 0.5)
F_multiplier <- seq(0, 2, by = 0.5)
for (i in 1:length(F_multiplier)) { 
  for (j in 1:length(F_multiplier)) { 
    for (h in 1:length(F_multiplier)) {
      
      tryCatch({
        a <- run_flbeia(m1 = F_multiplier[i], m2 = F_multiplier[j], m3 = F_multiplier[h])
        n <- n+1
        outputs[[n]] <- a 
        names(outputs)[n] <- paste("m=", i, "_", "m=", j, "_", "m=", h, sep="")
        print(i)
        print(j)
        print(h)
      },error=function(e){})
      
    }
  }
}
