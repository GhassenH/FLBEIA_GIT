##############################################################################################
# title : summarizing data of the assessement model outputs into a multidimensional array 
# Author : Ghassen Halouani
# Date of creation : June 2019
# Script and data info: outputs_array_all_AM_data.RData
##############################################################################################

## 1. Preparing data 
all_biols2 <- rbind(outputs_min_AM_df2, outputs_max_AM_df2, outputs_mean_AM_df2, outputs_previous_AM_df2)
all_biols3 <- all_biols2[,-c(1,4,7,13)]
all_biols4 <- subset(all_biols3, !(indicator %in% c("catch.iyv", "disc.iyv", "land.iyv")))

## 2. preparing dimension
ind <-levels(as.factor(all_biols4$indicator))
stk <-levels(as.factor(all_biols4$stock))
effrest <-levels(as.factor(all_biols4$eff.rest))
y <-levels(as.factor(all_biols4$year))

l1 <- length(ind)
l2 <- length(y)
l3 <- length(stk)
l4 <- length(effrest)

# preparing the indices for the loop and to subset data
ni <- 1:l1
nj <- 1:l2
nh <- 1:l3
nk <- 1:l4

grid <- expand.grid(ni, nj, nh, nk)
grid_ch <- expand.grid(ind, y, stk, effrest)

# empty array to fill
# ar <- array(NA, c(225, ncol(all_biols4), l1, l2, l3, l4) )
ar <- array(NA, c(225, 4, l1, l2, l3, l4) )

# filling the array
for (i in 1:nrow(grid) ) {
        a <- subset(all_biols4, indicator == grid_ch[i,1] & year == grid_ch[i,2] & stock == grid_ch[i,3] & eff.rest == grid_ch[i,4])
        a <- a[,c(1,4,7,8)] 
        ar[,, grid[i,1], grid[i,2], grid[i,3], grid[i,4] ] <- as.matrix(a) 
        print(i)
}

## 3. saving data
data.array <- ar
save(data.array, file = "output_analysis/outputs_array_all_AM_data.RData")
dim(data.array)

