
library(icesSAG) 
library(ggplot2)


## Download stock data

WHG_data <- getSAG(stock = "whg.27.7b-ce-k", year = 2017, data = "summary")
COD_data <- getSAG(stock = "cod.27.7e-k", year = 2017, data = "summary")
HAD_data <- getSAG(stock = "had.27.7b-k", year = 2017, data = "summary")

# ======================= Plot ssb and rec for all species ===================================
ssb <- HAD_data$SSB
rec <- HAD_data$recruitment
Year <- HAD_data$Year

lag <- 1 # lag should be > 0
rec_lag1 <- rec[-c((length(rec)-lag + 1):length(rec)) ]
ssb_lag1 <- ssb[-c(1:lag)]
Year <- Year[-c((length(Year)-lag + 1):length(Year)) ]

HAD_data_lag <- data.frame(Year, rec_lag1, ssb_lag1)

sr_plot <- ggplot (data = HAD_data_lag, aes(x = ssb_lag1, y = rec_lag1, label = Year)) +
  geom_point(color = "grey40") +
  geom_path(data = HAD_data_lag, aes(color = Year), size = 1) +
  scale_color_gradientn(colours = terrain.colors(10)) +
  geom_text(size = 3)
sr_plot

# plot of ssb(year+1) ~ ssb(year)
data <- HAD_data[,c("Year", "SSB", "recruitment")]
SSBplus <- data[-1, "SSB"]
SSBplus[length(SSBplus)+1] <- NA
data$SSBplus <- SSBplus

sr_plot <- ggplot (data = data, aes(x = SSBplus, y = recruitment, label = Year)) +
  geom_point(color = "grey40") +
  geom_path(aes(color = Year), size = 1) +
  scale_color_gradientn(colours = terrain.colors(10)) +
  geom_text(size = 3)
sr_plot

# ==========================

# plot of ssb(year-1) ~ ssb(year)
data <- COD_data[,c("Year", "SSB", "recruitment")]
SSBplus <- data[-1, "SSB"]
SSBplus[length(SSBplus)+1] <- NA
data$SSBplus <- SSBplus

sr_plot <- ggplot (data = data, aes(x = SSB, y = recruitment, label = Year)) +
  geom_point(color = "grey40") +
  geom_path(aes(color = Year), size = 1) +
  scale_color_gradientn(colours = terrain.colors(10)) +
  geom_text(size = 3)
sr_plot

##

COD <- COD_data[,c("Year", "SSB", "recruitment")]
COD$species <- 'COD'

WHG <- WHG_data[,c("Year", "SSB", "recruitment")]
WHG$species <- 'WHG'

HAD <- HAD_data[,c("Year", "SSB", "recruitment")]
HAD$species <- 'HAD'

data <- rbind(COD, WHG, HAD)

sr_plot <- ggplot (data = data, aes(x = Year, y = SSB)) +
  # geom_point(color = "grey40") +
  geom_point(aes(color = recruitment)) +
  facet_grid(species~., scales = "free") +
  geom_path(aes(color = recruitment), size = 1) +
  scale_x_continuous(limits = c(2000, 2017), breaks=seq(2000, 2017, 2)) +
  scale_color_gradientn(colours = terrain.colors(10)) 
sr_plot


# plot of rec and ssb in the same device
library(reshape)
data.melt <- melt(data, id.vars = c("Year", "species"))

sr_plot <- ggplot (data = data.melt, aes(x = Year, y = value)) +
  geom_line() +
  scale_x_continuous(limits = c(2000, 2017), breaks=seq(2000, 2017, 2)) +
  facet_wrap(variable~species, scales = "free")
sr_plot
