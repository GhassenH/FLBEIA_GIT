# =============================================
# Extract stock of WHG, COD and HAD in different areas
# =============================================

listStocks <- getListStocks(0)

# Whiting 
WHGstock2017 <- subset(listStocks, SpeciesName == "Merlangius merlangus" &
                         AssessmentYear == "2017")
WHG_summary <- getSummaryTable(assessmentKey = WHGstock2017$AssessmentKey)

# Cod 
CODstock2017 <- subset(listStocks, SpeciesName == "Gadus morhua" &
                         AssessmentYear == "2017")
COD_summary <- getSummaryTable(assessmentKey = CODstock2017$AssessmentKey)

# Haddok
HADstock2017 <- subset(listStocks, SpeciesName == "Melanogrammus aeglefinus" &
                         AssessmentYear == "2017")
HAD_summary <- getSummaryTable(assessmentKey = HADstock2017$AssessmentKey)

## plots
n_stk <- length(WHG_summary)
label_stk <- WHGstock2017$StockKeyLabel

WHG <- WHG_summary[[2]][,c("Year", "SSB", "recruitment")]
WHG$species <- 'WHG'


## data prepartion for plotting
n_stk_WHG <- length(WHG_summary)
label_stk_WHG <- WHGstock2017$StockKeyLabel

WHG_data <- data.frame()
for (i in 1:n_stk_WHG) {
  WHG <- WHG_summary[[i]][,c("Year", "SSB", "recruitment")]
  WHG$species <- 'WHG'
  WHG$Area <- label_stk_WHG[i]
  WHG_data <- rbind(WHG_data, WHG)
  print(i)
}


n_stk_COD <- length(COD_summary)
label_stk_COD <- CODstock2017$StockKeyLabel

COD_data <- data.frame()
for (i in 1:n_stk_COD) {
  COD <- COD_summary[[i]][,c("Year", "SSB", "recruitment")]
  COD$species <- 'COD'
  COD$Area <- label_stk_COD[i]
  COD_data <- rbind(COD_data, COD)
  print(i)
}


n_stk_HAD <- length(HAD_summary)
label_stk_HAD <- HADstock2017$StockKeyLabel

HAD_data <- data.frame()
for (i in 1:n_stk_HAD) {
  HAD <- HAD_summary[[i]][,c("Year", "SSB", "recruitment")]
  HAD$species <- 'HAD'
  HAD$Area <- label_stk_HAD[i]
  HAD_data <- rbind(HAD_data, HAD)
  print(i)
}


data <- rbind(COD_data, WHG_data, HAD_data)
data <- data[complete.cases(data), ]
# data_same_area <- subset(data, Area %in% label_stk_WHG)
View(data_same_area)
View(data)


# plot of rec and ssb in the same device
library(reshape)
library(ggplot2)
library(gridExtra) 


data.melt <- melt(data, id.vars = c("Year", "species", "Area"))

sr_plot <- ggplot (data = data.melt, aes(x = Year, y = value, colour = Area)) +
  geom_line() +
  # scale_x_continuous(limits = c(2000, 2017), breaks=seq(2000, 2017, 2)) +
  facet_wrap(variable~species, scales = "free")
sr_plot


## COD
data.melt <- melt(data, id.vars = c("Year", "species", "Area"))
data.melt <- subset(data.melt, species == "COD")
data.melt <- subset(data.melt, Area != "cod.27.1-2coast")

COD_plot <- ggplot (data = data.melt, aes(x = Year, y = value, color = variable)) +
  geom_line(aes(linetype=variable)) +
  theme(axis.text.y  = element_text(angle=0, vjust=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
  scale_x_continuous(minor_breaks=seq(1945, 2017, 1)) +
  facet_wrap(.~Area, scales = "free", ncol = 3) +
  ggtitle("COD")
COD_plot


## WHG
data.melt <- melt(data, id.vars = c("Year", "species", "Area"))
data.melt <- subset(data.melt, species == "WHG")

WHG_plot <- ggplot (data = data.melt, aes(x = Year, y = value, color = variable)) +
  geom_line(aes(linetype=variable)) +
  theme(axis.text.y  = element_text(angle=0, vjust=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
  scale_x_continuous(minor_breaks=seq(1945, 2017, 1)) +
  facet_wrap(.~Area, scales = "free", ncol = 2) +
  ggtitle("Whiting")
WHG_plot


## HAD
data.melt <- melt(data, id.vars = c("Year", "species", "Area"))
data.melt <- subset(data.melt, species == "HAD")

HAD_plot <- ggplot (data = data.melt, aes(x = Year, y = value, color = variable)) +
  geom_line(aes(linetype=variable)) +
  theme(axis.text.y  = element_text(angle=0, vjust=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
  scale_x_continuous(minor_breaks=seq(1945, 2017, 1)) +
  facet_wrap(.~Area, scales = "free", ncol = 3) +
  ggtitle("Haddock")
HAD_plot

plots.list = list(COD_plot, WHG_plot, HAD_plot)  # Make a list of plots

plots <- marrangeGrob( grobs = plots.list, nrow = 1, ncol = 1) # marrangeGrob: interface to arrangeGrob that can dispatch on multiple pages

ggsave("ssb_rec_cod_whg_had .pdf", plots, width = 29.7 , height = 21, units = "cm")








