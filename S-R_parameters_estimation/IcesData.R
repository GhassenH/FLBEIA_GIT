#################################################################
#
# Download stock assessment data for : 
# Haddock (Melanogrammus aeglefinus) in divisions 7.b-k (southern Celtic Seas and English Channel)
# Whiting (Merlangius merlangus) in divisions 7.b-c and 7.e-k (southern Celtic Seas and eastern English Channel)
# Cod (Gadus morhua) in divisions 7.e-k (eastern English Channel and southern Celtic Seas) 
# Year = 2017
#
##################################################################

# devtools::install_github("ices-tools-prod/icesSAG")
library(icesSAG) #ices stock assessement Graph


listStocks <- getListStocks(0)

# Haddock 
HaddocktSock2017 <- subset(listStocks, StockDatabaseID == "1345") # same as : HaddocktSock2017 <- subset(listStocks, SpeciesName == "Melanogrammus aeglefinus" & AssessmentYear== "2017" & StockKeyLabel == "had.27.7b-k")
sumtabHaddock <- getSummaryTable(assessmentKey = HaddocktSock2017$AssessmentKey)

# Whiting 
WhitingtSock2017 <- subset(listStocks, SpeciesName == "Merlangius merlangus" &
                                       AssessmentYear == "2017" &
                                       StockDatabaseID == "1508")
sumtabWhiting <- getSummaryTable(assessmentKey = WhitingtSock2017$AssessmentKey)

# Cod 
CodtSock2017 <- subset(listStocks, SpeciesName == "Gadus morhua" &
                                   AssessmentYear == "2017"&
                                   StockDatabaseID == "1324")
sumtabCod <- getSummaryTable(assessmentKey = CodtSock2017$AssessmentKey)

# convert smmary table of tthe stock assessement to dataframes
df_COD <- as.data.frame(sumtabCod)
df_WHG <- as.data.frame(sumtabWhiting)
df_HAD <- as.data.frame(sumtabHaddock)

# stock reference points
COD_rf_pts <- getFishStockReferencePoints(assessmentKey = CodtSock2017$AssessmentKey)
WHG_rf_pts <- getFishStockReferencePoints(assessmentKey = WhitingtSock2017$AssessmentKey)
HAD_rf_pts <- getFishStockReferencePoints(assessmentKey = HaddocktSock2017$AssessmentKey)


# Another method to extract data
WHG_summary <- getSAG(stock = "whg.27.7b-ce-k", year = 2017, data = "summary")
WHG_source <- getSAG(stock = "whg.27.7b-ce-k", year = 2017, data = "source")
WHG_refpts <- getSAG(stock = "whg.27.7b-ce-k", year = 2017, data = "refpts")

COD_summary <- getSAG(stock = "cod.27.7e-k", year = 2017, data = "summary")
COD_source <- getSAG(stock = "cod.27.7e-k", year = 2017, data = "source")
COD_refpts <- getSAG(stock = "cod.27.7e-k", year = 2017, data = "refpts")

HAD_summary <- getSAG(stock = "had.27.7b-k", year = 2017, data = "summary")
HAD_source <- getSAG(stock = "had.27.7b-k", year = 2017, data = "source")
HAD_refpts <- getSAG(stock = "had.27.7b-k", year = 2017, data = "refpts")








