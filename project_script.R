# Download the relevant data. Load the libraries to be used for the analysis.
library(dplyr) 
library(tidyr)
url1<-"https://www.epa.gov/sites/production/files/2016-08/temperature_fig-2.csv"
download.file(url1, destfile = "average_annual_worldwide_temperature_EPA.csv")
url2<-"https://www.epa.gov/sites/production/files/2016-08/precipitation_fig-2.csv"
download.file(url2, destfile = "average_global_precipitation_EPA")
url3<-"http://fenixservices.fao.org/faostat/static/bulkdownloads/Production_Crops_E_All_Data.zip"
download.file(url3, destfile = "Production_Crops_E_All_Data_FAO.csv", header=T)
url4<-"http://databank.worldbank.org/data/reports.aspx?source=2&series=SP.POP.TOTL&country=#"
download.file(url4, destfile = "population_data_WB.csv")
# Read the data into R, reshape and summarize them as necessary
# Annual Global change in population
population_data_WB<-read.csv("population_data_WB.csv", header=TRUE, skip=4)
population_data_WB_gather<-gather(population_data_WB, key= Year, value=Population, -c(Country.Name: Indicator.Code))
population_data_WB_gather$Year<-gsub(pattern = 'X', replacement = "", population_data_WB_gather$Year)
population_data_WB_gather$Year<-as.numeric(population_data_WB_gather$Year)
population_complete<-population_data_WB_gather[complete.cases(population_data_WB_gather),]
population_aggregated<-aggregate(population_complete$Population, by=list(population_complete$Year), FUN= sum)
#Annual Global Agricultural Produce
ww_agr_prod<-read.csv("Production_Crops_E_All_Data_FAO.csv", header=T)
ww_agr_yield<-filter(ww_agr_prod, ww_agr_prod$Element=="Yield")
ww_agr_yield_gather<-gather(ww_agr_yield, key = Year, value = Yield, -c(Area.Code:Unit))
ww_agr_yield_gather$Year<-gsub("Y", "", ww_agr_yield_gather$Year)
ww_agr_yield_gather$Year<-as.numeric(ww_agr_yield_gather$Year)
ww_agr_yield_gather$Yield<-as.numeric(ww_agr_yield_gather$Yield)
crops<-filter(ww_agr_yield_gather, ww_agr_yield_gather$Item %in% c("Maize", "Wheat", "Potatoes"))
crops_complete<-crops[complete.cases(crops),]
crops_global_average<-aggregate(x = crops_complete$Yield, by= list(crops_complete$Year, crops_complete$Item), FUN= mean)
maize<-filter(crops_global_average, crops_global_average$Group.2=="Maize")
wheat<-filter(crops_global_average, crops_global_average$Group.2=="Wheat")
potatoes<-filter(crops_global_average, crops_global_average$Group.2=="Potatoes")
# Annual Fluctation in global temperature and preciptation
ww_temp<-read.csv("annual_worldwide_temperature_EPA.csv", header = T, skip = 6)
ww_precip<-read.csv("annual_global_precipitation_EPA.csv", header=T, skip=6)
# Plot the data
par(mfrow=c(1,2))
plot(ww_temp$Year, ww_temp$Earth.s.surface..land.and.ocean., type = "o", main = "Annual Worldwide Temperature \n Annomaly (1901 - 2015)", xlab = "Year", ylab = "Temperature anomaly (oF)", ylim= c(-4, 4))
abline(h= 0.0, col="blue")
plot(ww_precip$Year, ww_precip$Anomaly..inches., type ="o", col="red", main = "Annual Annomaly in Worldwide \n Precipitation (1901 - 2015)", xlab = "Year", ylab = "Precipitation Anomaly (inches)", ylim= c(-6, 6))
abline(h= 0.0, col="blue")
par(mfrow=c(1,2))
plot(population_aggregated$Group.1, population_aggregated$x, main="Average Global Population Growth", xlab= "Year", ylab= "Population", type ="o")
plot(maize$Group.1, maize$x, xlim= c(1961, 2015), ylim= c(0, 200000), type="o", xlab="Year", ylab="Yield (hg/ha)", main ="Average Global Crop Production (1961-2015)")
points(maize$Group.1, potatoes$x, col="blue")
points(maize$Group.1, wheat$x, col="red")
