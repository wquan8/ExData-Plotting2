## POINT PM2.5 emission increase while all other types decrease over years
library(data.table)
library(ggplot2)
setwd("~/classes/exploratoryDataAnalysis/ExData-Plotting2")

fileName <- "PM25Dataset.zip"
if (!file.exists(fileName)) {
        print("Fetching data from source...")
        url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
        download.file(url, fileName, "curl", T)
}

unzippedFile1 <- "summarySCC_PM25.rds"
unzippedFile2 <- "Source_Classification_Code.rds"
if (!file.exists(unzippedFile2))
{
        print("unzipping downloaded archive...")
        unzip(fileName)
}

NEI <- readRDS(unzippedFile1)
SCC <- readRDS(unzippedFile2)

plot3 <- function ()
{
        pmData <- NEI[NEI$Pollutant == "PM25-PRI" & NEI$fips == "24510",]
        pmData$type <- as.factor(pmData$type)
        
        pmPlotData <- ddply(pmData,.(year,type),summarise,AnualTotal=sum(Emissions))
        
        bp <- qplot(year, AnualTotal, data=pmPlotData, color=type, geom=c("point","smooth"), method="lm",facets = .~ type)
        bp + ggtitle("Baltimore PM2.5 Emissions Over Time")
        
        png(filename="plot3.png",width=960, height=480, units="px")
        bp <- qplot(year, AnualTotal, data=pmPlotData, color=type, geom=c("point","smooth"), method="lm",facets = .~ type)
        bp + ggtitle("Baltimore PM2.5 Emissions Over Time")
        dev.off()
}

plot3()

