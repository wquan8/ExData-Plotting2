## Coal Related PM2.5 emission shows decrease over years
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

plot4 <- function ()
{
        sccData <- SCC[,c("SCC","EI.Sector")]
        sccData$SCC <- as.character(sccData$SCC)
        sccData$EI.Sector <- as.factor(sccData$EI.Sector)
        
        #merge NEI with SCC to get EI.sector data (for coal query)
        pmData <- merge(x=NEI,y=sccData, by = "SCC")
        pmData$type <- as.factor(pmData$type)
        
        pmData2 <- pmData[pmData$EI.Sector == "Fuel Comb - Electric Generation - Coal",]
        
        pmPlotData <- ddply(pmData2,.(year),summarise,AnualTotal=sum(Emissions))
        
        bp <- qplot(year, AnualTotal, data=pmPlotData, geom=c("point","smooth"), method="lm")
        bp + ggtitle("Coal Related PM2.5 Emissions Over Time")
        
        png(filename="plot4.png",width=480, height=480, units="px")
        bp <- qplot(year, AnualTotal, data=pmPlotData, geom=c("point","smooth"), method="lm")
        bp + ggtitle("Coal Related PM2.5 Emissions Over Time")
        dev.off()
}

plot4()

