## motor vehicle sources in Baltimore vs LA - no trend up or down over years
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

plot6 <- function ()
{
        #search motor SCC code
        sccData <- SCC[,c("SCC","Short.Name", "EI.Sector")]
        sccData <- sccData[grep("Motor", SCC$Short.Name),]
        sccData$SCC <- as.character(sccData$SCC)
    
        #merge NEI with SCC to get EI.sector data (for coal query)
        pmData <- merge(x=NEI,y=sccData, by = "SCC")
        pmData$type <- as.factor(pmData$type)
        
        pmData2 <- pmData[pmData$fips == "24510" | pmData$fips == "06037",]
        
        pmPlotData <- ddply(pmData2,.(year, fips),summarise,AnualTotal=sum(Emissions))
        
        bp <- qplot(year, AnualTotal, data=pmPlotData, geom=c("point","smooth"), method="lm", facets = .~ fips)
        bp + ggtitle("LA(06037) VS. Baltimore(24510) motor PM2.5 Emissions Over Time")
        
        png(filename="plot6.png",width=640, height=480, units="px")
        bp <- qplot(year, AnualTotal, data=pmPlotData, geom=c("point","smooth"), method="lm", facets = .~ fips)
        bp + ggtitle("LA(06037) VS. Baltimore(24510) motor PM2.5 Emissions Over Time")
        dev.off()
}

plot6()

