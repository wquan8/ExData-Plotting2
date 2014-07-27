## Although the trendline is down, some points are not close to the trendline. We might be able to say emissioin is reduced, but the evidence
## is not strong
library(data.table)
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

plot2 <- function ()
{
        pmData <- NEI[NEI$Pollutant == "PM25-PRI" & NEI$fips == "24510",]
        
        pmPlotData <- ddply(pmData,.(year),summarise,AnualTotal=sum(Emissions))
        
        with(pmPlotData, plot(year, AnualTotal))
        title(main = "Baltimore PM2.5 Emissions Over Time")
        model <- lm(AnualTotal ~ year, pmPlotData)
        abline(model, lwd = 2)
        
        png(filename="plot2.png",width=480, height=480, units="px")
        with(pmPlotData, plot(year, AnualTotal))
        title(main = "Baltimore PM2.5 Emissions Over Time")
        model <- lm(AnualTotal ~ year, pmPlotData)
        abline(model, lwd = 2)        
        dev.off()
}

plot2()

