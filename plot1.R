## plot1 shows PM2.5 change over years. The trendline shows the PM2.5 level reduced significantly from 1999 to 2008, data points are close to line
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

plot1 <- function ()
{
        pmData <- NEI[NEI$Pollutant == "PM25-PRI",]
        
        pmPlotData <- ddply(pmData,.(year),summarise,AnualTotal=sum(Emissions))
        
        with(pmPlotData, plot(year, AnualTotal))
        title(main = "PM2.5 Emissions Over Time")
        model <- lm(AnualTotal ~ year, pmPlotData)
        abline(model, lwd = 2)
        
        #boxplot(AnualTotal ~ year, pmPlotData, xlab = "Year", ylab = "Total PM2.5 Emissions", main="PM2.5 Emissions Over Time")
        
        png(filename="plot1.png",width=480, height=480, units="px")
        with(pmPlotData, plot(year, AnualTotal))
        title(main = "PM2.5 Emissions Over Time")
        model <- lm(AnualTotal ~ year, pmPlotData)
        abline(model, lwd = 2)
        dev.off()
}

plot1()

