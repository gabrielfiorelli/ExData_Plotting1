#Date: Date in format dd/mm/yyyy
#Time: time in format hh:mm:ss
#Global_active_power: household global minute-averaged active power (in kilowatt)
#Global_reactive_power: household global minute-averaged reactive power (in kilowatt)
#Voltage: minute-averaged voltage (in volt)
#Global_intensity: household global minute-averaged current intensity (in ampere)
#Sub_metering_1: energy sub-metering No. 1 (in watt-hour of active energy). It corresponds to the kitchen, containing mainly a dishwasher, an oven and a microwave (hot plates are not electric but gas powered).
#Sub_metering_2: energy sub-metering No. 2 (in watt-hour of active energy). It corresponds to the laundry room, containing a washing-machine, a tumble-drier, a refrigerator and a light.
#Sub_metering_3: energy sub-metering No. 3 (in watt-hour of active energy). It corresponds to an electric water-heater and an air-conditioner.

# download and unzip data
#setwd('C:\Users\gabriel.fiorelli\datasciencecoursera\Course04Week01\CourseProject_1\ExData_Plotting1')

rm(list = ls())

createDir <- function(){
    if(!file.exists('data')){
        dir.create('data')
    }
}

downloadData <- function(){
    if(!file.exists('./data/electric_power_consumption.zip')){
        fileUrl <- 'https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip'
        download.file(fileUrl, destfile = './data/electric_power_consumption.zip')   
    }
}

unzipData <- function(){
    if(!file.exists('./data/household_power_consumption.txt')){
        unzip('./data/electric_power_consumption.zip', exdir = './data') 
    }
}

startEnvironment <- function(){
    createDir()
    downloadData()
    unzipData()
}

readAllLinesFromFile <- function(){
    # Read from file all the lines and them subseting.
    
    startEnvironment()
    
    dataFile <- './data/household_power_consumption.txt'
    data <- read.table(dataFile, header=TRUE, sep=";", stringsAsFactors=FALSE, dec=".")
    #subSetData <- data[data$Date %in% c("1/2/2007","2/2/2007") ,
    subSetData <- subset(data, Date %in% c("1/2/2007","2/2/2007"))
    
    return(subSetData)
}

readJUstNeededLinesFromFile <- function(){
    # Read from file just the lines required.
    
    startEnvironment()
    
    dataFile <- file('./data/household_power_consumption.txt')
    data <- read.table(text = grep("^[1,2]/2/2007", readLines(dataFile),value=TRUE),
                       sep = ';',
                       col.names = c("Date", "Time", "Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
                       na.strings = '?')
    
    return(subSetData)
}

## START PLOT 1
library(dplyr)
electricData <- readAllLinesFromFile()
electricData <- mutate(electricData, Global_active_power_numeric = as.numeric(Global_active_power))
#png(filename = './plot1.png', width = 480, height = 480, units='px')
with(electricData,
     hist(Global_active_power_numeric, 
          col = "red", 
          xlab = 'Global Active Power (kilowatts)',
          main = 'Global Active Power'
          )
     )
dev.copy(png, file = "plot1.png")
dev.off() #Close PNG
dev.off() #Close Window