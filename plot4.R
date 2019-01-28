library(data.table)
library(stringr)
library(lubridate)

plot4 <- function () {
        #download the zip file if not already dowloaded
        file_URL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
        if (!file.exists(".\\exdata_data_household_power_consumption.zip")) {
                download.file(file_URL,".\\exdata_data_household_power_consumption.zip")
        }
        #unzip the zip archive if not already unzipped
        if (!file.exists(".\\household_power_consumption.txt")) {
                unzip(".\\exdata_data_household_power_consumption.zip")
        }

        df <- fread(".\\household_power_consumption.txt", colClasses = c(Global_active_power = "num"))
        
        #select the only required records of the dataset
        df <- subset(df,df$Date == "1/2/2007" | df$Date == "2/2/2007")
        
        #change the columns types to make elaboratin possible
        df$Global_active_power <- as.numeric(df$Global_active_power)
        df$Global_reactive_power <- as.numeric(df$Global_reactive_power)
        df$Sub_metering_1 <- as.numeric(df$Sub_metering_1)
        df$Sub_metering_2 <- as.numeric(df$Sub_metering_2)
        df$Sub_metering_3 <- as.numeric(df$Sub_metering_3)
        df$Voltage <- as.numeric(df$Voltage)
        df$DateTime <- paste(df$Date,df$Time,sep = " ")
        df$DateTime <- as.POSIXct(df$DateTime,format="%d/%m/%Y %H:%M:%OS")

        #create the plot
        png(".\\plot4.png")
        par(mfrow=c(2,2))
        plot(df$DateTime,df$Global_active_power, type = "l", ylab = "Global Active Power", xlab = "")
        plot(df$DateTime,df$Voltage, type = "l", ylab = "Voltage", xlab = "datetime")
        plot(df$DateTime,df$Sub_metering_1,type="l", ylab = "Energy Sub Metering", xlab="")
        lines(df$DateTime,df$Sub_metering_2,type="l",col="red")
        lines(df$DateTime,df$Sub_metering_3,type="l",col="blue")
        legend("topright",legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), col = c("black","red","blue"), lty= c(1,1,1))
        plot(df$DateTime,df$Global_reactive_power, type = "l", ylab = "Gloabal Reactive Power", xlab = "datetime")
        dev.off()
}