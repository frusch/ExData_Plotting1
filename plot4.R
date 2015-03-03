plot4 <- function(file){
        data_header <- read.table(file, header = TRUE, nrow = 1, sep = ";") ##reading the headers
        nc <- ncol(data_header)  ##number of columns
        data_dates <- read.table(file, header = TRUE, as.is = TRUE, sep = ";",
                                 colClasses = c(NA, rep("NULL", nc - 1)))  ##reading the first date column
        first <- which.max((data_dates$Date == "1/2/2007"))  ##number of the first row with given date
        number_of_rows = sum((data_dates$Date == "2/2/2007") | (data_dates$Date == "1/2/2007")) ##number of rows with given date
        data <- read.table(file, col.names = names(data_header), skip = first, as.is = TRUE, 
                           sep = ";", nrows = number_of_rows, na.strings = "?")  ##reading rows in given date only
        data$Time <- strptime(paste(data$Date,data$Time),"%d/%m/%Y %H:%M:%S")  ##convert to time class
        data$Date <- as.Date(data$Date, "%d/%m/%Y") ##convert to date class
        par(mfrow = c(2,2)) ##change global parametr to draw 4 plots in a 2x2 array
        ##first plot
        plot(data$Time, data$Global_active_power, type = "l", xlab = "", 
             ylab = "Global Active Power (kilowatts)")
        ##second plot
        plot(data$Time, data$Voltage, type = "l", xlab = "datetime", ylab = "Voltage")
        ##third plot
        plot(data$Time, data$Sub_metering_1, type = "n", xlab = "", 
             ylab = "Energy sub metering")
        lines(data$Time, data$Sub_metering_1, col = "black")
        lines(data$Time, data$Sub_metering_2, col = "red")
        lines(data$Time, data$Sub_metering_3, col = "blue")
        legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
               col = c("Black", "red", "blue"), lty = 1, cex = 0.7, bty = "n")
        ##fourth plot
        plot(data$Time, data$Global_reactive_power, type = "l", xlab = "datetime", ylab = "Global_reactive_power")
        dev.copy(png, file = "plot4.png") ##save as png
        dev.off()
        par(mfrow = c(1,1)) ##return global parametr to the default
}