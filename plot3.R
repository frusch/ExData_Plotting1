plot3 <- function(file){
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
        ##plotting three lines
        plot(data$Time, data$Sub_metering_1, type = "n", xlab = "", ylab = "Energy sub metering")
        lines(data$Time, data$Sub_metering_1, col = "black")
        lines(data$Time, data$Sub_metering_2, col = "red")
        lines(data$Time, data$Sub_metering_3, col = "blue")
        legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
               col = c("Black", "red", "blue"), lty = 1,) ##add the legend
        dev.copy(png, file = "plot3.png") ##saving as png
        dev.off()
}