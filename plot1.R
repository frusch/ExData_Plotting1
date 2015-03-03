plot1 <- function(file){
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
        hist(data$Global_active_power, col = "red", main = "Global Active Power", 
             xlab = "Global Active Power (kilowatts)") ##plotting the histogram
        dev.copy(png, file = "plot1.png") ##save plot as pn
        dev.off()
}