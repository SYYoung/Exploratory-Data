assg1 <- function() {
    # read set data: only read from 2007-02-01 and 2007-02-02 so that the 
    # read in data size will not be too huge
    feature_name <- c("Date","Time","Global_active_power","Global_reactive_power",
                     "Voltage","Global_intensity","Sub_metering_1",
                     "Sub_metering_2","Sub_metering_3")
    testFileName <- "./household_power_consumption.txt"
    data1 <- read.table(testFileName, sep=";",na.strings="?",
                        nrows=2880, skip= 66637, stringsAsFactors=FALSE)
    colnames(data1) <- feature_name  # assign column names
    # convert columns of "Data" and "Time" into POSITlt
    d2 <- paste(data1[,"Date"], data1[,"Time"])
    d2 <- strptime(d2,"%d/%m/%Y %H:%M:%S")
    data1 <- cbind(d2,data1)
    colnames(data1)[1] <- "Date_Time"
    
    data1
}

plot1 <- function() {
    my_data <- assg1()
    png(filename="plog1.png", width=480, height=480)
    hist(my_data$Global_active_power, xlab="Global Active Power(kilowatts)", 
         col="red", main="Global Active Power")
    dev.off()
}

plot2 <- function() {
    my_data <- assg1()
    png(filename="plot2.png",width=480,height=480)
    with(my_data, plot(x=Date_Time, y=Global_active_power,type="l",
                       ylab="Global Active Power(kilowatts)")) 
    dev.off()
}