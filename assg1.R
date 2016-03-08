read_assg1_data <- function() {
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
    my_data <- read_assg1_data()
    png(filename="plot1.png", width=480, height=480)
    hist(my_data$Global_active_power, xlab="Global Active Power(kilowatts)", 
         col="red", main="Global Active Power")
    dev.off()
}

plot2 <- function() {
    my_data <- read_assg1_data()
    png(filename="plot2.png",width=480,height=480)
    with(my_data, plot(x=Date_Time, y=Global_active_power,type="l",
                       ylab="Global Active Power(kilowatts)")) 
    dev.off()
}

plot3 <- function() {
    my_data <- read_assg1_data()
    png(filename="plot3.png", width=480, height=480)
    plot(range(my_data$Date_Time),range(my_data$Sub_metering_1),
         xlab=" ", ylab="Energy sub metering", type="n")
    points(my_data$Date_Time,my_data$Sub_metering_1, type="l")
    points(my_data$Date_Time,my_data$Sub_metering_2, type="l", col="red")
    points(my_data$Date_Time,my_data$Sub_metering_3, type="l",col="blue")
    
    legend("topright", col=c("grey","red", "blue"), pch="----",
           legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
    
    dev.off()
}

plot4 <- function() {
    my_data <- read_assg1_data()
    #png(filename="plot4.png", width=480, height=480)
    par(mfrow=c(2,2),mar=c(5,4,2,1))
    
    #plot 1st: same as plot#2 w/o the title and xlab
    with(my_data, plot(x=Date_Time, y=Global_active_power,type="l",
                       xlab=" ", ylab="Global Active Power")) 
    # plot 2nd
    with(my_data, plot(x=Date_Time, y=Voltage,type="l",
                       xlab="datetime", ylab="Voltage"))
    # plot 3rd: same as plot#3 w/o xlab
    plot(range(my_data$Date_Time),range(my_data$Sub_metering_1),
         xlab=" ", ylab="Energy sub metering", type="n")
    points(my_data$Date_Time,my_data$Sub_metering_1, type="l")
    points(my_data$Date_Time,my_data$Sub_metering_2, type="l", col="red")
    points(my_data$Date_Time,my_data$Sub_metering_3, type="l",col="blue")
    legend("topright", col=c("grey","red", "blue"), pch="----",
           legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
    
    # plot 4th
    with(my_data, plot(x=Date_Time, y=Global_reactive_power,type="l",
                       xlab="datetime", ylab="Global Reactive Power"))
     
}