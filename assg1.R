assg1 <- function() {
    # read set data: only read from 2007-02-01 and 2007-02-02 so that the 
    # read in data size will not be too huge
    feature_name <- c("Date","Global_active_power","Global_reactive_power",
                     "Voltage","Global_intensity","Sub_metering_1",
                     "Sub_metering_2","Sub_metering_3")
    testFileName <- "./household_power_consumption.txt"
    data1 <- read.table(testFileName, sep=";",na.strings="?",
                        nrows=2880, skip= 66637, stringsAsFactors=FALSE)
    data1
}