assg4_1 <- function() {
    library("dplyr")
    # 1. read two data files
    pm25 <- readRDS("./My Data/summarySCC_PM25.rds")
    scc <- readRDS("./My Data/Source_Classification_Code.rds")
    
    # 1. question 1:total emission decreased from 1999 to 2008
    # using base plotting: total emission for each year
    # group data based on year group, then add up the emission for
    # each year group
    # check how many rows in each year group
    sapply(split(pm25, pm25$year), nrow)
    # get the sum of emission for each year group
    nes_sum <- with(pm25, tapply(Emissions, year, sum, na.rm=TRUE))
    yr_range <- as.numeric(names(nes_sum))
    plot(yr_range, log10(nes_sum), type='l')
    # print this plot into a png file: plot1
    png(filename="assg4_plot1.png", width=480, height=480)
    plot(yr_range, log10(nes_sum), type='l')
    dev.off()
    
    # 2. question 2: total emission in Baltimore City (fips==24510) from 
    # 1999 to 2008.
    pm25_bal <- subset(pm25, fips=="24510")
    # do similar things as question #1
    sapply(split(pm25_bal, pm25_bal$year), nrow)
    nes_sum_bal <- with(pm25_bal, tapply(Emissions, year, sum, na.rm=TRUE))
    plot(yr_range, y=log10(nes_sum_bal), type='l')
    # print this plot into a png file: plot2
    png(filename="assg4_plot2.png", width=480, height=480)
    plot(yr_range, log10(nes_sum_bal), type='l')
    dev.off()
    
    # 3. question 3:  
    # divide pm25_bal into 4 tables for each type value
    result_data <- data.frame()
    type_level <- levels(factor(pm25_bal$type))
    for (i in seq_len(length(type_level))) {
        type_list <- subset(pm25_bal, type==type_level[i])
        sum_i <- with(type_list, tapply(Emissions, year, sum, na.rm=TRUE))
        temp <- data.frame(unclass(type_level[i]), sum_i, names(sum_i))
        names(temp) <- c("type","Emissions","year")
        result_data <- rbind(temp, result_data)
    }
    print(result_data)
    library(ggplot2)
    qplot(year,Emissions,data=result_data,facets=.~type)
    
}