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
    plot(yr_range, log10(nes_sum), type='b', ylab="Total Emission in log10",
         xlab="Year")
    # print this plot into a png file: plot1
    png(filename="assg4_plot1.png", width=480, height=480)
    plot(yr_range, log10(nes_sum), type='b',ylab="Total emission in log10",
         xlab="Year")
    dev.off()
    
    # 2. question 2: total emission in Baltimore City (fips==24510) from 
    # 1999 to 2008.
    pm25_bal <- subset(pm25, fips=="24510")
    # do similar things as question #1
    sapply(split(pm25_bal, pm25_bal$year), nrow)
    nes_sum_bal <- with(pm25_bal, tapply(Emissions, year, sum, na.rm=TRUE))
    plot(yr_range, y=log10(nes_sum_bal), type='b',ylab="log10(Emission in Baltimore)", 
         xlab="Year")
    # print this plot into a png file: plot2
    png(filename="assg4_plot2.png", width=480, height=480)
    plot(yr_range, log10(nes_sum_bal), type='b',ylab="log10(Emission in Baltimore", 
         xlab="Year")
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
    library(ggplot2)
    qplot(year,Emissions,data=result_data,facets=.~type)
    png(filename="assg4_plot3.png",width=480, height=480)
    print(qplot(year,Emissions, data=result_data,facets=.~type))
    dev.off()
    
    # 4. question 4: check across US, the trend of emissions from coal
    # combustion-related sources
    # get the subset which EI.Sector related to coal
    coal_code <- subset(scc, grepl("Coal",EI.Sector))
    coal_scc <- coal_code$SCC
    coal_data <- subset(pm25, SCC %in% coal_scc)
    coal_sum <- with(coal_data, tapply(Emissions, year, sum, na.rm=TRUE))
    yr_range <- as.numeric(names(coal_sum))
    plot(yr_range, log10(coal_sum), type='b',ylab="log10(Emission from coal
         combustion)", xlab="Year")
    png(filename="assg4_plot4.png",width=480,height=480)
    plot(yr_range, log10(coal_sum), type='b', ylab="log10(Emission from coal
         combustion)", xlab="Year")
    dev.off()
    
    # 5. question 5: emission from motor vehicles changed from 1999-2008
    # in Baltimore
    # step 1: get data related to Baltimore, similar to question 2
    # step 2: get subset from data 1 related to motor vehicle emission source.
    # similar to question #4
    pm25_bal <- subset(pm25, fips=="24510")
    mv_code <- subset(scc, grepl("Vehicle", EI.Sector))
    mv_scc <- mv_code$SCC
    mv_data_bal <- subset(pm25_bal, SCC %in% mv_scc)
    mv_sum_bal <- with(mv_data_bal, tapply(Emissions, year, sum, na.rm=TRUE))
    yr_range <- as.numeric(names(mv_sum_bal))
    plot(yr_range, mv_sum_bal, type='b',ylab="Emission in Baltimore from
         motor vehicle", xlab="Year")
    png(filename="assg4_plot5.png",width=480,height=480)
    plot(yr_range, mv_sum_bal, type='b', ylab="Emission in Baltimore from
         motor vehicle", xlab="Year")
    dev.off()
    
    #6. question 6: compare question 5 with LA County.
    # Repeat question 5 with LA county
    pm25_LA <- subset(pm25, fips=="06037")
    mv_data_LA <- subset(pm25_LA, SCC %in% mv_scc)
    mv_sum_LA <- with(mv_data_LA, tapply(Emissions, year, sum, na.rm=TRUE))
    yy <- range(c(mv_sum_bal, mv_sum_LA))
    png(filename="assg4_plot6.png", width=480, height=480)
    plot(yr_range, mv_sum_bal, type="n", ylim=yy)
    points(yr_range, mv_sum_bal, type="b",col="green")
    points(yr_range, mv_sum_LA, type="b", col="blue")
    dev.off()
}