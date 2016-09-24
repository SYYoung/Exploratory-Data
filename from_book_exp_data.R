from_book_exp_data_ch4 <- function() {
    # managing data frames with dplyr package:
    # select, filter, arrange, rename, mutate, summarise, %>%
    install.packages("dplyr")
    library(dplyr)
    
    # select: can be used to select columns
    chicago <- readRDS("chicago.rds")
    names(chicago)[1:3]
    subset <- select(chicago, city:dptp)
    i <- match("city", names(chicago))
    subset <- select(chicago, ends_with("2"))
    
    #filter: used to extract subsets of rows
    chic.f <- filter(chicago, pm25tmean2 > 30)
    summary(chic.f$pm25tmean2)
    chic.f <- filter(chicago, pm25tmean2>30 & tmpd>80)
    
    # arrange: reorder rows of data frame
    chi <- arrange(chicago, date)
    chi <- arrange(chicago, desc(date))
    
    # rename
    chicago <- rename(chicago, dewpoint=dptp, pm25=pm25tmean2)
    
    # mutate
    chicago <- mutate(chicago, pm25detrend=pm25-mean(pm25,na.rm=TRUE))
    
    # group_by
    chicago <- mutate(chicago, year=as.POXIXlt(date)$year+1900)
    years <- group_by(chicago, year)
    summarise(years, pm25=mean(pm25,na.rm=TRUE),
              o3=max(o3tmean2, na.rm=TRUE),
              no2=median(no2mean2,na.rm=TRUE))
    qq <- quantile(chicago$pm25, seq(0,1,0.2), na.rm=TRUE)
    chicago <- mutate(chicago, pm25.quint=cut(pm25,qq))
    quint <- group_by(chicago, pm25.quint)
    summarize(quint, o3=mean(o3tmean2,na.rm=TRUE),
              no2=mean(no2tmean2,na.rm=TRUE))
    
    # %>%
    mutate(chicago, pm25.quint=cut(pm25,qq)) %>%
        group_by(pm25.quint) %>%
        summarize(o3=mean(o3tmean2,na.rm=TRUE),
                  no2=mean(no2tmean2,na.rm=TRUE))
    
}

from_book_exp_data_ch5 <- function() {
    library(readr)
    ozone <- read_csv("data/hourly_44201_2014.csv",
                      col_types="ccccinnccccccncnnccccccc")
    names(ozone) <- make.names(names(ozone))
    nrow(ozone)
    ncol(ozone)
    str(ozone)
    head(ozone[,c(6:7,10)])
    tail(ozone[,c(6:7,10)])
    # check if they are hourly data
    table(ozone$Time.Local)
    unique(ozone$State.Name)
    
    # check with external data
    summary(ozone$Samplle.Measurement)
    quantile(ozone$Sample.Measurement, seq(0,1,0.1))
    
    # try the easy solution first
    ranking <- group_by(ozone, State.Name, County.Name) %>%
        summarize(ozone = mean(Sample.Measurement)) %>%
        as.data.frame %>%
        arrange(desc(ozone))
    filter(ozone, State.Name=="California" & County.Name=="Mariposa") %>%
        mutate(month=factor(months(Date.Local), levels=month.name)) %>%
        group_by(month) %>%
        summarize(ozone=mean(Sample.Measurement))
    
    # challenge your solution
    set.seed(10234)
    N <- nrow(ozone)
    idx <- sample(N, N, replace=TRUE)
    ozone2 <- ozone[idx,]
    ranking2 <- group_by(ozone2, State.Name, County.Name) %>%
        summarize(ozone=mean(Sample.Measurement)) %>%
        as.data.frame %>%
        arrange(desc(ozone))
    cbind(head(ranking,10),
          head(ranking2,10))
    
    
}

from_book_exp_data_ch7 <- function() {
    class <- c("numeric","character","factor","numeric","numeric")
    pollution <- read.csv("avgpm25.csv",colClasses=class)
    
    # five-number summary
    fivenum(pollution$pm25)
    
    # boxplot
    boxplot(pollution$pm25, col="blue")
    library(dplyr)
    filter(pollution, pm25 > 15)
    # make a map
    library(maps)
    map("county","california")
    with(filter(pollution,pm25>15), points(longitude,latitude))
    
    # histogram
    hist(pollution$pm25, col="green")
    rug(pollution$pm25)
    hist(pollution$pm25, col="green", breaks=100)
    rug(pollution$pm25)
    
    # overlaying features
    boxplot(pollution$pm25, col="blue")
    abline(h=12)
    hist(pollution$pm25, col="green")
    abline(v=12, lwd=2)
    abline(v=median(pollution$pm25), col="magenta", lwd=4)
    
    # barplot
    table(pollution$region) %>% barplot(col="wheat")
    
    # advanced:
    # multiple boxplots: show relationship between 2 variables is side-by
    # side boxplot
    boxplot(pm25~region, data=pollution, col="red")
    
    # multiple histograms
    par(mfrow=c(2,1),mar=c(4,4,2,1))
    hist(subset(pollution, region=="east")$pm25, col="green")
    hist(subset(pollution, region=="west")$pm25, col="green")
    
    # scatterplot
    with(pollution, plot(latitude,pm25))
    abline(h=12,lwd=2,lty=2)
    # scatterplot - using color
    with(pollution, plot(latitude,pm25,col=region))
    abline(h=12,lwd=2,lty=2)
    levels(pollution$region)
    # multiple scatterplots
    par(mfrow=c(1,2), mar=c(5,4,2,1))
    with(subset(pollution,region=="west"),plot(latitude,pm25,main="West"))
    with(subset(pollution,region=="east"),plot(latitude,pm25,main="East"))
    
    # lattice
    library(lattice)
    xyplot(pm25 ~ latitude | region, data=pollution)
    
    # ggplot2
    library(ggplot2)
    qplot(latitude, pm25, data=pollution, facets=.~region)
    
}

from_book_exp_data_ch8 <- function() {
    
}