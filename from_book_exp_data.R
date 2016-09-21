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