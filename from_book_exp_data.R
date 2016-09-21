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
    
    # arrange
}