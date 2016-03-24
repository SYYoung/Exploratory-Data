week4_air_poll1 <- function() {
  pm0 <- read.table("RD_501_88101_1999-0.txt", comment.char="#",
                    header=FALSE, sep="|", na.strings="")
  dim(pm0)
  cnames <- readLines("RD_501_88101_1999-0.txt",1)
  cnames <- strsplit(cnames, "|", fixed=TRUE)
  names(pm0) <- make.names(cnames[[1]]) # make a valid col names for data frame
  x0 <- pm0$Sample.Value
  summary(x0)
  # check how many NAs there are
  mean(is.na(x0))
  
  # now read 2012 data
  pm1 <- read.table("RD_501_88101_2012-0.txt", comment.char="#",
                    header=FALSE, sep="|",na.strings="")
  dim(pm1)
  names(pm1) <- make.names(cnames[[1]])
  x1 <- pm1$Sample.Value
  
  boxplot(log10(x0), log10(x1))
  
  # let see why there is negative vale
  negative <- x1 < 0
  sum(negative, na.rm=TRUE) # there is 26474, 2% values
  dates <- pm1$Date
  dates <- as.Date(as.character(dates), "%Y%m%d")
  hist(dates[negative],"month")
  
  # let pick up one monitor to investigate
  site0 <- unique(subset(pm0, State.Code==36, c(County.Code,Site.ID)))
  site1 <- unique(subset(pm1, State.Code==36, c(County.Code,Site.ID)))
  site0 <- paste(site0[,1],site0[,2],sep=".")
  site1 <- paste(site1[,1],site1[,2],sep=".")
  both <- intersect(site0, site1)
  # we check how many observations in each monitor
  pm0$county.site <- with(pm0, paste(County.Code, Site.ID, sep="."))
  pm1$county.site <- with(pm1, paste(County.Code, Site.ID, sep="."))
  cnt0 <- subset(pm0, State.Code==36 & county.site %in% both)
  cnt1 <- subset(pm1, State.Code==36 & county.site %in% both)
  sapply(split(cnt0,cnt0$county.site), nrow)
  sapply(split(cnt1, cnt1$county.site), nrow)
  
  # let see 63.2008
  pm1sub <- subset(pm1, State.Code==36 & County.Code==63 & Site.ID==2008)
  pm0sub <- subset(pm0, State.Code==36 & County.Code==63 & 
                     Site.ID==2008)
  # plot data vs time series
  dates1 <- as.Date(as.character(pm1sub$Date),"%Y%m%d")
  x1sub <- pm1sub$Sample.Value
  plot(dates1, x1sub)
  dates0 <- as.Date(as.character(pm0sub$Date),"%Y%m%d")
  x0sub <- pm0sub$Sample.Value
  par(mfrow=c(1,2),mar=c(4,4,2,1))
  plot(dates0,x0sub,pch=20)
  ablin(h=median(x0sub,na.rm=T))
  plot(dates1,x1sub)
  ablin(h=median(x1sub,na.rm=T))
  # put both plots under same range
  rng <- range(x0sub,x1sub,ran.rm=T)
  plot(dates0,x0sub,pch=20,ylim=rng)
  plot(dates1,x1sub,pch=20,ylim=rng)
  
  #let see indiv state
  # make a plot state by state
  mn0 <- with(pm0, tapply(Sample.Value, State.Code, mean, na.rm=T))
  mn1 <- with(pm1, tapply(Sample.Value, State.Code, mean, na.rm=T))
  d0 <- data.frame(state=names(mn0),mean=mn0)
  d1 <- data.frame(state=names(mn1),mean=mn1)
  mrg <- merge(d0,d1,by="state")
  par(mfrow=c(1,1))
  with(mrg, plot(rep(1999,52),mrg[,2],xlim=c(1998,2013)))
  with(mrg, points(rep(2012,52),mrg[,3]))
  segments(rep(1999,52),mrg[,2],rep(2012,52),mrg[,3])
  
  
  
}