swirl_1_1 <- function() {
    summary(pollution$pm25)
    
    quantile(ppm)
    
    boxplot(ppm, col="blue")
    abline(h=12)
    
    hist(ppm, col="green")
    abline(v=12, lwd=2)
    abline(v=median(ppm), col="magenta", lwd=4)
    
    reg <- table(pollution$region)
    barplot(reg, col="wheat", main="Number of Counties in Each Region")
    
    boxplot(pm25~region, data=pollution, col="red")
    
    par(mfrow=c(2,1),mar=c(4,4,2,1))
    
    east <- subset(pollution, region=="east")
    
    with(pollution, plot(latitude,pm25))
    
    par(mfrow=c(1,2),mar=c(5,4,2,1))
    west <- subset(pollution, region=="west")
    plot(west$latitude,west$pm25,main="West")
    plot(east$latitude,east$pm25,main="East")
    
    
}

swirl_1_3 <- function() {
    # scatter plot between eruptions and waiting
    with(faithful, plot(eruptions, waiting))
    title(main="Old Faithful Geyser data")
    
    dev.cur()
    
    pdf(file="myplot.pdf")
    with(faithful, plot(eruptions, waiting))
    title(main="Old Faithful Geyser data")
    dev.off()
    
}