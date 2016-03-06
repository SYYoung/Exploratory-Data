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

swirl_1_4 <- function() {
    with(cars, plot(speed,dist))
    text(mean(cars$speed), max(cars$dist), "SWIRL rules!")
    
    table(state$region)
    
    # lattice package: use xyplot
    xyplot(Life.Exp~Income|region, data=state, layout=c(4,1))
    
    # ggplot2 package: qplot:
    table(mpg$model)
    qplot(displ, hwy, data=mpg)
    
    
}

swirl_1_5 <- function() {
    # base plotting system
    range(airquality$Ozone, na.rm=TRUE)
    boxplot(Ozone~Month,airquality,xlab="Month",ylab="Ozone (ppb)",
            col.axis="blue",col.lab="red")
    
    with(airquality, plot(Wind, Ozone))
    par("lty")
    
    plot(airquality$Wind, airquality$Ozone, type="n")
    title(main="Wind and Ozone in NYC")
    may <- subset(airquality, Month==5)
    points(may$Wind, may$Ozone, col="blue",pch=17)
    
    notmay <- subset(airquality, Month!=5)
    points(notmay$Wind, notmay$Ozone, col="red", pch=8)
    legend("topright",pch=c(17,8), col=c("blue","red"), 
           legend=c("May","Other Months"))
    abline(v=median(airquality$Wind, lty=2, lwd=2))
    
    par(mfrow=c(1,2))
    plot(airquality$Wind, airquality$Ozone, main="Ozone and Wind")
    plot(airquality$Ozone, airquality$Solar.R, main="Ozone and Solar Radiation")
    
    # 3 plots
    par(mfrow=c(1,3), mar=c(4,4,2,1), oma=c(0,0,2,0))
    plot(airquality$Wind, airquality$Ozone, main="Ozone and Wind")
    plot(airquality$Solar.R, airquality$Ozone, main="Ozone and Solar Radiation")
    plot(airquality$Temp, airquality$Ozone, main="Ozone and Temperature")
    mtext("Ozone and Weather in New York City", outer=TRUE)
    
    
}