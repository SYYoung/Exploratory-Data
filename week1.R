week1_2_install <- function() {
    install.packages("ggplot2")
    
}

week1_2 <- function() {
    # base plotting system
    # used when you don't have idea what you want to plot, just explore
    # base plot
    library(datasets)
    data(cars)
    with(cars, plot(speed, dist))
    
    # lattice system
    library(lattice)
    state <- data.frame(state.x77, region=state.region)
    xyplot(Life.Exp ~ Income | region, data=state, layout=c(4,1))
    
    # ggplot2 system
    library(ggplot2)
    data(mpg)
    qplot(displ, hwy, data=mpg)
    
}

week1_2_base_1 <- function() {
   library(datasets)
   hist(airquality$Ozone)
   # scatterplot
   with(airquality, plot(Wind, Ozone))
   
   # boxplot
   airquality <- transform(airquality, Month=factor(Month))
   boxplot(Ozone~Month, airquality, xlab="Month", ylab="Ozone(ppb)")

}

week1_2_base_2 <- function() {
    # basic function: plot, lines, points, text, title, mtext, axis
    library(datasets)
    with(airquality, plot(Wind, Ozone))
    title(main="Ozone and Wind in New York City")
    
    with(subset(airquality, Month==5), points(Wind, Ozone, col="blue"))
    
    with(subset(airquality, Month!=5), points(Wind, Ozone, col="red"))
    legend("topright", pch=1, col=c("blue", "red"), 
            legend=c("May", "other Months"))
    
    
}

week1_2_base_2a <- function() {
    # base plot with regression line
    with(airquality, plot(Wind, Ozone, main="Ozone and Wind in New York City",
                          pch=20))
    model <- lm(Ozone~Wind, airquality)
    abline(model,lwd=2)
    
    # multiple base plots
    par(mfrow=c(1,2))
    with (airquality, {
        plot(Wind, Ozone, main="Ozone and Wind")
        plot(Solar.R, Ozone, main="Ozone and Solar Radiation")
    })
}