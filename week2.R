# the lattice system is ideal for creating conditioning plots where you examine
# the same kind of plot under many different conditions
week2_1_lattice <- function() {
    # lattice plotting system
    # e.g. xyplot, bwplot, histogram, stripplot, dotplot, splom,
    # levelplot, contourplot
    library(lattice)
    library(datasets)
    ## simple scatterplot
    xyplot(Ozone~Wind, data=airquality)
    
    airquality <- transform(airquality, Month=factor(Month))
    xyplot(Ozone ~ Wind | Month, data=airquality, layout=c(5,1))
    
    set.seed(10)
    x <- rnorm(100)
    f <- rep(0:1, each=50)
    y <- x + f = f * x + rnorm(100, sd=0.5)
    f <- factor(f, labels=c("Group 1", "Group 2"))
    xyplot(y ~ x |f, layout=c(2,1)) ## plot with 2 panels
    
    
    ## custom panel function
    xyplot(y ~ x|f, panel=function(x,y,...) {
        panel.xyplot(x,y,...)
        panel.abline(h=median(y),lty=2)
    })
       
}

week2_1_ggplot2 <- function() {
   install.packages("ggplot2")
   # the basic: qplot
   library(ggplot2)
   str(mpg)
   qplot(displ, hwy, data=mpg, color=drv)
   
   qplot(displ, hwy, data=mpg, geom=c("point","smooth"))
   
   qplot(hwy, data=mpg, fill=drv)
   
   qplot(displ, hwy, data=mpg, facets=.~drv)
   
   qplot(hwy,data=mpg, facets=drv~.,binwidth=2)
   
   qplot(log(pm25), log(eno), data=maacs, color=mopos)
   
   qplot(log(pm25),log(eno), data=maacs, color=mopos,geom=c("point","smooth"),
         method="lm")
   
}