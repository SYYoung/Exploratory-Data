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
   
   qplot(log(eno), data=maacs, geom="density")
   
   qplot(log(eno),data=maacs,geom="density",color=mopos)
   
   qploot(log(pm25),log(eno), data=maacs,color=mopos,geom=c("point","smooth"),
          method="lm")
    
}

week2_2_ggplot2 <- function() {
    # use ggplot to build up the plot layer by layer
    head(maacs[,1:3])   # it has 3 fields: logpm25, bmicat, NocturnalSympt
    g <- ggplot(maacs, aes(logpm25,NocturnalSympt))
    summary(g)
    # no layers in plot yet up to this point
    p <- g + geom_point()
    print(p)
    # or:
    g + geom_point()
    # more layers
    g + geom_point() + geom_smooth(method="lm")
    g + geom_point() + facet_grid(.~bmicat) + geom_smooth(method="lm")
    # modifying aesthetics
    g + geom_point(color="steelblue", size=4, alpha=1/2)
    g + geom_point(aes(color=bmicat), size=4, alpha=1/2)
    g + geom_point(aes(color=bmicat)) +labs(title="MAACS Cohort") +
        labs(x=expression("log "*PM[2.5]))
    g+geom_point(aes(color=bmicat),size=2,alpha=1/2) +
        geom_smooth(size=4,linetype=3,method="lm",se=FALSE)
    
}

week2_2_ggplot <- function() {
    # about axis limits
    g <- ggplot(testdat, aes(x=x,y=y))
    g + geom_line # you will see the extreme value on y-axis
    g + geom_line() + ylim(-3,3)
    
    # how to take continuous variables as conditional
    # calculate the deciles of the data
    cutpoints <- quantile(maacs$logno2_new, seq(0,1,length=4), na.rm=TRUE)
    # cut the data at the deciles and create a new factor variable
    maacs$no2dec <- cut(maacs$logno2_new, cutpoints)
    # see the levels of the newly created factor variable
    levels(maacs$no2dec) # [1]0.378,1.2  [2](1.2,1.42)...
    
    # code for final plot
    # setup ggplot with data frame
    g <- ggplot(maacs,aes(logpm25, NocturnalSympt))
    ## add layers
    g + geom_point(alpha=1/3)
        + facet_wrap(bmicat~no2dec, nrow=2, ncol=4)
        + geom_smooth(method="lm",se=FALSE,col="steelblue")
        + theme_bw(base_family="Avenir", base_size=10)
        + labs(x=expression("log"*PM[2.5]))
        + labs(y="Nocturnal Symptoms")
        + labs(title="MACCS Cohort")
    
    
}