# this is plot1.R which swirl_2_1 invoked:
plot1_R <- function() {}
    p <- xyplot(y ~ x | f, panel = function(x, y, ...) {
        panel.xyplot(x, y, ...)  ## First call the default panel function for 'xyplot'
        panel.abline(h = median(y), lty = 2)  ## Add a horizontal line at the median
    })
    print(p)
    invisible()
}

swirl_2_1 <- function() {
    ## lattice plot
    install.packages("lattice")
    install.packages("ggplot2")
    
    xyplot(Ozone~Wind, data=airquality)
    xyplot(Ozone~Wind|as.factor(Month), data=airquality, layout=c(5,1))
    
    p <- xyplodt(Ozone~Wind, data=airquality)
    print(p)
    
    # diamonds: 10 columns, 53940 observations
    table(diamonds$color)
    table(diamonds$color, diamonds$cut)
    xyplot(price~carat|color*cut, data=diamonds, strip=FALSE,
           pch=20, xlab=myxlab, ylab=myylab, main=mymain)
    
}

swirl_2_work_with_color <- function() {
    sample(colors(), 10)
    pal <- colorRamp(c("red","blue"))
    pal(seq(0,1,len=6))
    
    p1 <- colorRampPalette(c("red","blue"))
    
    p2 <- colorRampPalette(c("red","yellow"))
    
    # use colorBrewer palettes
    cols <- brewer.pal(3,"BuGn")
    pal <- colorRampPalette(cols)
    image(volcano, col=pal(20))
}

swirl_2_ggplot_1 <- function() {
    library(ggplot2)
    
    str(mpg)
    qplot(displ, hwy, data=mpg)
    qplot(displ, hwy, data=mpg, color=drv)
    qplot(displ, hwy, data=mpg, color=drv, geom=c("point","smooth"))
    
    qplot(drv, hwy, data=mpg, geom="boxplot")
    qplot(drv, hwy, data=mpg, geom="boxplot", color=manufacturer)
    qplot(hwy, data=mpg, fill=drv)
    qplot(displ, hwy, data=mpg, facets=~/.drv)
    qplot(hwy, data=mpg, facets=drv~., binwidth=2)
    
}

swirl_2_ggplot_2 <- function() {
    qplot(displ, hwy, data=mpg, geom=c("point","smooth"), facets=.~drv)
    # now use ggplot get same results
    g <- ggplot(mpg, aes(displ, hwy))
    g + geom_point()
    #(g+geom_point()) + geom_smooth()
    (g+geom_point()) + geom_smooth(method="lm")
    (g+geom_point()) + geom_smooth(method="lm")+ facet_grid(.~drv)
    + ggtitle("Swirl Rules")
    g+geom_point(size=4, alpha=1/2, aes(color=drv))
    
    g+geom_point(aes(color=drv)) + labs(title="Swirl Rules!") + 
        labs(x="Displayment",y="Hwy Mileage")
    g+geom_point(aes(color=drv)) + theme_bw(base_family="Times")
     g + geom_line() + coord_cartesian(ylim=c(-3,3))
    
    g <- ggplot(mpg, aes(x-displ, y=hwy,color=factor(year))) 
    # no display yet since ggplot hasn't been told how to display
    g + geom_point()
    g + geom_point() + facet_grid(drv~cyl,margins=TRUE) + 
        geom_smooth(method="lm",se=FALSE,size=2,color="black") +
        labs(x="Displacement",y="Highway Mileage",title="Swirl Rules!")
    
    
}

swirl_2_ggplot_extra <- funcion() {
    qplot(price, data=diamonds, binwidth=range(diamonds$price)/30)
    qplot(price, data=diamonds, binwidth=range(diamonds$price)/30,fill=cut)
    qplot(price, data=diamonds, geom="density")
    qplot(price, data=diamonds, geom="density", color=cut)
    
    qplot(carat,price,data=diamonds)
    qplot(carat,price,data=diamonds,color=cut)
    qplot(carat,price,data=diamonds,color=cut) + geom_smooth(method="lm")
    qplot(carat,price,data=diamonds,color=cut,facets=.~cut) +
        geom_smooth(method="lm")
    
    g <- ggplot(data=diamonds,aes(depth,price))
    cutpoints <- quantile(diamonds$carat, seq(0,1,length=4), na.rm=TRUE)
    diamonds$car2 <- cut(diaomonds$carat, cutpoints)
    g <- ggplot(data=diamonds,aes(depth,price))
    g+geom_point(alpha=1/3) + facet_grid(cut~car2)
    g + geom_point(alpha=1/3) + facet_grid(cut~car2) +
        geom_smooth(method="lm",size=3,color="pink")
    # boxplot
    ggplot(data=diamonds,aes(carat,price)) + geom_boxplot() +
        facet_grid(.~cut)
    
}