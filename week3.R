week3_1 <- function() {
    # hierarchical clustering part 1
    set.seed(1234)
    par(mar=c(0,0,0,0))
    x <- rnorm(12, mean=rep(1:3,each=4), sd=0.2)
    y <- rnorm(12, mean=rep(c(1,2,1), each=4), sd=0.2)
    plot(x,y,col="blue",pch=19,cex=2)
    text(x+0.05, y+0.05, labels=as.character(1:12))
    # run hier clustering
    # step1 : calculate distance
    dataFrame <- data.frame(x=x,y=y)
    dist(dataFrame)
    
    #hclust
    distxy <- dist(dataFrame)
    hClustering <- hclust(distxy)
    plot(hClustering)
    
    # heatmap
    set.seed(143)
    dataMatrix <- as.matrix(dataFrame)[sample(1:12),]
    heatmap(dataMatrix)
    
}

# a prettier dendrograms
myplclust <- function(hclust, lab=hclust$labels, lab.col=rep(1,length(hclust$labels)),
                      hang=0.1,...) {
    y <- rep(hclust$height, 2)
    x <- as.numeric(hclust$merge)
    y <- y[which(x<0)]
    x <- x[which(x<0)]
    x <- abs(x)
    y <- y[order(x)]
    x <- x[order(x)]
    plot(hclust, labels=FALSE, hang=hang,...)
    text(x=x,y=y[hclust$order]-(max(hclust$height)*hang), labels=lab[hclust$order],
         col=lab.col[hclust$order], srt=90, adj=c(1,0.5), xpd=NA,...)
}

kmean_clust_1 <- function() {
    set.seed(1234)
    par(mar=c(0,0,0,0))
    x <- rnorm(12, mean=rep(1:3,each=4), sd=0.2)
    y <- rnorm(12, mean=rep(c(1,2,1), each=4), sd=0.2)
    plot(x,y,col="blue",pch=19,cex=2)
    text(x+0.05, y+0.05, labels=as.character(1:12))
    
}

kmean_clust_2 <- function() {
    dataFrame <- data.frame(x,y)
    kmeansObj <- kmeans(dataFrame, centers=3)
    names(keansObj)
    
    par(mar=rep(0.2,4))
    plot(x, y, col=keansObj$cluster, pch=19, cex=2)
    points(keansObj$centers, col=1:3, pch=3, cex=3, lwd=3)
    
    #heapmaps
    set.seed(1234)
    dataMatrix <- as.matrix(dataFrame)[sample(1:12), ]
    keansObj2 <- keans(dataMatrix, centers=3)
    par(mfrow=c(1,2), mar=c(2,4,0.1,0.1))
    image(t(dataMatrix)[,nrow(dataMatrix):1], yaxt="n")
    image(t(dataMatrix)[,order(keanObj$cluster)],yaxt="n")
    
}