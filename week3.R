dweek3_1 <- function() {
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
    keansObj2 <- kmeans(dataMatrix, centers=3)
    par(mfrow=c(1,2), mar=c(2,4,0.1,0.1))
    image(t(dataMatrix)[,nrow(dataMatrix):1], yaxt="n")
    image(t(dataMatrix)[,order(keanObj$cluster)],yaxt="n")
    
}

dim_red_1 <- function() {
    # add a pattern
    set.seed(678910)
    for (i in 1:40) {
        # flip a coin
        coinFlip <- rbinom(1, size=1, prob=0.5)
        # if coin is heads add a common pattern to that row
        if (coinFlip) {
            dataMatrix[i, ]<- dataMatrix[i,] + rep(c(0,3), each=5)
        }
    }
    par(mar=rep(0.2,4))
    image(1:10, 1:40, t(dataMatrix)[,nrow)dataMatrix):1])

    hh <- hclust(dist(dataMatrix))
    dataMatrixOrdered <- dataMatrix[hh$order, ]
    par(mfrow=c(1,3))
    image(t(dataMatrixOrdered)[,nrow(dataMatrixOrdered):1])
    plot(rowMeans(dataMatrixOrdered, 40:1, xlab="Row Mean", ylab="Row", pch=19)
    plot(colMeans(dataMatrixOrdered), xlab="Column", ylab="Column Mean", pch=19))
}

dim_red_2 <- function() {
    svd1 <- svd(scale(dataMatrixOrdered))
    par(mfrow=c(1,3))
    image(t(dataMatrixOrdered)[,nrow(dataMatrixOrdered):1])
    plot(svd1$u[,1],40:1,,xlab="Row",ylab="First left singular vector", pch=19)
    plot(svd1$v[,1],xlab="Column",ylab="First right singluar vector",pch=19)
    
}

dim_red_3 <- function() {
    # SVD is not working in matrix with NA in it. use imputing
    library(impute) ## available from http://bioconductor.org
    dataMatrix2 <- dataMatrixOrdered
    dataMatrix2[sample(1:100, size=40, replace=FALSE)] <- NA
    dataMatrix2 <- imput.knn(dataMatrix2)$data
    svd1 <- svd(scale(dataMatrixOrdered)); svd2 <- svd(scale(dataMatrix2))
    par(mfrow=c(1,2));plot(svd1$v[,1],pch=19); plot(svd2$v[,1],pch=19)
    
    #face example
    load("data/face.rda")
    image(t(faceData)[,nrow(faceData):1])
    
    svd1 <- svd(scale(faceData))
    ## Note that %*% is matrix multiplication
    #Here svd1$d[1] is a constant
    approx1 <- svd1$u[,1] %*% t(svd1$v[,1]) * svd1$d[1]
    # in these examples we need to make the diagonal matrix out of d
    approx5 <- svd1$u[,1:5] %*% diag(svd1$d[,1:5]) %*% t(svd1$v[,1:5])
    approx10 <- svd1$u[,1:10] %*% diag(svd1$d[,1:10]) %*% t(svd1$v[,1:10])
    par(mfrow=c(1,4))
    image(t(approx1)[,nrow(approx1):1],main="(a)")
    image(t(approx5)[,nrow(approx5):1],main="(b)")
    image(t(approx10)[,nrow(approx10):1],main="(c)")
    image(t(faceData)[,nrow(faceData):1],main="(d)")
    
    
}

color_1 <- function() {
    # colorRamp
    pal <- colorRamp(c("red","blue"))
    pal(seq(0,1,len=10))
    
    # colorRampPalette
    pal <- colorRampPalette(c("red","yellow"))
    # pal(2) returns:#FF0000 and #FFFF00
    
    # RColorBrewer Package
    library(RColorBrewer)
    cols <- brewer.pal(3, "BuGn")
    cols
    pal <- colorRampPalette(cols)
    image(volcano, col=pal(20))
    
    x <- rnorm(10000)
    y <- rnorm(10000)
    smoothScatter(x,y)
    
    # transparency
    plot(x,y,col=rgb(0,0,0,0.2), pch=19) # alpha = 0.2
    
}
