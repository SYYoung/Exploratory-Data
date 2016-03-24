book_hier <- function() {
    set.seed(1234)
    x <- rnorm(12, rep(1:3, each=4), 0.2)
    y <- rnorm(12, rep(c(1,2,1), each=4), 0.2)
    plot(x, y, col="blue", pch=19, cex=2)
    text( x+0.05, y+0.05, labels = as.character(1:12))
    
    dataMatrix <- data.frame(x=x, y=y) %>% data.matrix
    heatmap(dataMatrix)
}

book_kmeans <- function(){
    set.seed(1234)
    x <- rnorm(12, rep(1:3, each=4), 0.2)
    y <- rnorm(12, rep(c(1,2,1), each=4), 0.2)
    plot(x, y, col="blue", pch=19, cex=2)
    text( x+0.05, y+0.05, labels = as.character(1:12))
    
    dataFrame <- data.frame(x=x, y=y)
    dataMatrix <- as.matrix(dataFrame)[sample(1:12),]
    kmeansObj <- kmeans(dataMatrix, centers=3)
    names(kmeansObj)
    par(mfrow=c(1,2))
    image(t(dataMatrix)[,nrow(dataMatrix):1],yaxt="n",main="Original Data")
    image(t(dataMatrix)[,order(kmeansObj$cluster)],yaxt="n",main="Clustered Data")
}

book_dim <- function() {
    set.seed(12345)
    dataMatrix <- matrix(rnorm(400), nrow=40)
    image(1:10, 1:40, t(dataMatrix)[,nrow(dataMatrix):1])
}