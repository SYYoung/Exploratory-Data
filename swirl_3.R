swirl_3_hier <- function() {
    install.packages("fields")
    
    distxy <- dist(dataFrame)
    hc <- hclust(distxy)
    plot(hc) # plot dendrogram
    plot(as.dendrogram(hc))
    
    heatmap(mt) 
    hclust(dist(mt))
    
}

swirl_3_kmeans <- function() {
    points(cd, cy, col=c("red","orange","purple"), pch=3, cex=2, lwd=2)
    mdist(x,y,cx,cy)
    apply(distTmp, 2, which.min)
    
    points(x,y,pch=19,cex=2,col=cols1[newClust])
    tapply(x,newClust,mean)
    tapply(y,newClust,mean)
    # another round
    # draw new centroids
    points(newCx,newCy,col=cols1,pch=8,cex=2,lwd=2)
    
    # another round
    midst(x,y,newCx,newCy)
    newClust2 <- apply(distTmp2, 2, which.min)
    points(x,y,pch=19,cex=2,col=cols1[newClust2])
    
    # get new centroids
    tapply(x,newClust2,mean)
    tapply(y,newClust2,mean)
    # draw new centroids
    points(finalCx,finalCy,col=cols1,pch=9,cex=2,lwd=2)
    
    # an easier method: kmeans, dataFrame stores the x and y coord
    kmObj <- kmeans(dataFrame, centers=3)
    plot(x,y,col=kmObj$cluster,pch=19,cex=2)
    points(kmObj$centers,col=c("black","red","green"),pch=3,cex=3,lwd=3)
    
}

add_patt <- function() {
    set.seed(678910)
    for(i in 1:40){
        # flip a coin
        coinFlip <- rbinom(1,size=1,prob=0.5)
        # if coin is heads add a common pattern to that row
        if(coinFlip){
            dataMatrix[i,] <- dataMatrix[i,] + rep(c(0,3),each=5)
        }
    }
}

swirl_3_dim_red <- function() {
    install.packages("fields")
    install.packages("jpeg")
    install.packages("datasets")
    
    heatmap(dataMatrix) # the original one
    add_patt() # add pattern
    heap(dataMatrix)
    
    mat <- matrix(c(1,2,2,5,3,7),nrow=2,ncol=3)
    svd(mat)
    # normalize mat
    svd(scale(mat))
    
    # a constant matrix
    head(constantMatrix) # it is a 6x10 matrix, first 5 col are 0 and other
    # 5 col are 1
    
    # dealing with missing values
    
    # facedata
    a1 <- (svd1$u[,1] * svd1$d[1]) %*% t(svd1$v[,1])
    myImage(a1)
    a2 <- svd1$u[,1:2] %*% diag(svd1$d[1:2]) %*% t(svd1$v[,1:2])
    myImage(a2)
    
}

swirl_3_cluster <- function() {
    install.packages("fields")
    install.packages("jpeg")
    install.packages("datasets")
    
    sub1 <- subset(ssd, subject==1)
    mdist <- dist(sub1[,1:3])
    hclustering <- hclust(mdist)
     
    # using col 10:12
    mdist <- dist(sub1[,10:12])
    hclustering <- hclust(mdist)
    myplclust(hclustering, lab.col=unclass(sub1$activity))
    
    svd1 <- svd(scale(sub1[,-c(562,563)]))
    
    maxCon <- which.max(svd1$v[,2])
    mdist <- dist(sub1[,c(10:12,maxCon)])
    hclustering <- hclust(mdist)
    myplclust(hclustering, lab.col=unclass(sub1$activity))
    kClust <- kmeans(sub1[,-c(562,563)],centers=6)
    kClust <- kmeans(sub1[,-c(562,563)],centers=6,nstart=100)
    
}