# Clustering of numerical variables based on average linkage
# returns: the clusters and the cluster representatives
varclust <- function(data,c=NULL,mincor=NULL) {
  x <- data[,sapply(data,is.numeric)]
  p <- ncol(x)
  co <- cor(x,use = "pairwise.complete.obs")
  distx <- 1-as.dist(co)
  hc <- hclust(distx,method = "average")
  if(is.null(c) & is.null(mincor)) stop("You have to choose either c (number of cluster) oder mincor (minimal correlation)")
  if(!is.null(c) & !is.null(mincor)) stop("You have to choose either c (number of cluster) oder mincor (minimal correlation)")

  if(is.null(c) & !is.null(mincor)){
  if(mincor< -1 | mincor > 1)  stop("mincor must be in [0,1]")
  
  mincor_c <- - 5
  c <- 0
  for(i in 1:p){
    c <- c+1
    clusters <- cutree(hc,k=c)
    mini <- vector(length=c)
    for(i in 1:c){
      ci <- which(clusters==i)
      coi <- as.matrix(co[ci,ci])
      mini[i] <- min(coi)
    }
    mincor_c <- min(mini)
    if(mincor_c>=mincor) break
  }
  }
  
  if(c>p){
    warning("c is greater than number of numerical variables")
    c <- p
  }
  clusters <- cutree(hc,k=c)
  clusrep <- vector(length=c)
  for(i in 1:c){
    ci <- which(clusters==i)
    coi <- as.matrix(co[ci,ci])
    corsum <- vector(length=ncol(coi))
    for(j in 1:ncol(coi)){
      corsum[j] <- sum(coi[j,])
    }
    max <- order(corsum,decreasing = TRUE)[1]
    clusrep[i] <- ci[max]
  }
  dfclusrep <- x[,clusrep]
  mini <- vector(length=c)
  for(i in 1:c){
    ci <- which(clusters==i)
    coi <- as.matrix(co[ci,ci])
    mini[i] <- min(coi)
  }
  micor <- round(min(mini),2)
  return(list(c=c,mincor=micor,clusters=clusters, clusrep=names(x)[clusrep], dfclusrep=dfclusrep))
}
