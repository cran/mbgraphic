cutcluster <- function(x,c=2){
  # c: number of clusters
  x <- x[,sapply(x,is.numeric)]
  p <- ncol(x)
  co <- cor(x,use = "pairwise.complete.obs")
  distx <- 1-as.dist(co)
  hc <- hclust(distx,method = "average")
  al <- seriate(distx,method="OLO",control=list(hclust=hc))
  ord <- get_order(al)
  cl <- cutree(hc,k=c)[ord]
  cuts <- vector(length=c-1)
  i <- 1  
  for(j in 1:(p-1)){
    if(cl[j]!=cl[j+1]) {
      cuts[i] <- j
      i <- i+1
    }
  }
  return(cuts)
}