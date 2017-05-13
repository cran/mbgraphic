valueflip <- function(xy){
  x <- xy[1]
  y <- xy[2]
  if(x>y) return(c(y,x))
  else(return(c(x,y)))
}

### quick sort for a list of class "sdfdata"
sdf_quicksort <- function(sdfdata){
  if(class(sdfdata)!="sdfdata") stop("sdfdata needs to be of class sdfdata. Please use the functions sdf and scagn2sdf.")
  x_sdf <- sdfdata[[1]]
  x <- sdfdata[[2]]
  if(sum(names(x_sdf)=="status")==1) sdf <- x_sdf[,-which(names(x_sdf)=="status")]
  else (sdf <- x_sdf)
  p <- ncol(x)
  te <- sdf[order(sdf$y),]
  te <- te[order(te$x),]
  co <- cor(x,use="pairwise.complete.obs")
  distx <- 1-as.dist(co)
  al <- seriate(distx,method="OLO",control=list(method="average"))
  ord <- get_order(al)
  x_s <- x[, ord] 
  x_n <-rep(0,nrow(te))
  y_n <-rep(0,nrow(te))
  for(i in 1:p){
    x_n[which(te$x==ord[i])] <- i
    y_n[which(te$y==ord[i])] <- i
  }
  xy <- cbind(x_n,y_n)
  xy <- t(apply(xy,1,valueflip))
  sdf_s <- cbind(te[,1:(ncol(te)-2)],x=xy[,1],y=xy[,2])
  sdf_s <- sdf_s[order(sdf_s[,ncol(te)]),]
  sdf_s <- sdf_s[order(sdf_s[,ncol(te)-1]),]
  ret <- list(sdf=sdf_s,data=x_s)
  class(ret) <- "sdfdata"
  return(ret)
}