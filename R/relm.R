relm <- function(x_sdf,matrix=FALSE){
  if(sum(names(x_sdf)=="status")==1) sdf <- x_sdf[,-which(names(x_sdf)=="status")]
  else (sdf <- x_sdf)
  m <- ncol(sdf)
  r <- apply(as.matrix(sdf[,-c(m-1,m)]),1,max)
  if(matrix==FALSE) return(cbind(r,sdf[,c(m-1,m)]))
  else {
    p <- max(sdf[,m])
    mat <- matrix(ncol=p,nrow=p)
    for(i in 1:nrow(sdf)){
      mat[sdf[i,m-1],sdf[i,m]] <- r[i]
      mat[sdf[i,m],sdf[i,m-1]] <- r[i]
    }
    for (j in 1:p){
      mat[j,j] <- 1
    }
    return(mat)
  }
}