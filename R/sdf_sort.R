### sorts a list of class "sdfdata"
sdf_sort <- function(sdfdata,maxt=NULL,select="All",printmsum=FALSE){ 
  if(class(sdfdata)!="sdfdata") stop("sdfdata needs to be of class sdfdata. Please use the functions sdf and scagn2sdf.")
  x_sdf <- sdfdata[[1]]
  x <- sdfdata[[2]]
  if(sum(names(x_sdf)=="status")==1) sdf <- x_sdf[,-which(names(x_sdf)=="status")]
  else (sdf <- x_sdf)
  begin <- Sys.time()
  p <- ncol(x)
  te <- sdf[order(sdf$y),]
  te <- te[order(te$x),]
  ncols <- ncol(te)
  y <- x
  max <- 100
  d <- 1
  repeat{
    # current sum
    if(select[1]=="All") {msum <- cmasum(as.matrix(te))}
    else{
      if(is.character(select)){
        sel <- which(names(x_sdf) %in% c(as.character(select),"x","y"))
        msum <- cmasum(as.matrix(te[,sel]))
      }
      if(is.numeric(select)){
        sel <- c(select, which(names(te) %in% c("x","y")))
        msum <- cmasum(as.matrix(te[,sel]))
      }
    }
    # matrix with changes of msum
    mat <- matrix(ncol=3,nrow=p*(p-1)/2)
    k <- 1
    for(i in 1:(p-1)){
      for(j in (i+1):p){
        xy <- variableflip(as.matrix(te),i,j)
        te_akt <- as.data.frame(cbind(te[,1:(ncols-2)],x=xy[,1],y=xy[,2]))
        te_akt <- te_akt[order(te_akt$y),]
        te_akt <- te_akt[order(te_akt$x),]
        if(select[1]=="All") {mat[k,1] <- msum - cmasum(as.matrix(te_akt))} 
        else{
          if(is.character(select)){
            sel <- which(names(x_sdf) %in% c(as.character(select),"x","y"))
            mat[k,1] <- msum - cmasum(as.matrix(te_akt[,sel]))
          }
          if(is.numeric(select)){
            sel <- c(select, which(names(te) %in% c("x","y")))
            mat[k,1] <- msum - cmasum(as.matrix(te_akt[,sel]))
          }
        }
        mat[k,2] <- i
        mat[k,3] <- j
        k <- k+1
      }
    }
    max <- max(mat[,1])
    # max. improvement und change of variables
    if(max<=0) {
      ret <- list(sdf=te,data=y)
      class(ret) <- "sdfdata"
      return(ret)
      break
    }
    if(!is.null(maxt)){
      if(difftime(Sys.time(),begin,units="secs")>maxt){
        ret <- list(sdf=te,data=y)
        class(ret) <- "sdfdata"
        return(ret)
      }
    }
      i <- mat[order(mat[,1],decreasing=TRUE)[1],2]
      j <- mat[order(mat[,1],decreasing=TRUE)[1],3]
      
      xy <- variableflip(as.matrix(te),i,j)
      te_best <- as.data.frame(cbind(te[,1:(ncols-2)],x=xy[,1],y=xy[,2]))
      te_best <- te_best[order(te_best$y),]
      te_best <- te_best[order(te_best$x),]
      
      te <- te_best
      ord <- 1:p
      ord[i] <- j
      ord[j] <- i
      y <- y[,ord]
      d <- d + 1
      if(printmsum) print(msum)
  }
}