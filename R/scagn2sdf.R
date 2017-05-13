### converts scagnostics to a list of class "sdfdata"
scag2sdf <- function(data,scagfun.list,arguments.list=NULL){
  data <- data[,sapply(data,is.numeric)]
  p <- ncol(data)
  n <- choose(p,2)
  x <- vector(length=n)
  y <- vector(length=n)
  sm <- matrix(ncol=length(scagfun.list)+2,nrow=n)
  name <- vector(length=length(scagfun.list))
  for (s in 1:length(scagfun.list)){
  k <- 1
  name[s] <- names(scagfun.list)[[s]]
  for (i in 1:(p-1)){
    for (j in (i+1):p){
      if(is.null(arguments.list[[s]])) sm[k,s] <- scagfun.list[[s]](data[,i],data[,j])
      else {
        l <- length(arguments.list[[s]])
        arg_cur <- arguments.list[[s]]
        arg_cur[[l+1]] <- data[,i]
        arg_cur[[l+2]] <- data[,j]
        sm[k,s] <- do.call(scagfun.list[[s]],arg_cur)
      }
      if(s==1){
       sm[k,length(scagfun.list)+1] <- i
       sm[k,length(scagfun.list)+2] <- j
      }
      k <- k+1
    }
  }
  }
  name <- c(name,c("x","y"))
  sdf <- as.data.frame(sm)
  names(sdf) <- name
  ret <- list(sdf=sdf,data=data)
  class(ret) <- "sdfdata"
  return(ret)
}