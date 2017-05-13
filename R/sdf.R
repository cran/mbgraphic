# calculates scagnostics from package scagnostics and transforms them to a list of class "sdfdata"
sdf <- function(data,selection="All"){
  if(!is.data.frame(data)) stop("data needs to be a  data frame")
  x <- data
  x <- x[,sapply(x,is.numeric)]
  s <- scagnostics(x)
  # use '1-Convex' instead of 'Convex'
  s[6,] <- 1-s[6,]
  rownames(s)[6] <- "1-Convex"
  
  out <- scagnosticsOutliers(s)
  ex <- scagnosticsExemplars(s)
  g <- scagnosticsGrid(s)
  if(length(selection)>1){
    if(is.character(selection)) select <- which(rownames(s) %in% selection)
    else(select=selection)
    s <- s[select,]
  }
  sm <- t(unclass(s))
  te <- cbind(sm,g)
  status <- out + 2*ex
  
  if(length(table(status))==3) te$status <- factor(status,labels=c("non","outlier","exemplar"))
  if(length(table(status))==2){ 
    if(names(table(status))[2]=="1") te$status <- factor(status,labels=c("non","outlier"))
    else te$status <- factor(status,labels=c("non","exemplar"))
  }
  if(length(table(status))==1) te$status <- factor(status,labels=c("exemplar"))
  tesort <- te[order(te$y),]
  tesort <- te[order(te$x),]
  ret <- list(sdf=tesort,data=x)
  class(ret) <- "sdfdata"
  return(ret)
}
  

