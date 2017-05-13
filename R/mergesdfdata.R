### merges two lists of class "sdfdata"
mergesdfdata <- function(sdfdata1,sdfdata2){
  if(all(dim(sdfdata1$data)!=dim(sdfdata2$data))) stop("sdfdata1 and sdfdata2 don't include the same data frames")
  p <- which(names(sdfdata1$sdf)=="x") - 1
  sdf1 <- sdfdata1$sdf[,1:p]
  sdfcom <- cbind(sdf1,sdfdata2$sdf)
  if(sum(names(sdfdata1$sdf)=="status")==1 & sum(names(sdfdata2$sdf)=="status")!=1) sdfcom$status <- sdfdata1$sdf$status
  ret <-  list(sdf=sdfcom,data=sdfdata1$data)
  class(ret) <- "sdfdata"
  return(ret)
}