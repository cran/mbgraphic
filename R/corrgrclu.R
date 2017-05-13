corrgrclu <- function(data,var.names=FALSE,cmin=0,cmax=1,nclu=1){
  par(mar=c(1,1,1,1))
  data <- data[,sapply(data,is.numeric)]
  p <- ncol(data)
  co <- cor(data,use="pairwise.complete.obs")
    distx <- 1-as.dist(co)
    hc <- hclust(distx,method="average")
    al <- seriate(distx,method="OLO",control=list(hclust=hc))
    ord <- get_order(al)
    data <- data[, ord]
    co <- cor(data,use="pairwise.complete.obs")
  plot(c(0, p), c(0,- p), type= "n", xaxt='n', yaxt='n', ann=FALSE,,bty="n")
  k <- 1
  for(i in 0:(p-1)){
    rect(i,-i,i+1,-(i+1),border="darkgrey",col="grey",density=50)
    if(var.names==TRUE) text(x=i+0.5,y=-i-0.5,label=names(data)[i+1],cex=0.7)
    for(j in (i+1):(p-1)){
      if (k <= choose(p,2)){
        col <- co[i+1,j+1]
        if(col<0){
          if(-col >= cmin & -col <= cmax){
            rect(i,-j,i+1,-(j+1),col=rgb(1,0,0,alpha=-col),border="darkgrey")
            rect(j,-i,(j+1),-(i+1),col=rgb(1,0,0,alpha=-col),border="darkgrey")
          }
          else {
            rect(i,-j,i+1,-(j+1),col=rgb(1,0,0,alpha=0),border="darkgrey")
            rect(j,-i,(j+1),-(i+1),col=rgb(1,0,0,alpha=0),border="darkgrey")
          }
        }
        else{ 
          if(col >= cmin & col <= cmax){
            rect(i,-j,i+1,-(j+1),col=rgb(0,0,1,col),border="darkgrey")
            rect(j,-i,(j+1),-(i+1),col=rgb(0,0,1,alpha=col),border="darkgrey")
          }
          else{
            rect(i,-j,i+1,-(j+1),col=rgb(1,0,0,alpha=0),border="darkgrey")
            rect(j,-i,(j+1),-(i+1),col=rgb(1,0,0,alpha=0),border="darkgrey")
          }
        }
        k <- k+1
      }
    }
  }
  if(nclu!=1){
    clines <- cutcluster(data,c=nclu)
    for(i in 1:(length(clines)+1)){
      #abline(v=clines[i],h=-clines[i],col="darkgreen",lwd=2)
      lines(x=c(clines[i],clines[i]),y=c(1,-p-1),col="darkgreen",lwd=2)
      lines(y=c(-clines[i],-clines[i]),x=c(-1,p+1),col="darkgreen",lwd=2)
      if(i==1) rect(0,0,clines[i],-clines[i],border="darkgreen",lwd=5)
      else if(i==(length(clines)+1)) rect(clines[i-1],-clines[i-1],ncol(data),-ncol(data),border="darkgreen",lwd=5)
      else {
        rect(clines[i-1],-clines[i-1],clines[i],-clines[i],border="darkgreen",lwd=5)
      }
    }
  }
}

