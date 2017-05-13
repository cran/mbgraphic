corrgr <- function(data,var.names=FALSE,cmin=0,cmax=1){
  par(mar=c(1,1,1,1))
  p <- ncol(data)
  co <- cor(data,use="pairwise.complete.obs")
  plot(c(0, p), c(0,- p), type= "n", xaxt='n', yaxt='n', ann=FALSE)
  k <- 1
  for(i in 0:(p-1)){
    rect(i,-i,i+1,-(i+1),border="darkgrey",col="darkgrey",density=30)
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
}

