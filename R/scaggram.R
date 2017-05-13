### scaggram
scaggram <- function(sdfdata,select=1,add=FALSE,label=FALSE,order=FALSE){
  if(class(sdfdata)!="sdfdata") stop("sdfdata needs to be of class sdfdata. Please use the functions sdf and scagn2sdf.")
  x_sdf <- sdfdata[[1]]
  x <- sdfdata[[2]]
  if(sum(names(x_sdf)=="status")==1) sdf <- x_sdf[,-which(names(x_sdf)=="status")]
  else (sdf <- x_sdf)
  par(mar=c(1,1,1,1))
  if(sum(colnames(sdf) %in% c("x","y"))<2) stop("'x' and/ore 'y' is missing")
  if(is.character(select)) {selection <- which(colnames(sdf) %in% select)}
  else{selection <- select}
  if(max(selection)>=ncol(sdf)-1) stop("Selected variables are no scagnostics")
  
  if(order %in% c(2,"quicksort")){
    y <- sdf_quicksort(sdfdata)
    sdf <- y[[1]]
    x <- y[[2]]
  }  
  
  if(order %in% c(3,"All scagnostics")){
    y <- sdf_sort(sdfdata)
    sdf <- y[[1]]
    x <- y[[2]]
  }
 
  if(order %in% c(4,"Selected scagnostics")){
    sel <- c(selection,which(names(x_sdf) %in% c("x","y")))
    sdfdata <- list(sdf=x_sdf[,sel],data=x)
    class(sdfdata) <- "sdfdata"
    y <- sdf_sort(sdfdata)
    sdf <- y[[1]]
    x <- y[[2]]
    sa <- as.data.frame(sdf)
  }

  if(!order %in% c(4,"Selected scagnostics")){
    sa <- as.data.frame(sdf[,selection])
    sa <- cbind(sa,sdf[,c("x","y")])
  }
  
  sdf <- as.data.frame(sdf)
  sdf <- sdf[order(sdf$y),]
  sdf <- sdf[order(sdf$x),]
  co1 <- as.data.frame(sa)
  co1 <- co1[order(co1$y),]
  co1 <- co1[order(co1$x),]
  
  if(length(selection)==1) names(sa)[1] <- names(x_sdf)[selection]
  if(length(selection)==3) co <- co1[,1:3]
  else if(length(selection)==2){
    co <- co1[order(co1$x),1:2]
    co <- cbind(co,0)
  }
  else if(length(selection)==1){
    co <- co1[order(co1$x),1]
    co <- cbind(co,0)
    co <- cbind(co,0)
  }
  else(stop("Choose one, two or three scagnostics"))
  p <- ncol(x)
  lab <- p/40
  plot(c(0, p), c(0,-p-lab), type= "n", xaxt='n', yaxt='n', ann=FALSE,bty="n")
  k <- 1
  for(i in 0:(p-1)){
    rect(i,-i,i+1,-(i+1),border="black")
    if(label==TRUE) text(x=i+0.5,y=-i-0.5,label=names(x)[i+1],cex=0.7)
    for(j in (i+1):(p-1)){
      if (k <= choose(p,2)){
        rect(i,-j,i+1,-(j+1),col=rgb(co[k,1],co[k,2],co[k,3],alpha=max(co[k,])))
        if(add=="glyphs"){
          rect(j,-i,(j+1),-(i+1),col=rgb(co[k,1],co[k,2],co[k,3],alpha=min(max(co[k,]),0.2)))
          stars(sdf[,1:((ncol(sdf)-2))],locations=cbind(sdf$y  - 0.5,-sdf$x + 0.5),
                scale=TRUE,labels="",len=0.5,add=TRUE,lwd=1.25)
        } 
        else if(add=="splom"){
          rect(j,-i,(j+1),-(i+1),col=rgb(co[k,1],co[k,2],co[k,3],alpha=0.2))
          x1 <- (x[,j+1] - min(x[,j+1]))/(max(x[,j+1])- min(x[,j+1]))
          x2 <- (x1+0.05)/(1.1*max(x1)-min(x1))
          vx <- x2 + j
          y1 <- (x[,i+1] -min(x[,i+1]))/(max(x[,i+1])-min(x[,i+1])) 
          y2 <- (y1+0.05)/(1.1*max(y1)-min(y1))
          vy <- y2 - (i+1)
          points(vx,vy,pch=19,cex=0.7)
        }
        else rect(j,-i,(j+1),-(i+1),col=rgb(co[k,1],co[k,2],co[k,3],alpha=max(co[k,])))
        k <- k+1
      }
    }
  }
points(0.5 ,-p-lab-p/100,col=rgb(1,0,0),pch=15,cex=2)
text(0.5+p/100,-p-lab-p/100,colnames(sa)[1],pos=4)
if(length(selection)>1){
points(p/4,-p-lab-p/100,col=rgb(0,1,0),pch=15,cex=2)
text(p/4+p/100,-p-lab-p/100,colnames(sa)[2],pos=4)
}
if(length(selection)>2){
points(p/4 +(p/4-0.5),-p-lab-p/100,col=rgb(0,0,1),pch=15,cex=2)
text(p/4+(p/4-0.5)+p/100,-p-lab-p/100,colnames(sa)[3],pos=4)
}
}
