### one-dimensional classification 
binning1d <- function (x, b = 50, bin ="equi",anchor="min") 
{
  if (bin %in% c(TRUE,"equi")) {
    if (anchor %in% c("min","nice","ggplot")) rx <- range(x, na.rm = TRUE)
    else  {
      rx <- c(anchor,max(x)) 
      anchor ="ind"
    }  
      if (anchor =="nice"){
      anc <- niceround(min(x), down=T)
      nim <- niceround(max(x), up=T)
      rx <- c(anc,nim)
      bw <- (diff(rx) +  1e-12)/b
      xbr <- seq(rx[1],rx[2],bw)
    } 
    if (anchor == "ggplot"){
      bw <- (diff(rx) +  1e-12)/b
      xbr <- fullseq(rx,bw)
    }
    if(anchor %in% c("min","ind")) {
      xbr <- seq(rx[1], rx[2], (diff(rx) +  1e-12)/b)
      xbr[length(xbr)] <- xbr[length(xbr)] + 1000
    }
  }
  if (bin %in% c("q", "quant")) {
    xbr <- unique(quantile(x, seq(0, 1, 1/b)))
    xbr[length(xbr)] <- xbr[length(xbr)] + 1000
  }
  bin <- .bincode(x, breaks = xbr, include.lowest = TRUE)
  return(bin)
}

### list of bins
binning.df <- function(data,b=10,bin="equi", anchor="min"){
  return(apply(data,2,binning1d,b=b,bin=bin,anchor=anchor))
}

### binning of a variable or a data frame 
binning <- function(x,y=NULL,b=20,bin="equi",anchor="min"){
  if (is.data.frame(x)) 
    data <- x[,sapply(x,is.numeric)]
  if(is.vector(x) && is.vector(y)){
    data <- as.data.frame(cbind(x,y))
    if (length(x) != length(y)) {
      stop("Vector lengths differ.")
    }
  }
  if (!is.data.frame(x) && is.null(y)) 
    data <- as.data.frame(x)
  
  d <- dim(data)[2]
  bxyz <- matrix(ncol=d,nrow=dim(data)[1])
  bindd <- vector(length=dim(data)[1])
  multi <- round
    
  for (i in 1:d){
    bxyz[,i] <- binning1d(data[,i],b,bin=bin)
  }
  
  bindd <- apply(bxyz, 1, paste, collapse="x")
  xyz <- data.frame(cbind(data,"bindd" <- bindd))
  counts <- table(bindd)
  mb <- matrix(ncol=d,nrow=length(counts))
  for (i in 1: d){
    mb[,i] <- tapply(xyz[,i],xyz[,d+1],mean)
  }
  ret <- data.frame(cbind(mb,as.vector(counts)))
  names(ret) <- c(names(data),"counts")
  return(ret)
}

### plot function 
binningplot <- function(x,y,b=10,bin="equi",anchor="min")
{
  if (length(x) != length(y)) {
    stop("Vector lengths differ.")
  }
  if (bin %in% c("e","equi")){
    bx <- binning1d(x,b,bin="equi",anchor=anchor)
    by <- binning1d(y,b,bin="equi",anchor=anchor)
    if (anchor %in% c("min","ggplot")){
      rx <- range(x, na.rm = TRUE)
      ry <- range(y, na.rm = TRUE)
    } 
    else {
      ancx <- niceround(min(x), down=T)
      nimx <- niceround(max(x), up=T)
      rx <- c(ancx,nimx)
      ancy <- niceround(min(y), down=T)
      nimy <- niceround(max(y), up=T)
      ry <- c(ancy,nimy)
    }
    if (anchor %in% c("min","nice")){
      xbr <- seq(rx[1], rx[2], (diff(rx) +  1e-12)/b)
      ybr <- seq(ry[1], ry[2], (diff(ry) +  1e-12)/b)
    }
    else {
      xbr <- fullseq(rx, (diff(rx) +  1e-12)/b)
      ybr <- fullseq(rx, (diff(ry) +  1e-12)/b)
    }
  }
  if (bin %in% c("q", "quant")) {
    bx <- binning1d(x,b,bin="quant")
    by <- binning1d(y,b,bin="quant")
    xbr <- unique(quantile(x, seq(0, 1, 1/b)))
    ybr <- unique(quantile(y, seq(0, 1, 1/b)))
 
  }
  lx <- length(xbr) - 1
  ly <- length(ybr) - 1
    
    num <- data.frame(bx=bx,by=by)
    num  <- num[with(num,order(by,bx)),]
    ta <- table(num)
  
    if (dim(ta)[1] != lx){
      na <- as.numeric(dimnames(ta)$bx)
      be <- vector(length=b)
      addx <- data.frame()
      for (i in 2:b){
        if(any(na==i)==FALSE){
          be[i] <- i
          bx <- rep(i,b)
          by <- 1:b
          Freq <- rep(0,b)
          add1 <- data.frame(bx,by,Freq)       
          addx <- rbind(addx,add1)        
        }       
      }      
    }
    
    if (dim(ta)[2] != ly){
      na <- as.numeric(dimnames(ta)$by)
      be <- vector(length=b)
      addy <- data.frame()
      for (i in 2:b){
        if(any(na==i)==FALSE){
          be[i] <- i
          by <- rep(i,b)
          bx <- 1:b
          Freq <- rep(0,b)
          add1 <- data.frame(bx,by,Freq)       
          addy <- rbind(addy,add1)        
        }       
      }      
    }
    
    if (dim(ta)[1] != lx && dim(ta)[2] != ly){
      add <- rbind(addx,addy)
      add <- add[!duplicated(add),]
    }
    if (dim(ta)[1] != lx && dim(ta)[2] == ly) add <- addx
    if (dim(ta)[1] == lx && dim(ta)[2] != ly) add <- addy
    
    
    ta.df <- as.data.frame(ta)
    
    if (dim(ta)[1] != lx || dim(ta)[2] != ly){
    add$bx <- as.factor(add$bx)
    add$by <- as.factor(add$by)
    ta.df <- rbind(ta.df,add)
    ta.df$bx <- as.numeric(as.character(ta.df$bx))
    ta.df$by <- as.numeric(as.character(ta.df$by))
    ta.df <- ta.df[with(ta.df,order(by,bx)),]
    }
    
    counts <- ta.df[,3]
    counts <- rep(counts,each=4)
    
    xe <- vector()
    ye <- vector()
    te <- vector()
    for (i in 1: (length(ybr)-1)){
      x1 <- vector()
      y <- rep(c(rep(ybr[i],2),rep(ybr[i+1],2)),length(xbr)-1)
      for (j in 1: (length(xbr)-1)){
        x <- c(xbr[j],rep(xbr[j+1],2),xbr[j])
        x1 <- c(x1,x)
      }
      xe <- c(xe,x1)
      ye <- c(ye,y)  
    }
    
    te <- rep(1:(lx*ly),each=4)
    df <- data.frame(xe=xe,ye=ye,te=as.character(te),counts=counts)
    if(sum(df$count==0)>0) df <- df[-which(df$count==0),]
      
    out <- ggplot() + geom_polygon(data=df,mapping=aes(xe,ye,group=te,fill=counts)) + xlab("x") + ylab("y")
    out <- out + scale_fill_gradient(high="dodgerblue4", low="paleturquoise3")  
    return(out)
}