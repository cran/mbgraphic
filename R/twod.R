### distance correlation 
dcor2d_xy <- function(x,y,bx=NULL,by=NULL,binning=FALSE,b=50,anchor="min",parallel=FALSE){
  if(binning %in% c(TRUE,"equi","quant")){
    bin2d <- data.frame(x=x,y=y) %>% mutate(bx,by)
    xyw <- bin2d  %>%   group_by(bx,by) %>% summarize(mx=mean(x),my=mean(y),we=length(x))
    measure <- with(xyw, wdcor(mx, my, we)) 
  }
  if(binning=="hexb"){
    bin <- hexbin(x,y,xbins=b)
    mx <- bin@xcm
    my <- bin@ycm
    we <- bin@count
    xyw <- data.frame(mx=mx,my=my,we=we)
    measure <- with(xyw, wdcor(mx, my, we)) 
  }
  if(binning==FALSE){
    measure <- wdcor(x,y)
  }
  return(measure)
}
  
dcor2d <- function(x,y=NULL,binning=FALSE,b=50,anchor="min",parallel=FALSE){
  if(is.vector(x) && is.vector(y))
    return(dcor2d_xy(x,y))
  else{
    if(is.data.frame(x)) data<- x[,sapply(x,is.numeric)]
    if(!is.data.frame(x) && is.matrix(x)) data<- x
    if(!is.matrix(x) && !is.data.frame(x) && is.null(y)) stop("supply both 'x' and 'y' or a matrix-like 'x'")
    
    if(binning %in% c(TRUE,"equi","quant")){
      bdata <- binning.df(data,b=b,bin=binning,anchor=anchor)
    }  
    if(!(binning %in% c(TRUE,FALSE,"equi","quant","hexb")))   stop("unknown binning method")
    
    vcom <- combn(ncol(data),2)
    x1 <- vcom[1,]
    x2 <- vcom[2,]
    
    measureij <- function(i,j,data) {dcor2d_xy(data[,i],data[,j],bdata[,i],bdata[,j],binning=binning,b=b,anchor=anchor)}
    if(parallel==FALSE) vecfun <- Vectorize(measureij, vectorize.args=list("i","j"))
    else (vecfun <- VectorizeP(measureij, vectorize.args=list("i","j")))
    measure <- vecfun(x1,x2,data)
    if(is.data.frame(x)){
      nx1=names(data)[x1] 
      nx2=names(data)[x2]
      return(data.frame(dcor2d=measure,x1=x1, x2=x2,nx1=names(data)[x1],nx2=names(data)[x2])) 
    }
    if(!is.data.frame(x) && is.matrix(x))
      return(data.frame(dcor2d=measure,x1=x1, x2=x2)) 
  }
}

### splines2d 
splines2d_xy <- function(x,y,bx=NULL, by=NULL,binning=FALSE,b=50,anchor="min"){
  if(binning %in% c(TRUE,"equi","quant")){
    bin2d <- data.frame(x=x,y=y) %>% mutate(bx,by)
    xyw <- bin2d  %>%   group_by(bx,by) %>% summarize(mx=mean(x),my=mean(y),we=length(x))
    kx <- with(xyw,ifelse(length(unique(mx))<20,3,10))
    ky <- with(xyw,ifelse(length(unique(my))<20,3,10))
    mgam1 <- with(xyw, gam(my ~ s(mx,bs="cr",k=kx),weights=we))
    mgam2 <- with(xyw, gam(mx ~ s(my,bs="cr", k=ky),weights=we))
    measure <- with(xyw, max(1-wvar(predict.gam(mgam1) - my,we)/wvar(my,we), 1-wvar(predict.gam(mgam2) - mx,we)/wvar(mx,we)))
    with(xyw, max(1-wvar(predict.gam(mgam1) - mx,we)/wvar(mx,we), 1-wvar(predict.gam(mgam2) - my,we)/wvar(my,we)))
    ifelse(with(xyw,1-wvar(predict.gam(mgam1) - my,we)/wvar(my,we) >= 1-wvar(predict.gam(mgam2) - mx,we)/wvar(mx,we)), tarvar <- 1,tarvar <- 2)
  }
  
  if(binning=="hexb"){
    bin <- hexbin(x,y,xbins=b)
    mx <- bin@xcm
    my <- bin@ycm
    we <- bin@count
    xyw <- data.frame(mx=mx,my=my,we=we)
    kx <- with(xyw,ifelse(length(unique(mx))<20,3,10))
    ky <- with(xyw,ifelse(length(unique(my))<20,3,10))
    mgam1 <- with(xyw, gam(my ~ s(mx,bs="cr",k=kx),weights=we))
    mgam2 <- with(xyw, gam(mx ~ s(my,bs="cr", k=ky),weights=we))
    measure <- with(xyw, max(1-wvar(predict.gam(mgam1) - my,we)/wvar(my,we), 1-wvar(predict.gam(mgam2) - mx,we)/wvar(mx,we)))
    with(xyw, max(1-wvar(predict.gam(mgam1) - mx,we)/wvar(mx,we), 1-wvar(predict.gam(mgam2) - my,we)/wvar(my,we)))
    ifelse(with(xyw,1-wvar(predict.gam(mgam1) - my,we)/wvar(my,we) >= 1-wvar(predict.gam(mgam2) - mx,we)/wvar(mx,we)), tarvar <- 1,tarvar <- 2)
  }
  
  if(binning==FALSE) {
        kx <- ifelse(length(unique(x[!is.na(x)]))<20,3,10)
        ky <- ifelse(length(unique(y[!is.na(y)]))<20,3,10)
        mgam1 <- gam(y ~ s(x,bs="cr",k=kx))
        mgam2 <- gam(x ~ s(y,bs="cr",k=ky))
        measure <- max(1-var(residuals(mgam1),na.rm=T)/var(y,na.rm=T), 1-var(residuals(mgam2),na.rm=T)/var(x,na.rm=T))
        ifelse(1-var(residuals(mgam1),na.rm=T)/var(y,na.rm=T) >= 1-var(residuals(mgam2),na.rm=T)/var(x,na.rm=T), tarvar <- 1, tarvar<- 2)
  }
 return(c(measure,tarvar))
}

splines2d <- function(x,y=NULL,binning=FALSE,b=50,anchor="min",parallel=FALSE){
  if(is.vector(x) && is.vector(y))
    return(splines2d_xy(x,y)[1])
  else{
    if(is.data.frame(x)) data<- x[,sapply(x,is.numeric)]
    if(!is.data.frame(x) && is.matrix(x)) data<- x
    if(!is.matrix(x) && !is.data.frame(x) && is.null(y)) stop("supply both 'x' and 'y' or a matrix-like 'x'")
    
    if(binning %in% c(TRUE,"equi","quant")){
      bdata <- binning.df(data,b=b,bin=binning,anchor=anchor)
    }  
    if(!(binning %in% c(TRUE,FALSE,"equi","quant","hexb")))   stop("unknown binning method")
    
    vcom <- combn(ncol(data),2)
    x1 <- vcom[1,]
    x2 <- vcom[2,]
    
    measureij <- function(i,j,data) {splines2d_xy(data[,i],data[,j],bdata[,i],bdata[,j],binning=binning,b=b,anchor=anchor)}
    if(parallel==FALSE) vecfun <- Vectorize(measureij, vectorize.args=list("i","j"))
    else (vecfun <- VectorizeP(measureij, vectorize.args=list("i","j")))
    measure <- t(vecfun(x1,x2,data))
    tarvar <- ifelse(measure[,2]==1,"x1","x2")
    if(is.data.frame(x)){
      nx1=names(data)[x1] 
      nx2=names(data)[x2]
      return(data.frame(splines2d=measure,x1=x1, x2=x2,nx1=names(data)[x1],nx2=names(data)[x2])) 
    }
    if(!is.data.frame(x) && is.matrix(x))
      return(data.frame(splines2d=measure,x1=x1, x2=x2)) 
  }
}