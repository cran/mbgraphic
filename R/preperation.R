### data type
datatype <- function(x,limit=0.8){
  if(is.vector(x) || is.factor(x)){
    numeric <- is.numeric(x)
    if (numeric){ 
      valdisc <- discrete1d(x)
      if (valdisc > limit) discrete <- TRUE
      else (discrete <- FALSE)
    }
      else {discrete <- NA 
            valdisc <- NA}
  }
  if (is.data.frame(x)){
    numeric <- vector(length=dim(x)[2]) 
    discrete <- vector(length=dim(x)[2]) 
    valdisc <- vector(length=dim(x)[2]) 
    for (i in 1:dim(x)[2]){
      numeric[i] <- is.numeric(x[,i])
      if (numeric[i]){ 
        valdisc[i] <- discrete1d(x[,i])
        if (valdisc[i] > limit) discrete[i] <- TRUE
      }
      else {discrete[i] <- NA
            valdisc[i] <- NA}
    }
  }
    if (!is.data.frame(x) && is.matrix(x)){   
      discrete <- vector(length=dim(x)[2]) 
      valdisc <- vector(length=dim(x)[2]) 
      numeric <- rep(TRUE,dim(x)[2])
      for (i in 1:dim(x)[2]){
          valdisc[i] <- discrete1d(x[,i])
          if (valdisc[i] > limit) discrete[i] <- TRUE
        }
  }
  ret <- data.frame("numeric"=numeric,"discrete"=discrete,"valdisc"=valdisc)
  return(ret)
}

### classification outliers/no outlier based on tukey definition  
outliertuk <- function(x) {
  x <- x[,sapply(x,is.numeric)]
  for(i in 1:ncol(x)){
    krit <- x[,i] < quantile(x[,i],0.25) - 1.5*IQR(x[,i]) | x[,i] > quantile(x[,i],0.75) + 1.5*IQR(x[,i])
    x[,i] <- as.factor(ifelse(krit,"outl","not"))
  }
  return(x)
}

### outlier measure (tukey) 
outlier1dv <- function(x){
  if(is.vector(x) || is.factor(x)){
    if(!is.numeric(x)) stop("x is not numeric")
    else {
      value <- ifelse(x > quantile(x,0.5),
                      (x - quantile(x,0.5))/(quantile(x,0.75) + 3*IQR(x) - quantile(x,0.5)),
                      - (x - quantile(x,0.5))/(quantile(x,0.25) - 3*IQR(x) - quantile(x,0.5)))
      xs1 <- ifelse(value>1,1,value)
      xs2 <- ifelse(xs1<=-1,-1,xs1)
      return(xs2) 
    }
  }  
  if (is.data.frame(x)){
    x <- x[,sapply(x,is.numeric)]
    for (i in 1:dim(x)[2]){
      value <- ifelse(x[,i]> quantile(x[,i],0.5),
                      (x[,i] - quantile(x[,i],0.5))/(quantile(x[,i],0.75) + 3*IQR(x[,i]) - quantile(x[,i],0.5)),
                      - (x[,i] - quantile(x[,i],0.5))/(quantile(x[,i],0.25) - 3*IQR(x[,i]) - quantile(x[,i],0.5)))
      xs1 <- ifelse(value>1,1,value)
      x[,i] <- ifelse(xs1<= -1,-1,xs1)
    }
    return(x)
  }
}
