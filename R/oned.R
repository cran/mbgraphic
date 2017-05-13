#### one-dimensional measures

### discrete1d
discrete1d <- function(x){
  if(is.vector(x) || is.factor(x)){
    if(!is.numeric(x)) stop("x is not numeric")
    else  (eindx <- 1 - length(unique(x[!is.na(x)]))/sum(!is.na(x)))
  }
  if (is.data.frame(x) | is.matrix(x)){
    eindx <- vector(length=dim(x)[2])    
    for (i in 1:dim(x)[2]){
     if(is.numeric(x[,i])) eindx[i] <- 1 - length(unique(x[!is.na(x[,i]),i]))/sum(!is.na(x[,i]))
     else (eindx[i] <- NA)
    }
  }
  return(eindx)
}

### skew1d
skew1d <- function(x){
  if(is.vector(x) || is.factor(x)){
    if(!is.numeric(x)) stop("x is not numeric")
    else (
    eindx <- 2*abs(0.5 - (quantile(x,0.9,na.rm=T)-quantile(x,0.5,na.rm=T))/
                   (quantile(x,0.9,na.rm=T)-quantile(x,0.1,na.rm=T)))
    )
  }
    if (is.data.frame(x) | is.matrix(x)){
    eindx <- vector(length=dim(x)[2])    
    for (i in 1:dim(x)[2]){
      if(is.numeric(x[,i])) eindx[i] <- 2*abs(0.5 - (quantile(x[,i],0.9,na.rm=T)-quantile(x[,i],0.5,na.rm=T))/
                                    (quantile(x[,i],0.9,na.rm=T)-quantile(x[,i],0.1,na.rm=T)))
      else eindx[i] <- NA
    }
  }
  return(eindx)
}

### multimod1d
multimod1d <- function(x,seed=NULL,exp=1){
	if(is.vector(x) || is.factor(x)){
		if(!is.numeric(x)) stop("x is not numeric")
		else {
			  sdx <- discrete1d(x)*sd(x,na.rm=T)/5
			  x <- x + rnorm(length(x),0,sdx)
			  eindx <- 1 - dip.test(x)$p.value
		}
	}
  if (is.data.frame(x) | is.matrix(x)){
		  eindx <- vector(length=dim(x)[2])    
		  for (i in 1:dim(x)[2]){
		    if(is.numeric(x[,i])){ 
		          sdx <- discrete1d(x[,i])*sd(x[,i],na.rm=T)/5
		          #sample(1:100000000,1)
		          if(is.null(seed)) set.seed(9042369)
		          else(set.seed(seed))
		          x[,i] <- x[,i] + rnorm(length(x[,i]),0,sdx)
              eindx[i] <- 1 - dip.test(x[,i])$p.value
		    }
        else eindx[i] <- NA
		  }
		}
	return(eindx^exp)
	}