### weighted mean
wmean <- function (x,weights) 
{
  return(sum(weights * x)/sum(weights))
}

### weighted varinance
wvar <- function (x, weights) 
{
  xmean = wmean(x, weights)
  return(1/(sum(weights)-1) * sum((x-xmean)^2*weights))
}


### nice rounding
niceround <- function(x,down=F,up=F){ 
  if (down)  d <- -0.5
  else if (up) d <- 0.5
  else d <- 0 
  log <- floor(log10(abs(x)+ 1e-24))
  rp <-vector(length=length(x))
  for (i in 1:length(x)){
    if  (log[i]<1) rp[i] <- 1
    else if (log[i]> 0 && log[i] <= 2) rp[i] <- 2
    else rp[i] <- 4
  }
  ret <- round(x*10^(-log)*rp + d)/10^(-log)/rp
  return(ret)
}

# Uses the code from base function 'Vectorize' and replaces 'mapply' by 'mcmapply'
VectorizeP <- function (FUN, vectorize.args = arg.names, SIMPLIFY = TRUE, USE.NAMES = TRUE) 
{
  arg.names <- as.list(formals(FUN))
  arg.names[["..."]] <- NULL
  arg.names <- names(arg.names)
  vectorize.args <- as.character(vectorize.args)
  if (!length(vectorize.args)) 
    return(FUN)
  if (!all(vectorize.args %in% arg.names)) 
    stop("must specify names of formal arguments for 'vectorize'")
  collisions <- arg.names %in% c("FUN", "SIMPLIFY", "USE.NAMES", 
                                 "vectorize.args")
  if (any(collisions)) 
    stop(sQuote("FUN"), " may not have argument(s) named ", 
         paste(sQuote(arg.names[collisions]), collapse = ", "))
  FUNV <- function() {
    args <- lapply(as.list(match.call())[-1L], eval, parent.frame())
    names <- if (is.null(names(args))) 
      character(length(args))
    else names(args)
    dovec <- names %in% vectorize.args
    do.call("mcmapply", c(FUN = FUN, args[dovec], MoreArgs = list(args[!dovec]), 
                          SIMPLIFY = SIMPLIFY, USE.NAMES = USE.NAMES))
  }
  formals(FUNV) <- formals(FUN)
  FUNV
}
