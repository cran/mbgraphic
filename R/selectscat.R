### selects a scatterplot matrix with "high relevance"
selectscat <- function(data,relmat=NULL,k=5,r=k,plot=TRUE,criteria="maxm"){
  if(class(data)=="sdfdata")
  x <- data$data
  else(x <- data)
  if (!is.data.frame(x) && !is.matrix(x))
    stop("supply  'x' must be a data frame or a numeric matrix")
  if(is.data.frame(x)) x <- x[,sapply(x,is.numeric)]
  
  p <- dim(x)[2]
  if(criteria=="cor"){
    mat <- cor(x)
  }
  if(criteria=="maxm"){
    if(is.null(relmat)){
      if(!class(data)=="sdfdata"){
        x_sdf <- sdf(x)
        x_sdf <- x_sdf$sdf[,c(1,3,4,5,6,8,10,11)]
      }
      else(x_sdf <- data$sdf)
      mat <- relm(x_sdf,matrix=TRUE)
    }
    else{
      mat <- relmat
    } 
  }
  
  ord <- get_order(seriate(as.dist(1-mat),method="OLO",control=list(method="average")))

  corsum <- rep(0,p) 
  variables <- matrix(nrow=k,ncol=length(corsum))
  
 for(i in 1:(p-r+1)){
    vars <- ord[i:(r+i-1)]
    matc <- combn(vars,k)
    corsum_all <- rep(0,dim(matc)[2])
    for(j in 1:dim(matc)[2]){
      corsum_all[j] <- sum(mat[matc[,j],matc[,j]])
    }
    max <- order(corsum_all,decreasing=TRUE)[1]
    variables[,i]<- matc[,max]
    corsum[i] <- corsum_all[max]
  }
  
  max <- order(corsum,decreasing=TRUE)[1]
  sel <- variables[,max]
  selection <- sort(sel)
  
  if(plot==TRUE){
    ggpairs(x[,selection],upper="blank",diag=list(continuous="densityDiag"), axisLabels='none')  
  }
  else return(names(x)[selection])
}
