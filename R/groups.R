# Calculates measure 'groups' for all numeric variables of a data.frame
groups <- function(data,classvar){
  x <- data
  if(is.character(classvar)) classvar <- which(names(x)==classvar)
  c <- x[,classvar]
  name <- names(x)[classvar]
  if(!is.factor(c)) stop("'x' needs to be a factor variable")
  if(is.vector(x) && is.numeric(x)){
  m <- lm(x~c)
  return(1-var(m$residuals)/var(x))
  }
  if(is.data.frame(x)){
    x <- x[,sapply(x,is.numeric)]
    r <- vector(length=ncol(x))
    for(i in 1:length(r)){
      m <- lm(x[,i]~c)
      r[i] <- 1-var(m$residuals)/var(x[,i])
    }
    return(data.frame(groups=r,variable=names(x)))
  }
}

### Plot function
groups_maxplot <- function(data,classvar,m=5,samebinsize=FALSE){
  x <- data
  if(is.character(classvar)) classvar <- which(names(x)==classvar)
  c <- x[,classvar]
  name <- names(x)[classvar]
  group <- groups(x,classvar)
  x <-  x[,sapply(x,is.numeric)]
  m <- min(m,ncol(x))
  go <- group[order(group[,1],decreasing = TRUE),]
  xo <- x[,order(group[,1],decreasing = TRUE)]
  # see https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
  grid_arrange_shared_legend <- function(..., nrow = 1, ncol = length(list(...)), position = c("bottom", "right")) {
    plots <- list(...)
    position <- match.arg(position)
    g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
    legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
    lheight <- sum(legend$height)
    lwidth <- sum(legend$width)
    gl <- lapply(plots, function(x) x + theme(legend.position = "none"))
    gl <- c(gl, nrow = nrow, ncol = ncol)
  
    combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
    grid.newpage()
    grid.draw(combined)
  }
  p <- list()
  for (i in 1:m) {
    df <- data.frame(xo[,i],c)
    df[,2] <- as.factor(df[,2])
    names(df) <- c(names(xo)[i],name)
    if(samebinsize==TRUE) plot <- ggplot(df, aes_string(x=names(df)[1],fill=names(df)[2])) + geom_histogram(position="fill") 
    else(plot <- ggplot(df, aes_string(x=names(df)[1],fill=names(df)[2])) + geom_histogram())
    p[[i]] <- plot + ylab("") +
      scale_fill_manual(values=c("orange","green3","grey30","darkred","steelblue","purple3","turquoise2")) +
      theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())
  }
  if(m==1)
    grid_arrange_shared_legend(p[[1]],nrow = 1, ncol = 1)
  if(m==2)
    grid_arrange_shared_legend(p[[1]],p[[2]],nrow = 1, ncol = 2)
  if(m==3)
    grid_arrange_shared_legend(p[[1]],p[[2]],p[[3]],nrow = 1, ncol = 3)
  if(m==4)
    grid_arrange_shared_legend(p[[1]],p[[2]],p[[3]],p[[4]],nrow = 1, ncol = 4)
  if(m==5)
    grid_arrange_shared_legend(p[[1]],p[[2]],p[[3]],p[[4]],p[[5]],nrow = 1, ncol = 5)
  if(m==6)
    grid_arrange_shared_legend(p[[1]],p[[2]],p[[3]],p[[4]],p[[5]],p[[6]],nrow = 2, ncol = 3)
  if(m==7)
    grid_arrange_shared_legend(p[[1]],p[[2]],p[[3]],p[[4]],p[[5]],p[[6]],p[[7]],nrow = 2, ncol = 4)
  if(m==8)
    grid_arrange_shared_legend(p[[1]],p[[2]],p[[3]],p[[4]],p[[5]],p[[6]],p[[7]],p[[8]],nrow = 2, ncol = 4)
  if(m==9)
    grid_arrange_shared_legend(p[[1]],p[[2]],p[[3]],p[[4]],p[[5]],p[[6]],p[[7]],p[[8]],p[[9]],nrow = 2, ncol = 5)
  if(m==10)
  grid_arrange_shared_legend(p[[1]],p[[2]],p[[3]],p[[4]],p[[5]],p[[6]],p[[7]],p[[8]],p[[9]],p[[10]],nrow = 2, ncol = 5)
}
