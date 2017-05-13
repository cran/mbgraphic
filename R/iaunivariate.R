### interactive app for discovering univariate anomalies
iaunivariate <- function(data,exp=1,seed=NULL) {
  x <- data
  xc <-  as.data.frame(x[,which(sapply(x,is.factor) + sapply(x,is.character)!=0)])
  names(xc) <- names(x)[which(sapply(x,is.factor) + sapply(x,is.character)!=0)]
  x <- x[,sapply(x,is.numeric)]
  discrete1d <- discrete1d(x)
  skew1d <- skew1d(x)
  multimod1d <- multimod1d(x,exp=exp,seed=seed)
  
  shinyApp(
    options = list(width="100%"),
    ui = fluidPage(
      fluidRow(
        column(width=3,
               sliderInput("bins", "Number of bins in histograms of criteria", 1, 100, 30),
               sliderInput("maxp","Max number of plots (plots with highest values are displayed)",1,8,4),
               if(ncol(xc)==0) selectInput("cvariables","Character Variables",c("No character variables" = "", colnames(xc)))
               else(selectInput("cvariables","Character variables",c("Choose one" = "", colnames(xc)))), 
               plotOutput("charac",click="click_cvar",brush="brush_cvar"),
               verbatimTextOutput("plot_clickinfo")
        ),
        
        column(width=3,
               plotOutput("discrete", height="auto",click="click_discrete"),
               plotOutput("choosen_discrete")
        ),
        
        column(width=3,
               plotOutput("skew", height="auto",click="click_skew"),
               plotOutput("choosen_skew")
        ),
        
        column(width=3,
               plotOutput("multimod", height="auto",click="click_multimod"),
               plotOutput("choosen_multimod")
        )
      )
    ),
    
    server = function(input, output, session) {
      
      binb <- reactive({
        n <- input$bins
        b <- 1/n
        ret <- seq(0,1,b)
        return(ret)
      })
      
      v <- reactiveValues(
        click_discete = NULL,
        click_skew=NULL,
        click_multimod=NULL,
        click_cvar = NULL    
      )
      
      observeEvent(input$click_cvar, {
        sv <- which(colnames(xc)==input$cvariables)
        tab <- table(xc[,sv])
        o <- length(tab)
        if(input$click_cvar$x>=0 & input$click_cvar$x<=o) v$click_cvar <- input$click_cvar
        else v$click_cvar <- NULL
      })
      
      observeEvent(input$click_discrete,{
        if(input$click_discrete$x>=0 & input$click_discrete$x<=1) v$click_discrete <- input$click_discrete
        else v$click_discrete <- NULL
      })
      
      observeEvent(input$click_skew,{
        if(input$click_skew$x>=0 & input$click_skew$x<=1)  v$click_skew <- input$click_skew
        else  v$click_skew<- NULL
      })
      
      observeEvent(input$click_multimod,{
        if(input$click_multimod$x>=0 & input$click_multimod$x <=1) v$click_multimod <- input$click_multimod
        else  v$click_multimod<- NULL
      })
      
      output$plot_clickinfo <- renderPrint({
        cat("Selected category \n ")
        if(is.null(v$click_cvar)) "No category selected"
        else {
          sv <- which(colnames(xc)==input$cvariables)
          classn <- floor(v$click_cvar$x)+1
          tab <- table(xc[,sv])
          names(tab)[classn]
        }
      })
      
      # plot selected character variables
      output$charac <- renderPlot({
        if(input$cvariables=="") plot(1,1,col="white",xaxt='n',yaxt='n',ylab='',xlab='',pch=19,bty="n")
        else {sv <- which(colnames(xc)==input$cvariables)
        tab <- table(xc[,sv])
        if(is.null(v$click_cvar)) barplot(tab,col='grey40', width=0.8,space=0.2,xlim=c(0,length(tab)+1),yaxt="n", axisnames=FALSE)
        else{
            xv<- v$click_cvar$x
            if (xv<0 | xv>length(tab)){
                barplot(tab,col='grey40', width=0.8,space=0.2,xlim=c(0,length(tab)+1),yaxt="n",axisnames=FALSE)
            }
            else{
                barplot(tab,col=c(rep('grey60',floor(xv)),'orange',rep('grey60',length(tab)-floor(xv))),
                        width=0.8,space=0.2,xlim=c(0,length(tab)+1),yaxt="n",axisnames=FALSE)
              }
        }
        }
      })
      
      # histogram of discrete1d
      output$discrete <- renderPlot({
        binseq <- binb()
        if(is.null(v$click_discrete)) qplot(discrete1d,breaks=binseq) + ylab("")
        else {
          xv <- v$click_discrete$x
          o <- binseq[which(binseq>xv)[1]]# position, where xv is first time above, upper boundary 
          if(o==1) o <- 1.1
          u <-  binseq[which(binseq>xv)[1]-1] # lower boundary
          col <- ifelse(discrete1d>=u & discrete1d<o,"2","1")
          if (length(table(col))==1){
            qplot(discrete1d,breaks=binseq) + ylab("")
          }
          else {
            qplot(discrete1d,breaks=binseq,fill=col) + ylab("")  + 
              scale_fill_manual(values = c('grey60', 'darkred')) +   theme(legend.position="none")
          }
        }
      }, 
      height = function() {
        session$clientData$output_discrete_width*0.8
      })
      
      # barplots of the selected variables
      output$choosen_discrete <- renderPlot({
        if(is.null(v$click_discrete)) plot(1,1,col="white",xaxt='n',yaxt='n',ylab='',xlab='',pch=19,bty="n")
        else{
          binseq <- binb()
          xv <- v$click_discrete$x
          o <- binseq[which(binseq>xv)[1]]# position, where xv is first time above, upper boundary 
          if(o==1) o <- 1.1
          u <-  binseq[which(binseq>xv)[1]-1] # lower boundary
          col <- ifelse(discrete1d>=u & discrete1d<o,"1","2")
          sel <- which(col=="1")
          maxp <- input$maxp
          if(length(sel)>maxp){
            sel <- sel[order(discrete1d[sel],decreasing = TRUE)][1:maxp]
          }
          if(is.null(v$click_cvar)){
            p <- list()
            for (i in seq_along(sel)) {
              avar <- names(x)[sel[i]]
              p[[i]] <- ggplot(x,aes_string(x=avar)) + ylab("")  + geom_bar() + 
                theme(axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks=element_blank())
            }
          }

        else{
          sv <- which(colnames(xc)==input$cvariables)
          classn <- floor(v$click_cvar$x)+1
          tab <- table(xc[,sv])
          x$co <- ifelse(xc[,sv]==names(tab)[classn],"sel","nonsel")
          p <- list()
          for (i in seq_along(sel)) {
            avar <- names(x)[sel[i]]
            p[[i]] <- qplot(factor(x[,sel[i]]),fill=factor(x$co),geom="bar") + ylab("") + xlab(avar) +
              theme(axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks=element_blank(), 
                    legend.position="none") +  scale_fill_manual(values = c('grey60','orange'))
          }
        }
          if(length(p)==1) grid.arrange(p[[1]])
          if(length(p)==2) grid.arrange(p[[1]],p[[2]])
          if(length(p)==3) grid.arrange(p[[1]],p[[2]],p[[3]],ncol=2)
          if(length(p)==4) grid.arrange(p[[1]],p[[2]],p[[3]],p[[4]],ncol=2)
          if(length(p)==5) grid.arrange(p[[1]],p[[2]],p[[3]],p[[4]],p[[5]],ncol=2)
          if(length(p)==6) grid.arrange(p[[1]],p[[2]],p[[3]],p[[4]],p[[5]],p[[6]],ncol=2)
          if(length(p)==7) grid.arrange(p[[1]],p[[2]],p[[3]],p[[4]],p[[5]],p[[6]],p[[7]],ncol=2)
          if(length(p)==8) grid.arrange(p[[1]],p[[2]],p[[3]],p[[4]],p[[5]],p[[6]],p[[7]],p[[8]],ncol=2)
        }
      }, 
      height = function() { # fixed height, depends on the window size and number of plot
        if(is.null(v$click_discrete)) h <- 0.8
        else{
        binseq <- binb()
        xv <- v$click_discrete$x
        o <- binseq[which(binseq>xv)[1]]
        if(o==1) o <- 1.1
        u <-  binseq[which(binseq>xv)[1]-1]
        col <- ifelse(discrete1d>=u & discrete1d<o,"1","2")
        sel <- which(col=="1")
        maxp <- input$maxp
        if(length(sel)>maxp){
          sel <- sel[order(multimod1d[sel],decreasing = TRUE)][1:maxp]
        }
        if(length(sel)==1) h <- 0.8
        if(length(sel) == 2) h <- 1
        if(length(sel) %in% c(3,4)) h <- 1
        if(length(sel) %in% c(5,6)) h <- 1.5
        if(length(sel) %in% c(7,8)) h <- 2
        }
        session$clientData$output_discrete_width*h
      })
      
      # histogram of skew1d
      output$skew <- renderPlot({
        binseq <- binb()
        if(is.null(v$click_skew)) qplot(skew1d,breaks=binseq) + ylab("")
        else {
          xv <- v$click_skew$x
          o <- binseq[which(binseq>xv)[1]]
          if(o==1) o <- 1.1
          u <-  binseq[which(binseq>xv)[1]-1] 
          col <- ifelse(skew1d>=u & skew1d<o,"2","1")
          if (length(table(col))==1){
            qplot(skew1d,breaks=binseq) + ylab("")
          }
          else {
            o <- binseq[which(binseq>xv)[1]]
            if(o==1) o <- 1.1
            u <-  binseq[which(binseq>xv)[1]-1]
            col <- ifelse(skew1d>=u & skew1d<o,"2","1")
            qplot(skew1d,breaks=binseq,fill=col) + ylab("")  + 
              scale_fill_manual(values = c('grey60', 'darkblue')) +   theme(legend.position="none")
          }
        }
      }, 
      height = function() {
        session$clientData$output_skew_width*0.8
      })
      
      # histograms of the selected variables
      output$choosen_skew <- renderPlot({
        if(is.null(v$click_skew)) plot(1,1,col="white",xaxt='n',yaxt='n',ylab='',xlab='',pch=19,bty="n")
        else{
          binseq <- binb()
          xv <- v$click_skew$x
          o <- binseq[which(binseq>xv)[1]]
          if(o==1) o <- 1.1
          u <-  binseq[which(binseq>xv)[1]-1] 
          col <- ifelse(skew1d>=u & skew1d<o,"1","2")
          sel <- which(col=="1")
          maxp <- input$maxp
          if(length(sel)>maxp){
            sel <- sel[order(skew1d[sel],decreasing = TRUE)][1:maxp]
          }
          if(is.null(v$click_cvar)){
            p <- list()
            for (i in seq_along(sel)) {
              avar <- names(x)[sel[i]]
              p[[i]] <- ggplot(x,aes_string(x=avar)) + ylab("")  + 
                geom_histogram(fill = 'darkblue',colour='grey10',bins=30) +
                theme(axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks=element_blank())
            }
            }

          else{
            sv <- which(colnames(xc)==input$cvariables)
            classn <- floor(v$click_cvar$x)+1
            tab <- table(xc[,sv])
            co <- ifelse(xc[,sv]==names(tab)[classn],"sel","nonsel")
            p <- list()
            for (i in seq_along(sel)) {
              avar <- names(x)[sel[i]]
              p[[i]] <- ggplot(x,aes_string(x=avar,fill="co")) + ylab("") +  geom_bar() + stat_bin(bins=30) +
                theme(axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks=element_blank(), 
                      legend.position="none") + scale_fill_manual(values = c('grey60','orange'))
            }
          }
          if(length(p)==1) grid.arrange(p[[1]])
          if(length(p)==2) grid.arrange(p[[1]],p[[2]])
          if(length(p)==3) grid.arrange(p[[1]],p[[2]],p[[3]],ncol=2)
          if(length(p)==4) grid.arrange(p[[1]],p[[2]],p[[3]],p[[4]],ncol=2)
          if(length(p)==5) grid.arrange(p[[1]],p[[2]],p[[3]],p[[4]],p[[5]],ncol=2)
          if(length(p)==6) grid.arrange(p[[1]],p[[2]],p[[3]],p[[4]],p[[5]],p[[6]],ncol=2)
          if(length(p)==7) grid.arrange(p[[1]],p[[2]],p[[3]],p[[4]],p[[5]],p[[6]],p[[7]],ncol=2)
          if(length(p)==8) grid.arrange(p[[1]],p[[2]],p[[3]],p[[4]],p[[5]],p[[6]],p[[7]],p[[8]],ncol=2)
        }
      }, 
      height = function() {
        if(is.null(v$click_skew)) h <- 0.8
        else{
        binseq <- binb()
        xv <- v$click_skew$x
        o <- binseq[which(binseq>xv)[1]]
        if(o==1) o <- 1.1
        u <-  binseq[which(binseq>xv)[1]-1] 
        col <- ifelse(skew1d>=u & skew1d<o,"1","2")
        sel <- which(col=="1")
        maxp <- input$maxp
        if(length(sel)>maxp){
          sel <- sel[order(multimod1d[sel],decreasing = TRUE)][1:maxp]
        }
        if(length(sel)==1) h <- 0.8
        if(length(sel) == 2) h <- 1
        if(length(sel) %in% c(3,4)) h <- 1
        if(length(sel) %in% c(5,6)) h <- 1.5
        if(length(sel) %in% c(7,8)) h <- 2
        }
        session$clientData$output_skew_width*h
      })

      # histogram of multimod1d
      output$multimod <- renderPlot({
        binseq <- binb()
        if(is.null(v$click_multimod)) qplot(multimod1d,breaks=binseq) + ylab("")
        else {
          xv <- v$click_multimod$x
          o <- binseq[which(binseq>xv)[1]]
          if(o==1) o <- 1.1
          u <-  binseq[which(binseq>xv)[1]-1]
          col <- ifelse(multimod1d>=u & multimod1d<o,"2","1")
          if (length(table(col))==1){
            qplot(multimod1d,breaks=binseq) + ylab("")
          }
          else {
            o <- binseq[which(binseq>xv)[1]]
            if(o==1) o <- 1.1
            u <-  binseq[which(binseq>xv)[1]-1] 
            col <- ifelse(multimod1d>=u & multimod1d<o,"2","1")
            qplot(multimod1d,breaks=binseq,fill=col) + ylab("")  + 
              scale_fill_manual(values = c('grey60', 'darkgreen')) +   theme(legend.position="none")
          }
        }
      }, 
      height = function() {
        session$clientData$output_multimod_width*0.8
      })
      
      # histograms of the selected variables
      output$choosen_multimod <- renderPlot({
        if(is.null(v$click_multimod)) plot(1,1,col="white",xaxt='n',yaxt='n',ylab='',xlab='',pch=19,bty="n")
        else{
          binseq <- binb()
          xv <- v$click_multimod$x
          o <- binseq[which(binseq>xv)[1]]
          if(o==1) o <- 1.1
          u <-  binseq[which(binseq>xv)[1]-1] 
          col <- ifelse(multimod1d>=u & multimod1d<o,"1","2")
          sel <- which(col=="1")
          maxp <- input$maxp
          if(length(sel)>maxp){
            sel <- sel[order(multimod1d[sel],decreasing = TRUE)][1:maxp]
          }
          if(is.null(v$click_cvar)){
            p <- list()
            for (i in seq_along(sel)) {  
              avar <- names(x)[sel[i]]
              p[[i]] <- ggplot(x,aes_string(x=avar)) + ylab("")  + 
                geom_histogram(fill = 'darkgreen',bins=30,colour='grey10') +
                theme(axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks=element_blank())
            }
          }
          else{
            sv <- which(colnames(xc)==input$cvariables)
            classn <- floor(v$click_cvar$x)+1
            tab <- table(xc[,sv])
            co <- ifelse(xc[,sv]==names(tab)[classn],"sel","nonsel")
            p <- list()
            for (i in seq_along(sel)) {  
              avar <- names(x)[sel[i]]
              p[[i]] <- ggplot(x,aes_string(x=avar,fill="co")) + ylab("") +  geom_bar() + stat_bin(bins=30) +
                theme(axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks=element_blank(), 
                      legend.position="none") +  scale_fill_manual(values = c('grey60','orange'))
            }
          }
          if(length(p)==1) grid.arrange(p[[1]])
          if(length(p)==2) grid.arrange(p[[1]],p[[2]])
          if(length(p)==3) grid.arrange(p[[1]],p[[2]],p[[3]],ncol=2)
          if(length(p)==4) grid.arrange(p[[1]],p[[2]],p[[3]],p[[4]],ncol=2)
          if(length(p)==5) grid.arrange(p[[1]],p[[2]],p[[3]],p[[4]],p[[5]],ncol=2)
          if(length(p)==6) grid.arrange(p[[1]],p[[2]],p[[3]],p[[4]],p[[5]],p[[6]],ncol=2)
          if(length(p)==7) grid.arrange(p[[1]],p[[2]],p[[3]],p[[4]],p[[5]],p[[6]],p[[7]],ncol=2)
          if(length(p)==8) grid.arrange(p[[1]],p[[2]],p[[3]],p[[4]],p[[5]],p[[6]],p[[7]],p[[8]],ncol=2)
        }
      }, 
      height = function() { 
        if(is.null(v$click_multimod)) h <- 0.8
        else{
        binseq <- binb()
        xv <- v$click_multimod$x
        o <- binseq[which(binseq>xv)[1]]
        if(o==1) o <- 1.1
        u <-  binseq[which(binseq>xv)[1]-1] 
        col <- ifelse(multimod1d>=u & multimod1d<o,"1","2")
        sel <- which(col=="1")
        maxp <- input$maxp
        if(length(sel)>maxp){
          sel <- sel[order(multimod1d[sel],decreasing = TRUE)][1:maxp]
        }
        if(length(sel)==1) h <- 0.8
        if(length(sel) == 2) h <- 1
        if(length(sel) %in% c(3,4)) h <- 1
        if(length(sel) %in% c(5,6)) h <- 1.5
        if(length(sel) %in% c(7,8)) h <- 2
        }
        session$clientData$output_multimod_width*h
      })  
    }
  )
}