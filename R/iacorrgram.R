### interactive corrgram
iacorrgram <- function(data) {
  data <- data[,sapply(data,is.numeric)]
  shinyApp(
    options = list(width="100%", height=700),
    ui = fluidPage(style="padding-top: 1px;",
      fluidRow(responsive = FALSE,
               column(width=3,
               selectInput("order", label = h3("Reordering?"), 
                           choices = list("No" = 1, "PCA" = 2,"OLO"=3),
                           selected = 3),
               sliderInput("corrval", label="Range of absolute correlation", min=0, max=1, value=c(0,1), step = NULL),
               numericInput("clusters", label="Number of clusters (only for OLO)", value=1, min = 1, max = ncol(data), step = 1,
                            width = NULL),
                   numericInput("mincor", label="Minimal correlation within the clusters", value=NA, min = -1, max = 1, step = 0.1,
                                width = NULL)
               ),
               column(width=5,
                      plotOutput("plotD", height="auto",click="plot_click", 
                                 brush = brushOpts(id = "plot_brush",opacity = 0.5,fill="darkgreen",stroke="darkgreen"))
               ),
               column(width=4,
               plotOutput("choosen_plot")
               )
      )
    ),
    server = function(input, output, session) {
      observe({
       corval <- input$mincor
       if(!is.na(corval)){
          if(corval<=1 & corval>=-1){ 
            c <- varclust(data,mincor=corval)[[1]]
            updateNumericInput(session, "clusters", value = c)
          }
       }
      })
      output$plotD <- renderPlot({
        data <- rdata()
        corrgr(data,cmin=as.numeric(input$corrval[1]),cmax=as.numeric(input$corrval[2]))
        if(input$order==3){
          if(input$clusters>1){
          ncluster <- input$clusters
          clines <- cutcluster(data,c=ncluster)
          for(i in 1:(length(clines)+1)){
            abline(v=clines[i],h=-clines[i],col="darkgreen",lwd=2)
            if(i==1) rect(0,0,clines[i],-clines[i],border="darkgreen",lwd=5)
            else if(i==(length(clines)+1)) rect(clines[i-1],-clines[i-1],ncol(data),-ncol(data),border="darkgreen",lwd=5)
            else {
              rect(clines[i-1],-clines[i-1],clines[i],-clines[i],border="darkgreen",lwd=5)
            }
          }
          }
        }
      }, 
      height = function() {
        session$clientData$output_plotD_width
      })
      rdata <- reactive({
        x_roh <- data 
        co <- cor(x_roh,use="pairwise.complete.obs")
        if(input$order==2){
          x.eigen <- eigen(co)$vectors[, 1:2]
          e1 <- x.eigen[, 1]
          e2 <- x.eigen[, 2]
          alpha <- ifelse(e1 > 0, atan(e2/e1), atan(e2/e1) + pi)
          ord <- order(alpha)
          x <- x_roh[, ord]
        }
        if(input$order==3){
          distx <- 1 - as.dist(co)
          hc <- hclust(distx,method = "average")
          al <- seriate(distx,method="OLO",control=list(hclust=hc))
          ord <- get_order(al)
          x <- x_roh[, ord]
        }
        if(input$order==1) {x <- x_roh}
        ret <- x
        return(ret) 
      })  
      val <- reactive({
        if(is.null(input$plot_click) & is.null(input$plot_brush)) ret <- NULL
        else if(!is.null(input$plot_click) & is.null(input$plot_brush)) ret <- c(floor(input$plot_click$x+1),floor(-input$plot_click$y+1)) 
        else{ 
          x <- floor(input$plot_brush$xmin+1):ceiling(input$plot_brush$xmax) 
          y <- floor(-input$plot_brush$ymin+1):ceiling(-input$plot_brush$ymax) 
          ret <- list(x=x,y=sort(y))
        }
        return(ret)
      })
      
      output$choosen_plot <- renderPlot({
        data <- rdata()
        if(is.null(input$plot_click) & is.null(input$plot_brush)) plot(1,1,col="white",xaxt='n',yaxt='n',ylab='',xlab='',pch=19)
        else if(!is.null(input$plot_click) & is.null(input$plot_brush)){
          sel <- val()
          plot(data[,sel],pch=19)
        }
        else {
          x <- val()$x
          y <- val()$y
          par(mar = c(0,0,0,0) + 0.1)
          par(oma=c(1,1,1,0) + 0.1)
          par(xpd=NA,cex=0.5)
          
          l <- (length(x)+1)*(length(y)+1)
          pl <- c(1:length(x),0,(length(x)+1):(l-1))
          
          m <- matrix(pl,ncol=length(x)+1,nrow=length(y)+1,byrow=TRUE)
          layout(m)     
          
          for(i in 1:length(x)){
            plot(0,xaxt='n',yaxt='n',bty='n',pch='',ylab='',xlab='')
            text(-0.8,names(data)[x[i]])
          }
          k <- y[1]
          for(i in y){
            for(j in x){
              if(i != j)plot(data[,c(j,i)],xaxt='n',yaxt='n',ylab='',xlab='',pch=19)
              else hist(data[,i],col="grey",main='',ylab='',xlab='',xaxt='n',yaxt='n')
            }
            plot(0,0,xaxt='n',yaxt='n',bty='n',pch='',ylab='',xlab='')
            text(-0.5,0,names(data)[k])
            k <- k+1
          }
        }
      },
      height = function() {
        session$clientData$output_choosen_plot_width
      })
    }
  )
}