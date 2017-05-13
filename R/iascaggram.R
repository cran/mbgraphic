### interactive scaggram 
iascaggram <- function(sdfdata) {
 if(class(sdfdata)!="sdfdata") stop("sdfdata needs to be of class sdfdata. Please use the functions sdf and scagn2sdf.")
  x_sdf <- sdfdata[[1]]
  x <- sdfdata[[2]]
  if(sum(names(x_sdf)=="status")==1) ns <- ncol(x_sdf) - 3
  else ns <- ncol(x_sdf) - 2
  shinyApp(
    ui = fluidPage(
      fluidRow(
        column(width=7,
               plotOutput("plotD", height="auto",click="plot_click", 
                          brush = brushOpts(id = "plot_brush",opacity = 0.4,fill="black",stroke="black")),
               verbatimTextOutput("plot_brushinfo")
        ),
        column(width=5,
               selectInput("order", label = h3("Reordering?"), 
                           choices = list("No" = 1,"Quick"=2,"Use all scagnostics"=3,"Use only chosen scagnostics"=4),
                           selected = 1),
               selectInput("add", label = h3("Add"), 
                           choices = list("Nothing" = FALSE,"Scatterplots"="splom",
                            "Glyphs"="glyphs"),selected = FALSE),
               selectInput("scagnostics", multiple = TRUE,label="Choose up to three scagnostics", choices=names(x_sdf)[1:ns], selected = names(x_sdf)[1], width = NULL),
               plotOutput("choosen_plot")
        )
      )
    ),
    
    server = function(input, output, session) {

      rdata <- reactive({
        if(input$order==2){
          y <- sdf_quicksort(sdfdata)
        }  
        if(input$order==1){
          y <- list(x_sdf,x)
        }
        if(input$order==3){
          y <- sdf_sort(sdfdata)
        }
        if(input$order==4){
          sel <- which(names(x_sdf) %in% c(as.character(input$scagnostics),"x","y"))
          sdfdata <- list(sdf=x_sdf[,sel],data=x)
          class(sdfdata) <- "sdfdata"
          y <- sdf_sort(sdfdata)
          ordn <- names(y[[2]])
          ord <- vector(length =length(ordn))
          for(i in 1:length(ordn)){
            ord[i] <- which(ordn[i]==names(x))
          }
          y[[2]] <- x[, ord] 
          x_n <-rep(0,nrow(x_sdf))
          y_n <-rep(0,nrow(x_sdf))
          for(i in 1:ncol(x)){
            x_n[which(x_sdf$x==ord[i])] <- i
            y_n[which(x_sdf$y==ord[i])] <- i
          }
          xy <- cbind(x_n,y_n)
          xy <- t(apply(xy,1,valueflip))
          sdf_s <- cbind(x_sdf[,which(!names(x_sdf) %in% c("x","y","status"))],x=xy[,1],y=xy[,2])
          sdf_s <- sdf_s[order(sdf_s$y),]
          y[[1]] <- sdf_s[order(sdf_s$x),]
        }
        class(y) <- "sdfdata"
        return(y)
        })
      
      output$plotD <- renderPlot({
        y <- rdata()
        selection <- as.character(input$scagnostics)
        if(length(selection)==0){
          opt <- options(show.error.messages=FALSE) 
          on.exit(options(opt)) 
          stop() 
        }
        if(length(selection)>=4){
          plot(0,xaxt='n',yaxt='n',bty='n',pch='',ylab='',xlab='')
          text(c(0,0),"You can only choose three scagnostics at the same time",col="red",cex=1.25)
        }
        else scaggram(y,select=selection,add=input$add)
      }, 
      height = function() {
        session$clientData$output_plotD_width
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
        data <- rdata()[[2]]
        if(is.null(input$plot_click) & is.null(input$plot_brush)) plot(1,1,col="white",xaxt='n',yaxt='n',ylab='',xlab='',pch=19)
        else if(!is.null(input$plot_click) & is.null(input$plot_brush)){
          sel <- val()
          plot(data[,sel],pch=19)
        }
        else {
          x <- val()$x
          y <- val()$y
          par(mar = c(0,0,0,0) + 0.1)
          par(oma=c(3,1,1,0) + 0.1)
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
      })
      
      output$plot_brushinfo <- renderPrint({
        y <- rdata()[[1]]
        sel <- val()
        if(is.null(input$plot_click) & is.null(input$plot_brush)) return("No plot selected")
        else if(!is.null(input$plot_click) & is.null(input$plot_brush)){
          cat("Selected plot \n")
          if(sel[1]<sel[2]) {row <- which(y$x==sel[1] & y$y==sel[2])}
          else (row <- which(y$x==sel[2] & y$y==sel[1]))
          return(t(unclass(round(y[row,1:ns],3))))
        }
        else{
          return("More than one plot selected")
          }
      })
    }
  )
}