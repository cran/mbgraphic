### interactive pcp with scagnostics results 
iascagpcp <- function(sdfdata){
  if(class(sdfdata)!="sdfdata") stop("sdfdata needs to be of class sdfdata. Please use the functions sdf and scagn2sdf.")
  x_sdf <- sdfdata[[1]]
  data <- sdfdata[[2]]
  if(sum(names(x_sdf)=="status")==1){
    s <- x_sdf[order(x_sdf$status,decreasing=FALSE),]
    ns <- ncol(x_sdf) - 3
  }
  else {
    s <- x_sdf
    ns <- ncol(x_sdf) - 2
  }
  s$id <- seq_along(s[,1])
  
  if(sum(names(x_sdf)=="status")==1){
   # oe <- subset(s,status %in% c("exemplar","outlier"))
    oe <- s[s$status %in% c("exemplar","outlier"),]
  }
  
  # scale function 
  scalef <- function(s,scale="std"){
    sc <- s
    if(sum(names(s)=="status")==1)  ns <- ncol(s) - 3
    else (ns <- ncol(s) - 2)
    for(i in 1:ns){
      if(scale=="std") sc[,i] <- (s[,i] - mean(s[,i]))/sd(s[,i])
      if(scale=="unimm") sc[,i] <- (s[,i] - min(s[,i]))/(max(s[,i]) - min(s[,i]))
    }
    return(sc)
  }
  
  shinyApp(
  ui = fluidPage(
    fluidRow(
      column(width=3,
             if(sum(names(x_sdf)=="status")==1){
               selectInput("select", label = h3("Plots"), 
                           choices = list("All" = 1, "Only Outliers and Exemplars" = 2),
                           selected = 2)},
             selectInput("scale", label = h3("Scale"), 
                         choices = list("std" = "std", "uniminmax" = "uniminmax","globalminmax"="globalminmax"),
                         selected = "std"),
             sliderInput("alpha", "Alpha:", 
                         min = 0, max = 1, value = 0.2, step= 0.05)
      ),
      column(width=9,
             uiOutput("ui")
      )),
    
    fluidRow(
      column(width=3,
             verbatimTextOutput("plot_brushinfo")),
      column(width=5,
             plotOutput("choosen_scag")
      )
    )
  ),
  
  server = function(input, output, session) {
    if(sum(names(x_sdf)=="status")==1){
    output$ui <- renderUI({
      if(input$select=="1"){  
        column(width=12,
               plotOutput("pcp", height="auto",brush = brushOpts(id = "plot_brush",opacity = 0.25))
        )
      }
      else if(input$select=="2"){  
        column(width=12,
               plotOutput("pcp_oe", height="auto",brush = brushOpts(id = "plot_brush",opacity = 0.25))
        )
      }
    })
    
    nearest_ra <- reactive({
      measure <- round(mean(c(input$plot_brush$xmax, input$plot_brush$xmin))) 
      value <- mean(c(input$plot_brush$ymax, input$plot_brush$ymin))
      
      if(input$select==1){
        if(input$scale=="std") nearest <- order(abs(scalef(s)[,measure] - value))[1]
        if(input$scale=="globalminmax") nearest <- order(abs(s[,measure] - value))[1]
        if(input$scale=="uniminmax") nearest <- nearest <- order(abs(scalef(s,scale="unimm")[,measure] - value))[1]
      }
      if(input$select==2){
        if(input$scale=="std") nearest <- order(abs(scalef(oe)[,measure] - value))[1]
        if(input$scale=="globalminmax") nearest <- order(abs(oe[,measure] - value))[1]
        if(input$scale=="uniminmax") nearest <- order(abs(scalef(oe,scale = "unimm")[,measure] - value))[1]
      }
      return(nearest)
    })
    
    output$pcp <- renderPlot({
        if(!is.null(input$plot_brush)){
          nearest <- nearest_ra()
          char <- as.character(s$status)
          char[nearest] <- "selected"
          s$status <- factor(char,levels=c("non","selected","outlier","exemplar"))
          levels(s$status) <- c("Non","Selected","Outlier","Exemplar")
          ord <- c(s$id[-nearest],nearest)
          s <- s[ord,]
        }
        colval <- c("black", "red","darkred", "green2")
        
        if(is.null(input$plot_brush)){
          char <- as.character(s$status)
          s$status <- factor(char,levels=c("non","outlier","exemplar"))
          levels(s$status) <- c("Non","Outlier","Exemplar")
          colval <- c("black","darkred", "green2")
        }
      
      v_alpha <- as.numeric(input$alpha)
      alpha <- vector(length=nrow(s))
      for(i in 1: length(alpha)){
        if(s$status[i] %in% c("Non","Outlier")) alpha[i] <- v_alpha
        else(alpha[i] <- 1)
      }
      s <- cbind(s,alpha)
      sc <- input$scale
      names(s)[ncol(s)] <- "Alpha"
      names(s)[ns+3] <- "Status"
      ggparcoord(s,1:ns,scale=sc,alphaLines ="Alpha",groupColumn =ns+3) + 
        scale_color_manual(values=colval) +
        theme(legend.text=element_text(size=15),legend.title=element_text(size=15,face="bold"))
    }, 
    height = function() {
      0.5*session$clientData$output_pcp_width
    })
    
    output$pcp_oe <- renderPlot({
        if(!is.null(input$plot_brush)){
          nearest <- nearest_ra()
          char <- as.character(oe$status)
          char[nearest] <- "selected"
          oe$status <- factor(char,levels=c("selected","outlier","exemplar"))
          levels(oe$status) <- c("Selected","Outlier","Exemplar")
          ord <- c(as.vector(1:nrow(oe))[-nearest],nearest)
          oe <- oe[ord,]
          colval <- c("red","black", "green2")
        }
        
        if(is.null(input$plot_brush)){
          char <- as.character(oe$status)
          oe$status <- factor(char,levels=c("outlier","exemplar"))
          levels(oe$status) <- c("Outlier","Exemplar")
          colval <- c("black","green2")
        }
      
      
      v_alpha <- as.numeric(input$alpha)
      alpha <- vector(length=nrow(oe))
      for(i in 1: length(alpha)){
        if(oe$status[i] %in% c("Outlier")) alpha[i] <- v_alpha
        else(alpha[i] <- 1)
      }
      oe <- cbind(oe,alpha)
      sc <- input$scale
      names(oe)[ncol(oe)] <- "Alpha"
      names(oe)[ns+3] <- "Status"
      ggparcoord(oe,1:ns,scale=sc, alphaLines ="Alpha",groupColumn =ns+3) +  
        scale_color_manual(values=colval) +
        theme(legend.text=element_text(size=15),legend.title=element_text(size=15,face="bold"))
      # 
    }, 
    height = function() {
      0.5*session$clientData$output_pcp_oe_width
    })
    }
   
    # no status
     else {
      output$ui <- renderUI({
          column(width=12,
                 plotOutput("pcp_else", height="auto",brush = brushOpts(id = "plot_brush",opacity = 0.25))
          )
      })
      
      nearest_ra <- reactive({
        measure <- round(mean(c(input$plot_brush$xmax, input$plot_brush$xmin))) 
        value <- mean(c(input$plot_brush$ymax, input$plot_brush$ymin))
         if(input$scale=="std") nearest <- order(abs(scalef(s)[,measure] - value))[1]
         if(input$scale=="globalminmax") nearest <- order(abs(s[,measure] - value))[1]
         if(input$scale=="uniminmax") nearest <- order(abs(scalef(s,scale="unimm")[,measure] - value))[1]
         return(nearest)
      })
      
      output$pcp_else <- renderPlot({
          if(!is.null(input$plot_brush)){
            nearest <- nearest_ra()
            highl <- as.character(rep("non",nrow(s)))
            highl[nearest] <- "selected"
            s$highl <- factor(highl,levels=c("non","selected"))
            levels(s$highl) <- c("Non","Selected")
            ord <- c(s$id[-nearest],nearest)
            s <- s[ord,]
          }
          colval <- c("black", "red")
          
          if(is.null(input$plot_brush)){
            highl <- as.character(rep("Non",nrow(s)))
            s$highl <- factor(highl,levels="Non")
            colval <- c("black")
          }
        
        v_alpha <- as.numeric(input$alpha)
        alpha <- vector(length=nrow(s))
        for(i in 1: length(alpha)){
          if(s$highl[i] == "Non") alpha[i] <- v_alpha
          else(alpha[i] <- 1)
        }
        s <- cbind(s,alpha)
        sc <- input$scale
        names(s)[ncol(s)] <- "Alpha"
        names(s)[ncol(s)-1] <- "Status"
        ggparcoord(s,1:ns,scale=sc,alphaLines ="Alpha",groupColumn =ncol(s)-1) + 
          scale_color_manual(values=colval) +
          theme(legend.text=element_text(size=15),legend.title=element_text(size=15,face="bold"))
      }, 
      height = function() {
        0.5*session$clientData$output_pcp_else_width
      })
        }
    
    pdata <- reactive({
      nearest <- nearest_ra()
      if(sum(names(x_sdf)=="status")==1){
      if(is.null(input$plot_brush)) return(as.data.frame(x=0,y=0))
      else{  
        if(input$select==1){
          x <- s$x[nearest]
          y <- s$y[nearest]
        }
        if(input$select==2){
          x <- oe$x[nearest]
          y <- oe$y[nearest]
        }
      }
      }
      else {
        x <- s$x[nearest]
        y <- s$y[nearest]
      }
        data_ac <- data[,c(as.numeric(x),as.numeric(y))]
        ret <- data_ac[complete.cases(data_ac),]
        return(ret)
    })
    
    
    output$choosen_scag <- renderPlot({
      if(is.null(input$plot_brush)) {
        qplot(0,0,xlab="",ylab="",alpha=I(1/1000)) + theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                           axis.text.y=element_blank(),axis.ticks=element_blank()
        ) }
      else {
        xy <- pdata()
        qplot(xy[,1],xy[,2],xlab=paste(names(xy)[1]),ylab=paste(names(xy)[2])) 
      }
    },
    height = function() {
      0.7*session$clientData$output_choosen_scag_width
    })
    
    output$plot_brushinfo <- renderPrint({
      if(is.null(input$plot_brush)) return("No plot selected")
      if(!is.null(input$plot_brush)){
        cat("Selected plot \n")
      }
      nearest <- nearest_ra()
      if(sum(names(x_sdf)=="status")==1){
      if(input$select==1){
        rownames(s) <- NULL
        return(t(round(s[nearest,1:ns],3)))
      }
      if(input$select==2){
        rownames(oe) <- NULL
        return(t(round(oe[nearest,1:ns],3)))
      }
      }
      else{
        rownames(s) <- NULL
        return(t(round(s[nearest,1:ns],3)))
      }
    })
  }
  
)
}