library(shiny)

ui <- basicPage(
    plotOutput("plot1", click = "plot_click"),
    verbatimTextOutput("info")
)

server <- function(input, output) {

    output$plot1 <- renderPlot({
        
par(mfrow=c(1,2))        
        par(bg="black",fg="white",col.lab="white",col.main="white",col.lab="white",col.axis="white",col.sub="white")
    plot(Data$PRCP~Data$yday,pch=20,cex=0.1,ylab="Precipitation (in.)",xlab="Day of year")
    abline(v=seq(0,max(Data$yday),length.out = 11),col="grey20",lty=2)
    abline(h=seq(0,max(Data$PRCP),length.out = 11),col="grey20",lty=2)
    
    sapply(length(quantile_names):2,function(i){
        suppressMessages(polygon(c(quantiles$yday,rev(quantiles$yday)),c(quantiles[[quantile_names[i]]],rev(quantiles[[quantile_names[i-1]]])),col=col[i],border=NA))
    })
    
    
    
    probability=0.1#newdata[which.min(abs(input$plot_click$x-newdata$yday)),]$w
    shape=2#newdata[which.min(abs(input$plot_click$x-newdata$yday)),]$shape
    scale=7#newdata[which.min(abs(input$plot_click$x-newdata$yday)),]$scale
    
    q=seq(0.0001,80,length.out = 300)
    p=seq(0,1,length.out = 300)

    plot(dziweibull(q,w=probability,shape=shape,scale=scale)~q,type="n",ylab="Density",xlab="x",main="ZIW PDF",ylim=c(0,max(dziweibull(q,w=probability,shape=shape,scale=scale),input$p)),xlim=c(0,max(q)))
    abline(v=seq(0,max(q),length.out = 11),col="grey20",lty=2)
    abline(h=seq(0,max(dziweibull(q,w=probability,shape=shape,scale=scale),input$p),length.out = 11),col="grey20",lty=2)
    segments(0,dziweibull(0,w=probability,shape=shape,scale=scale),0,0)
    points(c(0),c(dziweibull(0,w=probability,shape=shape,scale=scale)),type="b")
    points(dziweibull(q,w=probability,shape=shape,scale=scale)~q,type="l",ylab="Density",xlab="x")

    
    },width = 1500, height = 750)
    
    
    output$info <- renderText({
        paste0("x=", input$plot_click$x, "\ny=",newdata[which.min(abs(input$plot_click$x-newdata$yday)),])
        
        

    })
}

shinyApp(ui, server)


