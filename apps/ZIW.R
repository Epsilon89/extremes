library(shiny)
pziweibull=function(q,w,shape,scale)
{
  ifelse(q==0,w,(1-w)*pweibull(q,shape,scale)+w)
}

dziweibull=function(x,w,shape,scale)
{
  ifelse(x==0,w,dweibull(x,shape,scale)*(1-w))
}

qziweibull = function(p,w,shape,scale,br=c(0,10000))
{
  if(p<w){0}else{
    G = function(x) pziweibull(x,w,shape,scale)-p
    return( uniroot(G,br)$root ) 
  }
}



ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(width=2,
                 sliderInput("p",
                             "p:",
                             min = 0.01,
                             max = 1,
                             value = 0.1,step=0.01,
                             animate = animationOptions(interval = 10,loop=TRUE)),
                 
                 sliderInput("shape",
                             "shape:",
                             min = 0.1,
                             max = 10,
                             value = 2,step=0.01,
                             animate = animationOptions(interval = 11,loop=TRUE)),
                 sliderInput("scale",
                             "scale:",
                             min =0.1,
                             max = 20,
                             value = 10,step=0.01,
                             animate = animationOptions(interval = 10,loop=TRUE))
                 
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

server <- function(input, output) {
  output$distPlot <- renderPlot({
    
    
    layout(matrix(c(1,2,3),nrow=1))
    par(bg="black",fg="white",col.lab="white",col.main="white",col.lab="white",col.axis="white",col.sub="white") 
    
    probability=input$p
    shape=input$shape
    scale=input$scale
    
    q=seq(0.0001,qziweibull(0.999999,w=probability,shape=shape,scale=scale),length.out = 300)
    p=seq(0,1,length.out = 300)
    
    

    plot(dziweibull(q,w=probability,shape=shape,scale=scale)~q,type="n",ylab="Density",xlab="x",main="ZIW PDF",ylim=c(0,max(dziweibull(q,w=probability,shape=shape,scale=scale),input$p)),xlim=c(0,max(q)))
    abline(v=seq(0,max(q),length.out = 11),col="grey20",lty=2)
    abline(h=seq(0,max(dziweibull(q,w=probability,shape=shape,scale=scale),input$p),length.out = 11),col="grey20",lty=2)
    segments(0,dziweibull(0,w=probability,shape=shape,scale=scale),0,0)
    points(c(0),c(dziweibull(0,w=probability,shape=shape,scale=scale)),type="b")
    points(dziweibull(q,w=probability,shape=shape,scale=scale)~q,type="l",ylab="Density",xlab="x")
    
    plot(pziweibull(q,w=probability,shape=shape,scale=scale)~q,type="n",ylab="Cumulative density",xlab="x",main="ZIW CDF",ylim=c(0,1))
    
    abline(v=seq(0,max(q),length.out = 11),col="grey20",lty=2)
    abline(h=seq(0,1,length.out = 11),col="grey20",lty=2)
    
    segments(0,pziweibull(0,w=probability,shape=shape,scale=scale),0,0)
    points(pziweibull(q,w=probability,shape=shape,scale=scale)~q,type="l",ylab="Density",xlab="x")
    
    plot(sapply(p,function(x)qziweibull(x,w=probability,shape=shape,scale=scale))~p,type="n",ylab="Quantile",xlab="x",main="ZIW quantile function",ylim=c(min(q),max(q)))
    abline(v=seq(0,1,length.out = 11),col="grey20",lty=2)
    abline(h=seq(0,max(q),length.out = 11),col="grey20",lty=2)
    points(sapply(p,function(x)qziweibull(x,w=probability,shape=shape,scale=scale))~p,type="l",ylab="Density",xlab="x")

  })
}

# Run the application 
shinyApp(ui = ui, server = server)


