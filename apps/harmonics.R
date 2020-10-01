library(shiny)

ui <- fluidPage(

    sidebarLayout(
        sidebarPanel(width=3,
            sliderInput("alpha",
                        "alpha:",
                        min = -2,
                        max = 2,
                        value = 0,step=0.01,
                        animate = animationOptions(interval = 10,loop=TRUE)),
            
            sliderInput("beta",
                        "beta:",
                        min = -2.,
                        max = 2,
                        value = 0.5,step=0.01,
                        animate = animationOptions(interval = 11,loop=TRUE)),
            sliderInput("gamma",
                        "gamma:",
                        min =0,
                        max = 1,
                        value = 0,step=0.01,
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
        
        par(bg="black",fg="white",col.lab="white",col.main="white",col.lab="white",col.axis="white",col.sub="white") 
        t=seq(0,1,length.out=100)
        alpha=input$alpha
        beta=input$beta
        gamma=input$gamma
        
        plot(alpha+beta*cos(2*pi*t-gamma)~t,ylab="y",xlab="Scaled x",ylim=c(-2,2),type="n")
        legend("bottomleft",legend=c(as.expression(bquote(alpha~"="~.(alpha))),
                                     as.expression(bquote(beta~"="~.(beta))),
                                     as.expression(bquote(gamma~"="~.(gamma)))),bty="n")
        abline(v=seq(0,1,by=0.1),col="grey20",lty=2)
        abline(h=seq(-10,10,by=1),col="grey20",lty=2)
        points(alpha+beta*cos(2*pi*t-gamma)~t,type="l",ylab="y",xlab="Scaled x",ylim=c(-2,2))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)






