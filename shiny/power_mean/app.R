library(shiny)

ui <- fluidPage(

    # Application title
    titlePanel("One Tail vs Two Tail Test Power: H_0: μ=20"),

    # Sidebar
    sidebarLayout(
        sidebarPanel(
          sliderInput("sigma",
                      "pop sd:",
                      min = 5,
                      max = 25,
                      value = 10),
          sliderInput("n",
                      "sample size:",
                      min = 5,
                      max = 100,
                      value = 20),
          sliderInput("mu",
                        "True μ:",
                        min = 0,
                        max = 40,
                        value = 20),
            sliderInput("alpha",
                        "α:",
                        min = 0.001,
                        max = .2,
                        value = .05),
            radioButtons( 
              inputId = "testType", 
              label = "Test Type", 
              choices = list( 
                "Lower Tail" = 1, 
                "Two Tail" = 2, 
                "Right Tail" = 3 
              ) 
            ), 
        ),

        # Show a plot of the generated plots
        mainPanel(
           plotOutput("distPlot")
        ),
        position = c("left", "right"),
        fluid = TRUE
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    output$distPlot <- renderPlot({
      n = input$n
      alpha = input$alpha
      sigma = input$sigma
      
      RRl=NA; RRr=NA
      if(input$testType==1) RRl=qnorm(alpha, 20, sigma/sqrt(n))
      if(input$testType==2) RRl=qnorm(alpha/2, 20, sigma/sqrt(n))
      if(input$testType==2) RRr=qnorm(1-alpha/2, 20, sigma/sqrt(n))
      if(input$testType==3) RRr=qnorm(1-alpha, 20, sigma/sqrt(n))
      
      par(mfrow=c(2,1))
      par(mar = c(0,4,1,1) + 0.2)
      x <- seq(0,40, length.out=300); 
      px <- dnorm(x,20,sigma/sqrt(n));
      plot(y=px, x=x, type="l", col="gray", xaxt="n", ylab="density")
      pxA <- dnorm(x,input$mu, sigma/sqrt(n));
      if(!is.na(RRl)) polygon(y=c(pxA[x<=RRl],0,0), x=c((x[x<=RRl]),RRl,0), col="pink", border=NA)
      if(!is.na(RRr)) polygon(y=c(pxA[x>=RRr],0,0), x=c((x[x>=RRr]),0,RRr), col="pink", border=NA)
      lines(y=pxA, x=(x), type="l")
      abline(v=c(RRl,RRr), col="red")
      text(x=20, y=dnorm(20,20,sigma/sqrt(n)), "X-bar (H0)", col="darkgray", pos=1)
      text(x=input$mu, y=dnorm(20,20,sigma/sqrt(n))*.85, "X-bar (true)", col="black", pos=1)
      if(!is.na(RRl)) text(x=RRl, y=dnorm(20,20,sigma/sqrt(n))/3, pos=2, "Rejection Region", col="red")
      if(!is.na(RRr)) text(x=RRr, y=dnorm(20,20,sigma/sqrt(n))/3, pos=4, "Rejection Region", col="red")
      
      muAlt <- seq(0,40,by=.005)
      RR <- rep(0, length(muAlt))
      power <- 0
      if(input$testType<=2){
        RR <- pnorm(RRl, mean=muAlt, sd=sigma/sqrt(n))
        power <- pnorm(RRl, mean=input$mu, sd=sigma/sqrt(n))
      } 
      if (input$testType>=2){
        RR <- RR + pnorm(RRr, mean=muAlt, sd=sigma/sqrt(n), lower.tail=F)
        power <- power + pnorm(RRr, mean=input$mu, sd=sigma/sqrt(n), lower.tail=F)
      }

      par(mar = c(2,4,1,1) + 0.2)
      plot(x=muAlt, y=RR, type="l", ylab="rejection rate", xlab="true mu", ylim=c(0,1))
      abline(h=alpha, col="red",lty=2)
      points(x=input$mu, y=power)
      align <- 1
      if(power<.5) align <-3
      text(x=input$mu, y=power, paste0("Power=",round(power,3)),pos=align)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
