library(shiny)
sigma <- 10

ui <- fluidPage(

    # Application title
    titlePanel("Variance Test Power: H₀: σ²=10"),

    # Sidebar
    sidebarLayout(
        sidebarPanel(
          sliderInput("n",
                      "sample size:",
                      min = 3,
                      max = 60,
                      value = 15,
                      step=1),
          sliderInput("s2",
                        "True σ²:",
                        min = 1,
                        max = 30,
                        value = 10),
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
      RRl=NA; RRr=NA
      df=input$n-1
      if(input$testType==1) RRl=qchisq(input$alpha, df)*10/df
      if(input$testType==2) RRl=qchisq(input$alpha/2, df)*10/df
      if(input$testType==2) RRr=qchisq(1-input$alpha/2, df)*10/df
      if(input$testType==3) RRr=qchisq(1-input$alpha, df)*10/df
      
      par(mfrow=c(2,1))
      par(mar = c(0,4,1,1) + 0.1)
      x <- seq(0,40, length.out=300); 
      px <- dchisq(x*df/10,df);
      pxA <- dchisq(x*df/input$s2, df)*10/input$s2;
      scaleHeight = max(c(px, pxA))
      print(scaleHeight)
      
      plot(y=px, x=x, type="l", col="darkgray", xaxt="n", ylab="density", ylim=c(0, scaleHeight))
      if(!is.na(RRl)) polygon(y=c(pxA[x<=RRl],0,0)*ifelse(input$s2<19,1,10/input$s2), x=c((x[x<=RRl]),RRl,0), col="pink", border=NA)
      if(!is.na(RRr)) polygon(y=c(pxA[x>=RRr],0,0)*ifelse(input$s2<19,1,10/input$s2), x=c((x[x>=RRr]),40,RRr), col="pink", border=NA)
      lines(y=pxA*ifelse(input$s2<19,1,10/input$s2), x=(x), type="l")
      abline(v=c(RRl,RRr), col="red")
      segments(x0=10,y0=0, y1=-1, col="darkgray")
      segments(x0=input$s2, y0=0, y1=-1, col="black")
      text(x=10, y=dchisq(df,df), "S² (H₀)", col="darkgray", pos=1)
      text(x=input$s2, y=dchisq(df,df)*.85, "S² (true)", col="black", pos=1)
      if(!is.na(RRl)) text(x=RRl, y=dchisq(df,df)/3, pos=2, "Rejection\n Region", col="red")
      if(!is.na(RRr)) text(x=RRr, y=dchisq(df,df)/3, pos=4, "Rejection\n Region", col="red")
      
      s2alt <- seq(0,40,by=.005)
      RR <- rep(0, length(s2alt))
      power <- 0
      if(input$testType<=2){
        RR <- pchisq(RRl*df/s2alt, df)
        power <- pchisq(RRl*df/input$s2, df)
      } 
      if (input$testType>=2){
        RR <- RR + pchisq(RRr*df/s2alt, df, lower.tail=F)
        power <- power + pchisq(RRr*df/input$s2, df, lower.tail=F)
      }
      par(mar = c(2,4,1,1) + 0.1)
      
      plot(x=s2alt, y=RR, type="l", ylab="rejection rate", xlab="true variance", ylim=c(0,1))
      abline(h=input$alpha, col="red",lty=2)
      points(x=input$s2, y=power)
      align <- 1
      if(power<.5) align <-3
      text(x=input$s2, y=power, paste0("Power=",round(power,3)),pos=align)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
