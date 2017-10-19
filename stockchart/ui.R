library(shiny)
source('server.R')

getExplanation <- function() {
  explanation <- "This app implements the stock market simulation described in the slide deck.  The idea is to randomly sample from the historical daily changes of the S & P 500 index, going back to 1950, and create a \"what might have been\" simulation.  
  
  <br>Here are explanations of each field
  <br><b>Initial Investment</b> - lets you input an initial amount to invest.  <br><b>\"Get One Chart\" button</b> - allows you to get one possibly chart based on randomly sampling the historical daily changes."
}

shinyUI(fluidPage(
  titlePanel("Stock Chart Generator"),
  
  sidebarLayout(
    sidebarPanel(numericInput("initial",label = h4("Initial Investment"), value=16.66), 
                              actionButton("oneChartButton","get one chart"),
                 numericInput("numRuns", label=h4("number of simulations"), value=1000),
                 actionButton("doSimButton","Do Simulation"),
                 h4("Idea:"),
                 p("This app implements the stock market simulation described in the slide deck at http://pauljabernathy.github.io/proj1/.  The idea is to randomly sample from the historical daily changes of the S & P 500 index, going back to 1950, and create a \"what might have been\" simulation."),
                 h4("Initial Investment"),
                 p("lets you input an initial amount to invest"),
                 h4("\"Get One Chart\" button"),
                 p("generates one chart to the right, representing one \"alternative history\" of what could have happened"),
                 h4("\"number of runs...\" input field"),
                 p("how many \"alternative histores\" to simulate in the Monte Carlo simulation"),
                 h4("\"Do Simulation\" button"),
                 p("runs the Monte Carlo simulation with the specified number of runs"),
                 h4("github code"),
                 p("https://github.com/pauljabernathy/proj1/tree/master/stockchart (since I forgot to submit the link with the assignment)")
    ),
    mainPanel(
      plotOutput("stockchart"),
      verbatimTextOutput("amount"),
      p("monte carloe results"),
      verbatimTextOutput("sim")
#       verbatimTextOutput(renderPrint({
#         input$goButton
#         isolate(
#           doSimulation()[1]
#         )
#       }))
    )
  )
))