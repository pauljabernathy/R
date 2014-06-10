library(shiny)
source('server.R')
shinyUI(fluidPage(
  titlePanel("Stock Chart Generator"),
  
  sidebarLayout(
    sidebarPanel(numericInput("initial",label = h4("Initial Investment"), value=16.66), 
                              actionButton("oneChartButton","get one chart"),
                 numericInput("numRuns", label=h4("number of simulations"), value=1000),
                 actionButton("doSimButton","Do Simulation")
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