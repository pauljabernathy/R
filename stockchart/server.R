library(shiny)
source('stocks.r') 

doSimulation <- function(initial = 16.66) {
  numReplays <- 1000;
  sim <- doSNPReturnSimulation(numRuns = numReplays, initial = initial);
  ss <- summary(sim);
  summaries <- data.frame(as.numeric(ss[1]), as.numeric(ss[2]), as.numeric(ss[3]), as.numeric(ss[4]), as.numeric(ss[5]), as.numeric(ss[6]));
  names(summaries) <- names(ss)
  
  return(summaries)
}
final <- 0;

shinyServer(function(input, output) {
  
  snp <- read.csv("sp500.csv");
  snp <- snp[order(snp$Date),]  #reverse it because the file gives the latest days first
  oneday <- findDiffs(snp$Close);
  onedayr <- findDiffRatios(snp$Close);
 #) )
  
  output$stockchart <- renderPlot({
    input$oneChartButton
    isolate(
      initial <- input$initial #if you have '=' instead of '<-' you get error warnings saying "unused argument (initial = input$initial)"
    )
    isolate(
      numbers <- initial * cumprod(sample(onedayr, length(onedayr), T))
    )
    input$oneChartButton
    isolate (
      plot(numbers, type="l", main="randomly generated plot based on historical prices of the S&P 500 from Jan. of 1950 to April of 2014", 
         ylab="S & P 500", xlab="days after Jan. 3, 1950")
    )
    isolate (
      #if you have the <<- here, it allows you to define final outside of the functions for output$stockchart or output$amount, so they can both use the same array and you can display the amount beneath the chart
      #this took a lot of trial and error to discover how to do this
      final <<- numbers[length(numbers)]
    )
  })
  
  output$amount <- renderPrint({
    input$oneChartButton
    isolate(
      paste("final value: ", final)
    )
  })

  output$sim <- renderPrint({
    input$doSimButton
    isolate(
      doSimulation(initial = input$initial)
    )
  })
})