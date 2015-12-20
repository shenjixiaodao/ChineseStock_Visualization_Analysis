# server.R

library(quantmod)
library(stringr)
#source("helpers.R")
#library(rvest)
source("sina_financial.R")

shinyServer(function(input, output) {
  #stockID_pattern = "\\d{6}\\.[sS][sSzZ]"
  stockID_pattern = "\\d{6}"
  
  dataInput <- reactive({
    #setSymbolLookup(GZMT=list(name=input$symb,src='yahoo'))
    #data <- getSymbols("GZMT", 
    #                   from = input$dates[1],
    #                   to = input$dates[2],
    #                  auto.assign = FALSE)
    data = getPeriodRestorationofRightPrice(input$dates[1],input$dates[2],input$symb)
    return (data)
  })
  
  output$plot <- renderPlot({
    if(length(grep(stockID_pattern, str_trim(input$symb))) == 0)
      return (NA)
    #chartSeries(GZMT)
    chartSeries(dataInput(), theme = chartTheme("white"), 
                type = "line", log.scale = input$log, TA = NULL)
  })
  
})


setSymbolLookup(GZMT=list(name="600030.ss",src='yahoo'))
data <- getSymbols("GZMT", 
                   from = "2013-01-01",
                   to = as.character(Sys.Date()),
                   auto.assign = FALSE)



