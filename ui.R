library(shiny)

shinyUI(fluidPage(
  titlePanel("stockVis"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Select a stock to examine. 
               Information will be collected from yahoo finance."),
      
      textInput("symb", "Symbol", "SPY"),
      
      dateRangeInput("dates", 
                     "Date range",
                     start = "2013-01-01", 
                     end = as.character(Sys.Date())),
      
      br(),
      br(),
      selectInput("selectInput", label = "selectInput", 
                  choices = list(`股票1` = "000001",`股票2` = "000002")),
      
      checkboxInput("log", "Plot y axis on log scale", 
                    value = FALSE),
      
      checkboxInput("adjust", 
                    "Adjust prices for inflation", value = FALSE)
      ),
      
    mainPanel(plotOutput("plot"))
  )
))
