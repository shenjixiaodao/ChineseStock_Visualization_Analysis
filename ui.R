library(shiny)
library(dygraphs)
IndexCategory = read.table(IndexCategory_file, header = TRUE,row.names = 3, colClasses = "character",sep = "\t")
IndustryCategory = read.table(IndustryCategory_file, header = TRUE,row.names = 1, sep = "\t")
source("datafiles_name.R")

layout_left_width = 2
layout_right_width = 12 - layout_left_width

shinyUI(fluidPage(
  
  titlePanel("stockVis"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("提示文本"),
      
      dateRangeInput("dates", 
                     "日期范围",
                     start = "2015-10-01", 
                     end = as.character(Sys.Date())),
      br(),
      br(),
      
      checkboxInput("log", "Plot y axis on log scale", 
                    value = FALSE)
      #,checkboxInput("adjust", 
      #              "Adjust prices for inflation", value = FALSE)
      , width = layout_left_width),
      
    mainPanel(
      #plotOutput("plot")
      tabsetPanel(
        tabPanel("个股记录",
                 fluidRow(
                   column(2,textInput("symb", "股票代码", "")),
                   column(4,selectInput("base_index", label = "加入指数基线", 
                               choices = {
                                 #读入行业类别数据
                                 #temp = t(IndexCategory)
                                 #隐式返回要求的list数据
                                 #as.list(temp[2,])
                                 list()
                               }, multiple = TRUE)
                 )),
                 fluidRow(
                   dygraphOutput("dygraph")
                 )
        ),
        tabPanel("财务概况", 
                 fluidRow(
                   #行业输入
                   column(2,selectInput("FS_IndustryCategory", label = "行业分类", 
                                        choices = {
                                          #读入行业类别数据
                                          temp = t(IndustryCategory)
                                          #隐式返回要求的list数据
                                          as.list(temp[1,])
                                        })
                    ),
                   column(6,selectInput("FS_IC_Stocks", label = "加入个股", 
                                        choices = {
                                        }, multiple = TRUE)
                 )),
                 fluidRow(
                   column(5,dygraphOutput("FS_asset_dygraph")),
                   column(5,dygraphOutput("FS_profit_dygraph"))
                 ),
                 fluidRow(
                   column(5,dygraphOutput("FS_cash_dygraph")),
                   column(5,dygraphOutput("FS_reserve_dygraph"))
                 )
        ),
        tabPanel("收益概况", value = "YS",
                 fluidRow(
                   #行业输入
                   column(2,selectInput("YS_IndustryCategory", label = "行业分类", 
                                        choices = {
                                          #读入行业类别数据
                                          temp = t(IndustryCategory)
                                          #隐式返回要求的list数据
                                          as.list(temp[1,])
                                        })
                   ),
                   column(6,selectInput("YS_IC_Stocks", label = "加入个股", 
                                        choices = {
                                        }, multiple = TRUE)
                   )),
                 fluidRow(
                   tableOutput("YeildIndic")
                 )
                 
        )
      )
    , width = layout_right_width)
  )
))


