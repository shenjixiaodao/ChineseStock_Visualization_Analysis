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
      
      helpText("提示信息:"),
      verbatimTextOutput("info"),
      tableOutput("hint")
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
                                 list()
                               }, multiple = TRUE)),
                   column(3,dateRangeInput("dates", "日期范围",start = "2015-10-01", end = as.character(Sys.Date())))
                   
                 ),
                 fluidRow(
                   column(2,uiOutput("followers", inline = TRUE))
                 ),
                 fluidRow(
                   dygraphOutput("dygraph", width = "95%")
                 ),
                 fluidRow(
                   column(width = 4,plotOutput("RtoV_plot")),
                   column(width = 8,plotOutput("RandV_plot"))
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
        ),
        tabPanel("财务",
           fluidRow(
             #行业输入
             column(2,selectInput("Finance_IndustryCategory", label = "行业分类", 
                                  choices = {
                                    #读入行业类别数据
                                    temp = t(IndustryCategory)
                                    #隐式返回要求的list数据
                                    as.list(temp[1,])
                                  })
             ),
             column(6,selectInput("Finance_IC_Stocks", label = "加入个股", 
                                  choices = {
                                  }, multiple = TRUE)
           )),
           tabsetPanel(
             tabPanel("财务概况",
                fluidRow(plotOutput("FS_graphs",width = "95%", height = "600px"))
             ),
             tabPanel("涨幅～财务",
                      fluidRow(
                        column(5,plotOutput("UDF_UD_1_plot",width = "95%", click = "FS_dbclick")),
                        column(5,plotOutput("UDF_UD_2_plot",width = "95%", click = "FS_dbclick"))
                      ),
                      fluidRow(
                        column(5,plotOutput("UDF_UD_3_plot",width = "95%", click = "FS_dbclick")),
                        column(5,plotOutput("UDF_UD_4_plot",width = "95%", click = "FS_dbclick"))
                      ),
                      fluidRow(
                        column(5,plotOutput("UDF_UD_5_plot",width = "95%", click = "FS_dbclick")),
                        column(5,plotOutput("UDF_UD_6_plot",width = "95%", click = "FS_dbclick"))
                      )
             ),id = "F_Panel")
      ),
      tabPanel("大盘走势", value = "YS",
         fluidRow(
           column(4,selectInput("portfolio_index", label = "加入指数", 
                  choices = {
                    temp = t(IndexCategory)
                    #隐式返回要求的list数据
                    as.list(temp[2,])
                  }, multiple = TRUE)),
           column(3,dateRangeInput("portfolio_dates", "日期范围",start = "2015-10-01", end = as.character(Sys.Date())))
        ),
        fluidRow(
          column(width = 12,plotOutput("portfolio_RandV_plot", height = "700px"))
        )
      )
      ,type = "pills")
    , width = layout_right_width)
  )
))


