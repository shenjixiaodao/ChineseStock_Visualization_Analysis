# server.R

library(quantmod)
library(stringr)

#source("helpers.R")
#library(rvest)
source("sina_financial.R")
source("helpers.R")

shinyServer(function(input, output, session) {
  stockID_pattern = "\\d{6}"
  stockid = ""
  data = NULL
  start_date = "1990-10-01"
  end_date = "1990-10-01"
  dataInput <- reactive({
    if(start_date != input$dates[1] | end_date != input$dates[2] | stockid != input$symb){
      #==================================修改了个股状态，更新数据
      #更新 基指标 选择框
      updateSelectInput(session,"base_index",choices = {
        #读入行业类别数据
        temp = t(IndexCategory)
        #隐式返回要求的list数据
        as.list(temp[2,])
      }, selected = NULL)
      
      #修改全局环境data
      temp = getPeriodHistoryPrice(input$dates[1],input$dates[2],input$symb,getRestorationofRightPrice)
      data <<- getYeildRateData(temp, "个股")
      start_date <<- input$dates[1]#修改全局环境start_date
      end_date <<- input$dates[2]#修改全局环境end_date
      stockid <<- input$symb#修改当前内存中的个股
      
      print("change stock")
    }
    if(!is.null(data) & !is.null(input$base_index)){
      have_base_index = input$base_index %in% str_extract(colnames(data),"\\d{6}")
      #保证data中只有选择的 指数 和 个股
      data <<- data[, c("个股",paste(rownames(IndexCategory[IndexCategory$code==input$base_index[have_base_index],]),
                            input$base_index[have_base_index], sep = "-"))]
      #将未加载的 指数 数据加入请求队列 , 在列名中提取 股票id
      new_base_index = input$base_index[!have_base_index]
      if(length(new_base_index) > 0){
        temp = getPeriodHistoryPrice(input$dates[1],input$dates[2],new_base_index,getIndexHistory)
        temp = getYeildRateData(temp, paste(rownames(IndexCategory[IndexCategory$code==new_base_index,]),
                                            new_base_index,sep = "-"))
        data <<- cbind(data,temp)
      }
      #暂停一会儿
    }else
      data <<- data[,1]#保证当没有 指数 被选时只有个股
    
    return (data)
  })
  
  output$dygraph <- renderDygraph({
    if(length(grep(stockID_pattern, str_trim(input$symb))) == 0){
      #清空 基指标 选择框
      updateSelectInput(session,"base_index",choices = {list()}, selected = NULL)
      return()
    }
    #展示复权股价
    dygraph(dataInput(),main = "日波动率")
  })
  
  #=======================finance summary reactive
  FS_data = NULL
  FS_industray_category = ""
  FS_datainput <- reactive({
    if(input$FS_IndustryCategory != FS_industray_category){
      print(input$FS_IndustryCategory)
      #更新 行业类别内的个股 选择框
      updateSelectInput(session,"FS_IC_Stocks",choices = {
          temp = getIndustralCategoryStocks(IC_Stocks_url(input$FS_IndustryCategory))
          as.list(temp[2,])
        }, selected = NULL
      )
      FS_data <<- NULL
      FS_industray_category <<- input$FS_IndustryCategory
    }
    
    if(!is.null(input$FS_IC_Stocks)){
      have_stocks = input$FS_IC_Stocks %in% str_extract(colnames(FS_data),"\\d{6}")
      #保证FS_data中只有选择的
      FS_data <<- FS_data[, paste("X",input$FS_IC_Stocks[have_stocks], sep = "")]
      #将未加载的 指数 数据加入请求队列 , 在列名中提取 股票id
      new_stock = input$FS_IC_Stocks[!have_stocks]
      if( length(new_stock) > 0){
        #当有新的被选中时
        temp = getFinanceSummary(FinanceSummary_url(new_stock))
        temp = getFSYieldData(temp,2,paste("X",new_stock, sep = ""))#取每股收益
        if(!is.null(FS_data)){
          FS_data <<- cbind(FS_data,temp)
        }else
          FS_data <<- temp
      }
      
      print(FS_data)
      #暂停一会儿
      return(FS_data)
    }
    return(NULL)
  })
  output$FS_dygraph <- renderDygraph({
    data = FS_datainput()
    if(!is.null(data))
      dygraph(data,main = "每股收益")
  })
  
})



