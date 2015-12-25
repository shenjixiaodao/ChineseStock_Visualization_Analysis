# server.R

library(quantmod)
library(stringr)

#source("helpers.R")
#library(rvest)
source("sina_financial.R")
source("helpers.R")
source("xueqiu_financial.R")

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
      print(paste("have base index ", str_extract(colnames(data),"\\d{6}"), sep = ","))
      #保证data中只有选择的 指数 和 个股
      data <<- data[, c("个股",paste(rownames(IndexCategory[IndexCategory$code %in% input$base_index[have_base_index],]),
                            input$base_index[have_base_index], sep = "."))]
      
      #将未加载的 指数 数据加入请求队列 , 在列名中提取 股票id
      new_base_index = input$base_index[!have_base_index]
      print(paste("new base index ", new_base_index, sep = ","))
      if(length(new_base_index) > 0){
        temp = getPeriodHistoryPrice(input$dates[1],input$dates[2],new_base_index,getIndexHistory)
        temp = getYeildRateData(temp, paste(rownames(IndexCategory[IndexCategory$code %in% new_base_index,]),
                                            new_base_index,sep = "."))
        data <<- cbind(data,temp)
      }
      
      print(colnames(data))
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
  FS_profit_data = NULL
  FS_asset_data = NULL
  FS_cash_data = NULL
  FS_reserve_data = NULL
  FS_industray_category = ""
  FS_profit_datainput <- reactive({
    if(input$FS_IndustryCategory != FS_industray_category){
      #print(input$FS_IndustryCategory)
      #更新 行业类别内的个股 选择框
      updateSelectInput(session,"FS_IC_Stocks",choices = {
          temp = getIndustralCategoryStocks(IC_Stocks_url(input$FS_IndustryCategory))
          as.list(temp[2,])
        }, selected = NULL
      )
      FS_profit_data <<- NULL
      FS_asset_data <<- NULL
      FS_cash_data <<- NULL
      FS_reserve_data <<- NULL
      FS_industray_category <<- input$FS_IndustryCategory
      #return(NULL)
    }
    
    if(!is.null(input$FS_IC_Stocks)){
      have_stocks = input$FS_IC_Stocks %in% str_extract(colnames(FS_profit_data),"\\d{6}")
      #保证FS_profit_data中只有选择的
      FS_profit_data <<- FS_profit_data[, paste("X",input$FS_IC_Stocks[have_stocks], sep = "")]
      FS_asset_data <<- FS_asset_data[, paste("X",input$FS_IC_Stocks[have_stocks], sep = "")]
      FS_cash_data <<- FS_cash_data[, paste("X",input$FS_IC_Stocks[have_stocks], sep = "")]
      FS_reserve_data <<- FS_reserve_data[, paste("X",input$FS_IC_Stocks[have_stocks], sep = "")]
      #将未加载的 指数 数据加入请求队列 , 在列名中提取 股票id
      new_stocks = input$FS_IC_Stocks[!have_stocks]
      if( length(new_stocks) > 0){
        for(new_stock in new_stocks){
          #当有新的被选中时
          temp = getFinanceSummary(FinanceSummary_url(new_stock))
          temp_asset = getFSYieldData(temp,1,paste("X",new_stock, sep = ""))#取每股净资产
          temp_profit = getFSYieldData(temp,2,paste("X",new_stock, sep = ""))#取每股收益
          temp_cash = getFSYieldData(temp,3,paste("X",new_stock, sep = ""))#取每股现金含量
          temp_reserve = getFSYieldData(temp,4,paste("X",new_stock, sep = ""))#取每股公积金
          if(!is.null(FS_profit_data)){
            FS_profit_data <<- cbind(FS_profit_data,temp_profit)
            FS_asset_data <<- cbind(FS_asset_data, temp_asset)
            FS_cash_data <<- cbind(FS_cash_data, temp_cash)
            FS_reserve_data <<- cbind(FS_reserve_data, temp_reserve)
          }else{
            FS_profit_data <<- temp_profit
            FS_asset_data <<- temp_asset
            FS_cash_data <<- temp_cash
            FS_reserve_data <<- temp_reserve
          }
        }
      }
      
      #暂停一会儿
      return(list(asset=FS_asset_data,profit=FS_profit_data,
                  cash=FS_cash_data, reserve=FS_reserve_data))
    }
    return(NULL)
  })
  
  output$FS_asset_dygraph <- renderDygraph({
    data = FS_profit_datainput()
    #lapply(data, function(x){print(names(x))})
    #
    output$FS_profit_dygraph <- renderDygraph({
      if(!is.null(data))
        dygraph(data$profit, xlab = index(data$cash),main = "每股收益")
    })
    #
    output$FS_cash_dygraph <- renderDygraph({
      if(!is.null(data))
        dygraph(data$cash, xlab = index(data$cash),main = "每股现金含量")
    })
    #
    output$FS_reserve_dygraph <- renderDygraph({
      if(!is.null(data))
        dygraph(data$reserve,main = "每股公积金")
    })
    #
    if(!is.null(data))
      dygraph(data$asset,main = "每股净资产")
    else
      NULL
  })
  #======================= FS END ========================
  
  #=============== YeildIndic
  YS_data = NULL
  YS_industray_category = ""
  YS_datainput <- reactive({
    if(input$YS_IndustryCategory != YS_industray_category){
      print(input$YS_IndustryCategory)
      #更新 行业类别内的个股 选择框
      updateSelectInput(session,"YS_IC_Stocks",choices = {
        temp = getIndustralCategoryStocks(IC_Stocks_url(input$YS_IndustryCategory))
        as.list(temp[1,])
      }, selected = NULL
      )
      YS_data <<- NULL
      YS_industray_category <<- input$YS_IndustryCategory
    }
    
    if(!is.null(input$YS_IC_Stocks)){
      #将已经加载的 数据 , 在列名中提取 股票id
      have_stocks = input$YS_IC_Stocks %in% colnames(YS_data) #str_extract(colnames(YS_data),"s[zh]{1}\\d{6}")
      #保证YS_data中只有选择的
      YS_data <<- YS_data[, input$YS_IC_Stocks[have_stocks]]
      if(!is.data.frame(YS_data) & !is.null(YS_data)){
        YS_data <<- as.data.frame(YS_data)
        colnames(YS_data) <<- input$YS_IC_Stocks[have_stocks]
        print(YS_data)
      }
        
      new_stock = input$YS_IC_Stocks[!have_stocks]
      if( length(new_stock) > 0){
        #当有新的被选中时
        #print(new_stock)
        temp = getXQStockYieldIndic(substr(new_stock,3,8),substr(new_stock,1,2))
        
        if(!is.null(YS_data)){
          YS_data <<- cbind(YS_data,temp)
        }else
          YS_data <<- temp
      }
      
      #暂停一会儿
      return(YS_data)
    }
    return(NULL)
  })
  
  output$YeildIndic <- renderTable({
    YS_datainput()
    #getXQStockYieldIndic("000001","sz")
  })
})



