# server.R

library(quantmod)
library(stringr)
library(reshape2)
#library(lattice)
#library(parallel)
library(ggplot2)
#library(plotly)
#source("helpers.R")
#library(rvest)
source("sina_financial.R")
source("helpers.R")
source("xueqiu_financial.R")

shinyServer(function(input, output, session) {
  stockID_pattern = "\\d{6}"
  #================== 全局变量
  info = NULL
  
  #================== end
  
  stockid = ""
  data = NULL
  FandV_data = NULL
  start_date = "1990-10-01"
  end_date = "1990-10-01"
  dataInput <- reactive({
    input_stockid = stockPanel_InputValidate(input, output, session)
    if(is.null(input_stockid))
      return(NULL)
    #input_stockid = substr(input_stockid, 3, 8)
    
    if(start_date != input$dates[1] | end_date != input$dates[2] | stockid != input_stockid){
      #==================================修改了个股状态，更新数据
      #更新 基指标 选择框
      updateSelectInput(session,"base_index",choices = {
        temp = t(IndexCategory)
        #隐式返回要求的list数据
        as.list(temp[2,])
      }, selected = NULL)
      
      #修改全局环境data
      temp = getPeriodHistoryPrice(input$dates[1],input$dates[2],substr(input_stockid, 3, 8),getRestorationofRightPrice)
      data <<- getYeildRateData(temp, input$symb)#获取涨幅率数据
      FandV_data <<- cbind(data, temp[,5])#涨幅率和交易量
      
      start_date <<- input$dates[1]#修改全局环境start_date
      end_date <<- input$dates[2]#修改全局环境end_date
      stockid <<- input_stockid#修改当前内存中的个股
      
      print("change stock")
    }
    if(!is.null(data) & !is.null(input$base_index)){
      
      have_base_index = input$base_index %in% str_extract(colnames(data),"\\d{6}")
      #保证data中只有选择的 指数 和 个股
      data <<- data[, c(input$symb,paste(rownames(IndexCategory[IndexCategory$code %in% input$base_index[have_base_index],]),
                            input$base_index[have_base_index], sep = "."))]
      
      #将未加载的 指数 数据加入请求队列 , 在列名中提取 股票id
      new_base_index = input$base_index[!have_base_index]
      #print(paste("new base index ", new_base_index, sep = ","))
      if(length(new_base_index) > 0){
        temp = getPeriodHistoryPrice(input$dates[1],input$dates[2],new_base_index,getIndexHistory)
        temp = getYeildRateData(temp, paste(rownames(IndexCategory[IndexCategory$code %in% new_base_index,]),
                                            new_base_index,sep = "."))
        data <<- cbind(data,temp)
      }
      
      #print(colnames(data))
      #暂停一会儿
    }else
      data <<- data[,1]#保证当没有 指数 被选时只有个股
    
    return (data)
  })
  
  output$dygraph <- renderDygraph({
    
    #展示复权股价
    data = dataInput()
    if(!is.null(data)){
      cn = c("rate","volume(M)")
      temp = as.data.frame(FandV_data)
      colnames(temp) = cn
      temp[,cn[2]] = temp[,cn[2]] / 1000000
      temp$date = rownames(temp)
      
      
      output$RtoV_plot <- renderPlot({
        color = vapply(temp[,cn[1]], FUN = function(x){
          if(x>0) 
            return("red") 
          else 
            return("green")
        },"colorname")
        g = ggplot(temp, aes(x = rate, y = `volume(M)`)) + ggtitle("volume ~ rate")
        g + geom_point(col = color) + ylab("volume(M)") + xlab("rate") + theme(
          strip.background = element_rect(fill = "transparent")
        )
      })
      output$RandV_plot <- renderPlot({
        #temp$group = "1"#只有个股
        FandV_plot2(temp)
      })
      dygraph(data,main = paste(input$symb,"日波动率",sep = "-"))
    }
  })
  
  #=======================finance summary reactive
  FS_profit_data = NULL
  FS_asset_data = NULL
  FS_cash_data = NULL
  FS_reserve_data = NULL
  FS_industray_category = ""
  FS_profit_datainput <- reactive({
    if(input$Finance_IndustryCategory != FS_industray_category){
      #print(input$Finance_IndustryCategory)
      #更新 行业类别内的个股 选择框
      updateSelectInput(session,"Finance_IC_Stocks",choices = {
          temp = getIndustralCategoryStocks(IC_Stocks_url(input$Finance_IndustryCategory))
          as.list(temp[2,])
        }, selected = NULL
      )
      FS_profit_data <<- NULL
      FS_asset_data <<- NULL
      FS_cash_data <<- NULL
      FS_reserve_data <<- NULL
      FS_industray_category <<- input$Finance_IndustryCategory
      #return(NULL)
    }
    
    if(!is.null(input$Finance_IC_Stocks)){
      have_stocks = input$Finance_IC_Stocks %in% str_extract(colnames(FS_profit_data),"\\d{6}")
      #保证FS_profit_data中只有选择的
      FS_profit_data <<- FS_profit_data[, paste("X",input$Finance_IC_Stocks[have_stocks], sep = "")]
      FS_asset_data <<- FS_asset_data[, paste("X",input$Finance_IC_Stocks[have_stocks], sep = "")]
      FS_cash_data <<- FS_cash_data[, paste("X",input$Finance_IC_Stocks[have_stocks], sep = "")]
      FS_reserve_data <<- FS_reserve_data[, paste("X",input$Finance_IC_Stocks[have_stocks], sep = "")]
      #将未加载的 指数 数据加入请求队列 , 在列名中提取 股票id
      new_stocks = input$Finance_IC_Stocks[!have_stocks]
      if( length(new_stocks) > 0){
        for(new_stock in new_stocks){
          #当有新的被选中时, 请求新数据
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
  
  #output$FS_graphs <- renderDygraph({
  output$FS_graphs <- renderPlot({
    data = FS_profit_datainput()
    
    if(!is.null(data)){
      temp1 = data.frame()
      for( name in names(data)){
        temp = as.data.frame(data[name])
        temp$date = rownames(temp)
        temp = melt(temp, "date")
        temp$class = name
        temp1 = rbind(temp1,temp)
      }
     # xyplot(as.numeric(value) ~ as.factor(date) | as.factor(class), groups = variable, 
     #        data = temp1, type = "o", main = "asset per-share",xlab = "",ylab = "",
     #        layout=c(2,2))
      g = ggplot(data = temp1, aes(x=as.factor(date), y=as.numeric(value), 
            col = as.factor(str_extract(variable, "\\d{6}")), group = as.factor(variable)))
      g = g + geom_point() + geom_line() + facet_wrap(~class,scales="free_y")
      g + xlab("") + ylab("") + theme(
        legend.position = "left",
        legend.title = element_blank(),
        strip.text = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 15),
        strip.background = element_rect(fill = "transparent"),
        legend.text = element_text(size = 15)
      )
    }
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
  
  #==================  up and down ~ finance
  output$followers <- renderUI({
    data = dataInput()
    if(!is.null(data)){
      tc = getXQStockFollowers(substr(stockid, 3, 8),substr(stockid, 1, 2), 0)[["totalcount"]]#返回关注人数
      HTML(paste0("<a href=\"http://xueqiu.com/S/",stockid,"/follows\"><label 
                  id=\"follower\" class=\"shiny-text-output\">",tc,"</label></a>人关注"))
    }
    else
      NULL
  })
  
  output$UDF_UD_1_plot <- renderPlot({
    data = FS_profit_datainput()
    if(!is.null(data)){
      
      output$UDF_UD_2_plot <- renderPlot({FF_plot(data, names(data)[1], names(data)[3])})
      output$UDF_UD_3_plot <- renderPlot({FF_plot(data, names(data)[1], names(data)[4])})
      output$UDF_UD_4_plot <- renderPlot({FF_plot(data, names(data)[2], names(data)[3])})
      output$UDF_UD_5_plot <- renderPlot({FF_plot(data, names(data)[2], names(data)[4])})
      output$UDF_UD_6_plot <- renderPlot({FF_plot(data, names(data)[3], names(data)[4])})
      FF_plot(data, names(data)[1], names(data)[2])
      #plot_ly(temp, x = asset, y = profit, text = paste("date: ", date),
      #        mode = "markers", color = carat, size = carat)
    }else
      NULL
  })
  
  #====================  portfolio_index
  PI_rate_data = NULL
  PI_volume_data = NULL
  PI_start_date = "1990-10-01"
  PI_end_date = "1990-10-01"
  PI_Input <- reactive({
    if(PI_start_date != input$portfolio_dates[1] | PI_end_date != input$portfolio_dates[2]){
      #==================================修改了个股状态，更新数据
      
      #修改全局环境data
      PI_start_date <<- input$portfolio_dates[1]#修改全局环境start_date
      PI_end_date <<- input$portfolio_dates[2]#修改全局环境end_date
      PI_rate_data <<- NULL
      PI_volume_data <<- NULL
    }
    if(!is.null(input$portfolio_index)){
      have_base_index = input$portfolio_index %in% str_extract(colnames(PI_rate_data),"\\d{6}")
      #保证data中只有选择的 指数 和 个股
      PI_rate_data <<- PI_rate_data[, c(paste("X", input$portfolio_index[have_base_index], sep = "."))]
      
      #将未加载的 指数 数据加入请求队列 , 在列名中提取 股票id
      new_base_index = input$portfolio_index[!have_base_index]
      if(length(new_base_index) > 0){
        colname = paste("X", new_base_index,sep = ".")
        temp = getPeriodHistoryPrice(input$portfolio_dates[1],input$portfolio_dates[2], new_base_index, getIndexHistory)
        colnames(temp)[5] = colname
        PI_volume_data <<- cbind(PI_volume_data, temp[,5])#涨幅率和交易量
        
        temp = getYeildRateData(temp, colname)
        PI_rate_data <<- cbind(PI_rate_data,temp)
      }
      #暂停一会儿
      
      return (list(PI_rate_data=PI_rate_data, PI_volume_data=PI_volume_data))
    }
    return(NULL)
  })
  
  output$portfolio_RandV_plot <- renderPlot({
    data = PI_Input()
    if(!is.null(data)){
      plots = list()
      cn = c("rate","volume(B)")
      for(var in colnames(data[["PI_rate_data"]])){
        #生成多个绘图对象
        temp = as.data.frame(cbind(data[["PI_rate_data"]][,var], data[["PI_volume_data"]][,var]))
        colnames(temp) = cn
        temp[,cn[2]] = temp[,cn[2]] / 1000000000
        temp$date = rownames(temp)
        
        plots[[var]] = FandV_plot2(temp, var, cn) + theme(
          #没增加一个指数，面板标题字体大小减1
          strip.text = element_text(size = 15 - ncol(data[["PI_rate_data"]]), face = "bold")
        )
      }
      #绘制多个ggplot对象
      multiplot(plotlist = plots, cols = 1)
    }else
      NULL
  })
})



