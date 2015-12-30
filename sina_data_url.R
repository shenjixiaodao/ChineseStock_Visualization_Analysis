#复权历史交易信息页面
RestorationofRight_url = function(request_parameter, stockid){
  return(paste("http://vip.stock.finance.sina.com.cn/corp/go.php/vMS_FuQuanMarketHistory/stockid/",
               stockid,".phtml?",request_parameter,sep = ""))
}
#指数历数据
IndexRecord_url = function(request_parameter, indexid){
  return(paste("http://vip.stock.finance.sina.com.cn/corp/go.php/vMS_MarketHistory/stockid/",
               indexid,"/type/S.phtml?",request_parameter,sep = ""))
}


#财务摘要
FinanceSummary_url = function(stockid){
  return(paste("http://vip.stock.finance.sina.com.cn/corp/go.php/vFD_FinanceSummary/stockid/",
               stockid,"/displaytype/4.phtml", sep = ""))
}

#板块分类
IndustryCategory_url = "http://money.finance.sina.com.cn/d/api/openapi_proxy.php/?__s=[[%22bkshy%22,%22%22,0]]&callback=FDC_DC.theTableData"

#指数
Index_url = "http://money.finance.sina.com.cn/d/api/openapi_proxy.php/?__s=[[%22hq%22,%22dpzs%22,%22%22,0,1,40]]&callback=FDC_DC.theTableData"

#IndustryCategory_code = "new_dzxx"

IC_Stocks_url = function(IndustryCategory_code){
  pagecount = 1
  recordCount = 400#一次把全部取完
  return(paste("http://money.finance.sina.com.cn/d/api/openapi_proxy.php/?__s=[[%22bkshy_node%22,%22",
        IndustryCategory_code,"%22,%22%22,0,",
        pagecount,",",recordCount,"]]&callback=FDC_DC.theTableData",sep = ""))
}

#个股查询url
Stock_Search_url = function(key){
  return(paste("http://suggest3.sinajs.cn/suggest/type=&key=",key,"&name=suggestdata_",
               as.integer(Sys.time()), sep = ""))
}
#查询个股的历史分价
stock_price_history_url = function(symbol, startDate, endDate){
  startDate = as.Date(startDate)
  endDate = as.Date(endDate)
  return(list(
    price=paste0("http://market.finance.sina.com.cn/pricehis.php?symbol=",symbol,"&startdate=",
          startDate,"&enddate=",endDate),
    UpDown=paste0("http://hq.sinajs.cn/list=",symbol)))
}




