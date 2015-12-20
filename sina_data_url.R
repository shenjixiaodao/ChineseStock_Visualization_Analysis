#复权历史交易信息页面
RestorationofRight_url = "http://vip.stock.finance.sina.com.cn/corp/go.php/vMS_FuQuanMarketHistory/stockid/"

#财务摘要
FinanceSummary_url = function(stockid){
  return(paste("http://vip.stock.finance.sina.com.cn/corp/go.php/vFD_FinanceSummary/stockid/",
               stockid,"/displaytype/4.phtml", sep = ","))
}

#板块分类
IndustryCategory_url = "http://money.finance.sina.com.cn/d/api/openapi_proxy.php/?__s=[[%22bkshy%22,%22%22,0]]&callback=FDC_DC.theTableData"


#IndustryCategory_code = "new_dzxx"

IC_Stocks_url = function(IndustryCategory_code){
  pagecount = 1
  recordCount = 400#一次把全部取完
  return(paste("http://money.finance.sina.com.cn/d/api/openapi_proxy.php/?__s=[[%22bkshy_node%22,%22",
        IndustryCategory_code,"%22,%22%22,0,",
        pagecount,",",recordCount,"]]&callback=FDC_DC.theTableData",sep = ""))
}


