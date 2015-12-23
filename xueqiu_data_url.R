#个股网页信息，http://xueqiu.com/S/SZ300288
XUEQIU_stock_url = function(stockid, type){
  return(paste("http://xueqiu.com/S/",type,stockid,sep = ""))
}

#个股关注de球友
XUEQIU_stock_followers_url = function(stockid, type){
  start = 0
  count = 14
  timestamp = as.integer(Sys.time())#时间戳
  return(paste("http://xueqiu.com/recommend/pofriends.json?type=1&code=",type,
               stockid,"&start=",start,"&count=",count,"&_=",timestamp,sep = ""))
}

#股票收益指标，http://xueqiu.com/S/SZ300288/GPSYLZB
XUEQIU_yieldIndex_url = function(stockid, type){
  page = 1
  size = 4
  timestamp = as.integer(Sys.time())
  return(paste("http://xueqiu.com/stock/f10/yieldindic.json?symbol=",type,
               stockid,"&page=",page,"&size=",size,"&_=",timestamp,sep = ""))
}

#球友组合表现，http://xueqiu.com/9820690652
XUEQIU_user_url = function(userid){
  return(paste("http://xueqiu.com/",userid,sep = ""))
}

#webpage[["tqSkYieldindic"]][["beta52w"]]
