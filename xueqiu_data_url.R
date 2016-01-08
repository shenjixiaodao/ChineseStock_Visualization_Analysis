#个股网页信息，http://xueqiu.com/S/SZ300288
XUEQIU_stock_url = function(stockid, type){
  return(paste("http://xueqiu.com/S/",type,stockid,sep = ""))
}

#个股关注de球友
XUEQIU_stock_followers_url = function(stockid, type, count = 14){
  start = 0
  #count = 0
  timestamp = as.integer(Sys.time())#时间戳
  return(paste("http://xueqiu.com/recommend/pofriends.json?type=1&code=",toupper(type),
               stockid,"&start=",start,"&count=",count,"&_=",timestamp,sep = ""))
}

#股票收益指标，http://xueqiu.com/S/SZ300288/GPSYLZB
XUEQIU_yieldIndex_url = function(stockid, type){
  page = 1
  size = 14
  timestamp = as.integer(Sys.time())
  return(paste("http://xueqiu.com/stock/f10/yieldindic.json?symbol=",type,
               stockid,"&page=",page,"&size=",size,"&_=",timestamp,sep = ""))
}

#球友组合表现，http://xueqiu.com/9820690652
XUEQIU_user_url = function(userid){
  return(paste("http://xueqiu.com/",userid,sep = ""))
}

#股票搜索， http://xueqiu.com/stock/search.json?code=00&size=5&key=47bce5c74f
XUEQIU_StockSearch_url = function(code){
  size = 5
  return(paste("http://xueqiu.com/stock/search.json?code=",code,"&size=",size,"&key=47bce5c74f",
               as.integer(Sys.time()), sep = ""))
}

#关注股票的 热门用户
#http://xueqiu.com/recommend/user/stock_hot_user.json?symbol=SH600228&start=5&count=6&_=1451720083530
XUEQIU_hotFollowers_url = function(symbol, top){
  start = 0
  return(paste0("http://xueqiu.com/recommend/user/stock_hot_user.json?symbol=",toupper(symbol),
                "&start=",start,"&count=",top,"&_=",as.integer(Sys.time())))
}

#球友的 组合
#http://xueqiu.com/cubes/list.json?user_id=1852792513&_=1451723177647\
XUEQIU_portfolio_url = function(userid){
  return(paste0("http://xueqiu.com/cubes/list.json?user_id=",userid,"&_=",as.integer(Sys.time())))
}




