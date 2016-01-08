library(jsonlite)
library(RCurl)
source("xueqiu_data_url.R")
#============== request header ==============
Cookie = NULL
User_Agent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/43.0.2357.124 Safari/537.36"
Cookies = function(){
  #获取 雪球 的cookie
  if(!is.null(Cookie))
    return(Cookie)
  Cookie <<- "bid=e7c77eae1047f602a9e269c50faa4150_iijo0oda; __utmt=1; __utma=1.315434125.1450836471.1450836471.1450923913.2; __utmb=1.8.10.1450923913; __utmc=1; __utmz=1.1450923913.2.2.utmcsr=baidu|utmccn=(organic)|utmcmd=organic; Hm_lvt_1db88642e346389874251b5a1eded6e3=1449113255,1449124685,1450706133,1450923913; Hm_lpvt_1db88642e346389874251b5a1eded6e3=1450926464"
  #解析雪球的 response header
  h = basicHeaderGatherer()
  getURI("http://xueqiu.com/", headerfunction = h$update)
  setCookies = h$value()[names(h$value())=="Set-Cookie"]
  Cookie <<- paste(Cookie,str_split(setCookies, pattern = ";")[[1]][1],
               str_split(setCookies, pattern = ";")[[2]][1],
               str_split(setCookies, pattern = ";")[[3]][1],sep = ";")
  return(Cookie)
}
#========== 初始化数据
xueqiu_YieldIndic_field = read.table("xueqiu_field_description", sep = ",",colClasses = "character",
                                     comment.char = "#",header = FALSE, row.names = 2)

#========= 处理函数
#解析个股收益指标
getXQStockYieldIndic = function(stockid, type){
  #使用rcurl请求json
  temp = getURL(XUEQIU_yieldIndex_url(stockid, type), useragent = User_Agent,cookie = Cookies())
  #使用jsonlite解析json
  temp = fromJSON(temp, simplifyVector = FALSE)[["tqSkYieldindic"]]
  temp = lapply(temp , FUN = function(x){
    if(is.null(x)) 
      return("")
    else
      return(x)
  })
  temp = data.frame(as.vector(temp[xueqiu_YieldIndic_field[,1]]), row.names = paste(type,stockid,sep = ""))
  colnames(temp) = rownames(xueqiu_YieldIndic_field)
  return(as.data.frame(t(temp)))
}

#解析个股的 follower count
getXQStockFollowers = function(stockid, type, count = 0){
  temp = getURL(XUEQIU_stock_followers_url(stockid, type, count), useragent = User_Agent,cookie = Cookies())
  fromJSON(temp)
}

#=========================== 股票查询
searchXQStock = function(code){
  temp = getURL(XUEQIU_StockSearch_url(code), useragent = User_Agent,cookie = Cookies())
  temp = fromJSON(temp)[["stocks"]]
  if(length(temp) > 0){
    colnames(temp) = c("代码","名称")
    return(temp[,c(1,2)])
  }else
    return(data.frame())
}

#=========================== 获取关注股票的 热门用户
getXQStockHotFollowers = function(symbol, top, start = 0){
  temp = getURL(XUEQIU_hotFollowers_url(symbol, top), useragent = User_Agent,cookie = Cookies())
  temp = fromJSON(temp)
  field = c("id","followers_count","friends_count")
  temp[, field]
  #temp$id = as.character(temp$id)
  #cbind(temp,Reduce(rbind, x = lapply(temp[,"id"], getXQFollowersPortfolio)))
}
 #雪球组合
getXQFollowersPortfolio = function(userid){
  temp = getURL(XUEQIU_portfolio_url(userid), useragent = User_Agent,cookie = Cookies())
  temp = fromJSON(temp)[["list"]]
  #获取组合收益情况
  if(length(temp) == 0)
    return(list(geomean = -1, ratio = -1, net_value = -1))
  net_value = temp[,"net_value"]
  #计算收益表现 收益几何平均值、方差、几何平均值／方差
  len = length(net_value)
  geomean = format((prod(net_value))^(1/len) - 1, digits = 3)
  ratio = format(sum((net_value-1)) / len / sd(net_value), digits = 3)
  list(geomean = geomean, ratio = ratio, net_value = net_value)
}

#===========================  雪球组合
#获取组合
getXQPortfolios = function(page = 1){
  temp = getURL(XUEQIU_portfilios_url(page), useragent = User_Agent,cookie = Cookies())
  temp = fromJSON(temp)[["list"]]
  field = c("symbol","created_at","updated_at","follower_count","last_rb_id")
  temp[,field]
}

#获取组合仓位配置 
getXQPortfolioConfiguration = function(rb_id){
  temp = getURL(XUEQIU_portfolioConfiguration_url(rb_id), useragent = User_Agent,cookie = Cookies())
  temp = fromJSON(temp)[["rebalancing"]][["rebalancing_histories"]]
  field = c("stock_name","stock_symbol","prev_weight_adjusted","target_weight","updated_at")
  temp = temp[,field]
  temp$updated_at = format(as.POSIXct(as.integer(temp$updated_at / 1000), origin = "1970-01-01"), format = "%Y-%m-%d")
  temp
}

#获取组合表现
getXQPortfoliosPerformance = function(symbol){
  temp = getURL(XUEQIU_performance_url(symbol), useragent = User_Agent,cookie = Cookies())
  temp = fromJSON(temp)[["sub_scores"]]
  #field = c("name","score","max_score")
  res = data.frame(x1=1,x2=1,x3=1,x4=1,x5=1)
  colnames(res) = temp[,"name"]
  res[1,] = format(temp[,"score"] / temp[,"max_score"] * 100, nsmall = 0)
  res
  #data.frame(name = temp[,"name"], score = temp[,"score"] / temp[,"max_score"])
}



