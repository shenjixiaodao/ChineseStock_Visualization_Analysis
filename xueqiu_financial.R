library(jsonlite)
library(RCurl)
#============== request header ==============
Cookie="s=22xk126o1o; xq_a_token=f0580d847da88a23a3a2c072360234a225411f5d; xq_r_token=6343d35414132b6423de06f9eba9b3fc741fc39e; __utma=1.1484259619.1450748076.1450748076.1450748076.1; __utmb=1.13.10.1450748076; __utmc=1; __utmz=1.1450748076.1.1.utmcsr=(direct)|utmccn=(direct)|utmcmd=(none); Hm_lvt_1db88642e346389874251b5a1eded6e3=1449057079,1449113255,1449124685,1450706133; Hm_lpvt_1db88642e346389874251b5a1eded6e3=1450748417"
User_Agent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/43.0.2357.124 Safari/537.36"

#解析个股收益指标
getXQStockYieldIndic = function(stockid, type){
  #使用rcurl请求json
  temp = getURL(XUEQIU_yieldIndex_url(stockid, type), useragent = User_Agent,cookie = Cookie)
  #使用jsonlite解析json
  temp = fromJSON(temp, simplifyVector = FALSE)
  return(temp[["tqSkYieldindic"]])
}

#解析个股的 follower count
getXQStockFollowers = function(stockid, type){
  temp = getURL(XUEQIU_stock_followers_url(stockid, type), useragent = User_Agent,cookie = Cookie)
}