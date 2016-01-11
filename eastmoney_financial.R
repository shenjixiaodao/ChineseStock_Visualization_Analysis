require(stringr)
require(jsonlite)
#获取热门股票 和 所有 上海和深圳的a股
getHotStockAndAllStock = function(){
  temp = read_html(getURL(AllAndHotStock_url()))
  #热门股票
  hotStock = temp %>% html_node("div.zhutibarlist") %>% html_nodes("a") %>% html_text()
  #上海a股
  shStocks = temp %>% html_nodes("div.ngbggulbody div:nth-child(1)") %>% html_nodes("a") %>% html_text()
  names(shStocks) = str_extract(shStocks, "\\d{6}")
  shStocks = shStocks[-which(is.na(names(shStocks)))]#去掉无效的个股
  #深圳a股
  szStocks = temp %>% html_nodes("div.ngbggulbody div:nth-child(3)") %>% html_nodes("a") %>% html_text()
  names(szStocks) = str_extract(szStocks, "\\d{6}")
  szStocks = szStocks[-which(is.na(names(szStocks)))]
  list(hotStock = hotStock, allStocks = shStocks, szStocks = szStocks)
}

##共同关注比例 top 5
getCommonTop5Stock = function(stockid){
  temp = fromJSON(CommonTop5_url(stockid))[["re"]]
  colnames(temp) = c("targetID", "targetName","targetFansCount","targetFanPercent")
  temp["targetFansCount"] = as.numeric(str_extract(temp["targetFansCount"], "[^%]+"))
  temp$sourceID = stockid
  list(links = temp, FansCount = as.numeric(temp[1,"targetFansCount"])/as.numeric(temp[1,"targetFanPercent"]))
}

stocks = getHotStockAndAllStock()
stocks = c(stocks$shStocks, stocks$szStocks)
size = replicate(length(stocks), 1)

stockid = names(stocks)[1]
temp = getCommonTop5Stock(stockid)
link = temp$links
size[which(names(stocks)==stockid)] = temp$FansCount
nodeID = unique(c(link$sourceID,link$targetID))
link$targetID = which(nodeID %in% link$targetID) - 1
link$sourceID = which(nodeID %in% link$sourceID) - 1

link$targetFanPercent = as.numeric(link$targetFanPercent)


forceNetwork(Links = link, Nodes = data.frame(nodeID = 0:length(nodeID), group = 1),
             Source = "sourceID", Target = "targetID",Group = "group",
             linkDistance = "targetFanPercent", NodeID = "nodeID", opacity = 0.8, zoom = TRUE)

