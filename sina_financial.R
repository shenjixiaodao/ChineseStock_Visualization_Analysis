#
library(rvest)
source("sina_data_url.R")
#============================复权数据==================================
getPeriodRestorationofRightPrice = function(startDate, endDate, stockid){
  startDate = as.Date(startDate)
  endDate = as.Date(endDate)
  #判断startDate和endDate， 大小关系，是否在不同的季度
  if(startDate >= endDate)
    return ("开始时间必须小于结束时间")
  yq1 = format(as.yearqtr(startDate), format = "year=%Y&jidu=%q")
  yq2 = format(as.yearqtr(endDate), format = "year=%Y&jidu=%q")
  
  if(yq1!=yq2){
    data = getRestorationofRightPrice(yq1, stockid)
    data = rbind(data,getRestorationofRightPrice(yq2, stockid))
  }else
    data = getRestorationofRightPrice(yq1, stockid)
    
  #截取大于范围内的
  index = rownames(data)
  data = data[startDate <= index  & endDate >= index,]
  return (xts(data, order.by = as.Date(rownames(data))))
}
#处理获取指定年份和季度的复权数据
getRestorationofRightPrice = function(request_parameter, stockid){
  #获取时间中的年份和季度信息
  #request_parameter = format(as.yearqtr(date), format = "year=%Y&jidu=%q")
  url = paste(RestorationofRight_url,stockid,".phtml?",request_parameter,sep = "")
  webpage = read_html(url)#请求网页
  #解析网页中的历史复权数据
  data_table = (webpage %>% html_node("table#FundHoldSharesTable") %>% html_table())
  colnames(data_table) = data_table[1,]#数据的第一行为表头
  data_table = data_table[-1,]#去掉第一行
  rownames(data_table) = data_table[,1]#将时间信息转成行索引
  data_table = data_table[,-1]#去掉第一列
  for(i in 1:ncol(data_table)){
    #将数字字符串转成数字
    data_table[,i] = as.numeric(data_table[,i])
  }
  return (data_table)
}


#============================板块股票数据==================================
getIndustralCategoryStocks = function(url){
  #获取数据的json主体
  temp = fromJSON(str_extract_all(read_html(url), "\\[.+\\]")[[1]][2], 
                  simplifyDataFrame = data.frame)[[1]]
  
  colnames(temp$items) = temp$fields
  return (temp)
}

#


