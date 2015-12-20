#
library(rvest)
source("sina_data_url.R")
getPeriodRestorationofRightPrice = function(startDate, endDate, stockid){
  #判断startDate和endDate， 大小关系，是否在不通的季度
  
}
#处理获取指定年份和季度的复权数据
getRestorationofRightPrice = function(date, stockid){
  #获取时间中的年份和季度信息
  request_parameter = format(as.yearqtr(as.Date(date)), format = "year=%Y&jidu=%q")
  url = paste(RestorationofRight_url,stockid,".phtml?",request_parameter,sep = "")
  print(url)
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

stockid = "600030"

