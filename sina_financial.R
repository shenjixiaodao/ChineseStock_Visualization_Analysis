#
library(rvest)
library(jsonlite)
source("sina_data_url.R")
#============================复权数据==================================
getPeriodHistoryPrice = function(startDate, endDate, stockid, parseFun){
  startDate = as.Date(startDate)
  endDate = as.Date(endDate)
  #判断startDate和endDate， 大小关系，是否在不同的季度
  if(startDate >= endDate)
    return ("开始时间必须小于结束时间")
  yq1 = as.yearqtr(startDate)
  yq2 = as.yearqtr(endDate)
  
  data = parseFun(format(yq1, format = "year=%Y&jidu=%q"), stockid)
  while(yq1!=yq2){
    yq1 = yq1 + 0.25#增加一个季度
    data = rbind(data,parseFun(format(yq1, format = "year=%Y&jidu=%q"), stockid))
  }
    
  #截取大于范围内的
  index = rownames(data)
  data = data[startDate <= index  & endDate >= index,]
  return (xts(data, order.by = as.Date(rownames(data))))
}
#========
#获取个股复权数据
getRestorationofRightPrice = function(request_parameter, stockid){
  #获取时间中的年份和季度信息
  #request_parameter = format(as.yearqtr(date), format = "year=%Y&jidu=%q")
  #url = paste(RestorationofRight_url,stockid,".phtml?",request_parameter,sep = "")
  webpage = read_html(RestorationofRight_url(request_parameter,stockid))#请求网页
  return (procFundHoldSharesTable(webpage))
}
#========
#获取指数历史数据
getIndexHistory = function(request_parameter, indexid){
  #获取时间中的年份和季度信息
  webpage = read_html(IndexRecord_url(request_parameter,indexid))#请求网页
  return (procFundHoldSharesTable(webpage))
}
#处理"table#FundHoldSharesTable"的数据
procFundHoldSharesTable = function(webpage){
  #解析网页中的FundHoldSharesTable数据
  data_table = (webpage %>% html_node("table#FundHoldSharesTable") %>% html_table())
  colnames(data_table) = data_table[1,]#数据的第一行为表头
  data_table = data_table[-1,]#去掉第一行
  rownames(data_table) = data_table[,1]#将时间信息转成行索引
  data_table = data_table[,-1]#去掉第一列
  for(i in 1:ncol(data_table)){
    #将数字字符串转成数字
    data_table[,i] = as.numeric(data_table[,i])
  }
  return(data_table)
}

#============================板块股票数据==================================
getIndustralCategoryStocks = function(url){
  #获取数据的json主体
  temp = fromJSON(str_extract_all(read_html(url), "\\[.+\\]")[[1]][2], 
                  simplifyDataFrame = data.frame)[[1]]
  
  colnames(temp$items) = temp$fields[1:ncol(temp$items)]
  return (t(as.data.frame(temp$items,row.names = temp$items[,3])[,1:3]))
}


#============================财务摘要数据==================================
#返回结果为  matrix
getFinanceSummary = function(url){
  webpage = read_html(url)#请求网页
  #解析网页中的季度财务摘要数据, 一次一个网页中会记录 4个季度数据
  data_table = (webpage %>% html_node("table#FundHoldSharesTable") %>% html_table(fill = TRUE))
  data_table = data_table[,1:2]#只取有效数据
  #每个季度的财务指标行数 : 12行有效数据, 1行空数据
  quartercount  = 4
  rowcount = 13
  # i = 1
  res = data_table[1:rowcount, ]
  #将单个季度财务概要转换成单条记录
  res = t(res[1:rowcount-1,1:2])
  #将列名变换可识别明
  colnames(res) = res[1,]
  res = res[-1,]#删除第一行
  for( i in 2:quartercount){
    rowindex = rowcount * i
    temp = data_table[rowindex-rowcount+1:rowindex, ]
    temp = t(temp[1:rowcount-1,1:2])
    colnames(temp) = temp[1,]
    temp = temp[-1,]
    res = rbind(res, temp)
  }
  #rownames(res) = rep(colnames(data_table)[1], quartercount)
  #转成 xts 对象, 并去掉时间数据列
  res = xts(res, order.by = as.Date(res[,1]))[,-1]
  return (res)
}







