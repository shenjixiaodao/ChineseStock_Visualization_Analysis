#计算data 中的收益率
getYeildRateData = function(data, colname){
  temp = (data[,3] - data[,1]) / data[,1] * 100
  colnames(temp) = colname
  return(temp)
}
#返回 data 中指定的列为col， 被命名为 colname
getFSYieldData = function(data, col,colname){
  temp = data[,col]
  temp[,1] = as.numeric(str_extract(temp[,1], "\\d+[.]\\d+"))
  colnames(temp) = colname
  return(temp)
}