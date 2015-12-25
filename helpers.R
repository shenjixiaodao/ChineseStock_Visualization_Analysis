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

FS_Plot = function(data){
  #
  output$FS_profit_dygraph <- renderDygraph({
    if(!is.null(data))
      dygraph(data$profit, xlab = index(data$cash),main = "每股收益")
  })
  #
  output$FS_cash_dygraph <- renderDygraph({
    if(!is.null(data))
      dygraph(data$cash, xlab = index(data$cash),main = "每股现金含量")
  })
  #
  output$FS_reserve_dygraph <- renderDygraph({
    if(!is.null(data))
      dygraph(data$reserve,main = "每股公积金")
  })
  #
  output$FS_asset_dygraph <- renderDygraph({
    if(!is.null(data))
      dygraph(data$asset,main = "每股净资产")
  })

}