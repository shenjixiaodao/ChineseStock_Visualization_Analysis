library(gtable)
library(grid)
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

#个股面板输入
stockPanel_InputValidate = function(input, output, session){
  key = str_trim(input$symb)
  stockid = NULL
  #搜索关键词不能为空，字符中间不能有空格
  if(str_length(key) > 0 & is.na(str_extract(key,"\\s"))){
    temp = searchXQStock(key)
    if(nrow(temp)==1)
      stockid = temp[1,1]
    output$hint = renderTable({temp})
  }else
    output$hint = renderTable({})
  
  if(is.null(stockid)){
    #清空 基指标 选择框
    updateSelectInput(session,"base_index",choices = {list()}, selected = NULL)
  }
  return(stockid)
}

#涨幅航～财务 绘图
FF_plot = function(data, indic1, indic2, legend.position = "bottom"){
  temp1 = as.data.frame(data[indic1])
  temp1$date = rownames(temp1)
  temp1 = melt(temp1, "date")
  temp2 = as.data.frame(data[indic2])
  temp2$date = rownames(temp2)
  temp2 = melt(temp2, "date")
  temp1 = cbind(temp1, temp2)[,c(1,2,3,6)]
  colnames(temp1) = c("date","variable",indic1,indic2)
  g = ggplot(data = temp1, aes(x=temp1[,indic1], y=temp1[,indic2], 
                               col = as.factor(str_extract(variable, "\\d{6}")), group = as.factor(variable)))
  g + geom_point() + xlab(indic1) + ylab(indic2) + ggtitle(paste("fluctuate ~ ",indic1," + ",indic2,sep = "")) + theme(
    legend.position = legend.position,
    legend.title = element_blank(),
    strip.text = element_text(size = 20, face = "bold"),
    axis.text = element_text(size = 15),
    strip.background = element_rect(fill = "transparent"),
    legend.text = element_text(size = 15)
  )
}

#涨幅 and 交易量
FandV_plot = function(data){
  grid.newpage()
  
  # two plots
  p1 <- ggplot(data, aes(x = date, y = rate, group = as.factor(group), col = group)) 
  p1<- p1 + geom_point(pch = 1) + geom_line() + theme_bw() %+replace% theme(
      legend.position = "top"
    )
  p2 <- ggplot(data, aes(x = date, y = volume, group = group, col = group)) + geom_point(pch = 2) + geom_line() + theme_bw() %+replace% 
    theme(panel.background = element_rect(fill = NA))
  
  # extract gtable
  g1 <- ggplot_gtable(ggplot_build(p1))
  g2 <- ggplot_gtable(ggplot_build(p2))
  
  # overlap the panel of 2nd plot on that of 1st plot
  pp <- c(subset(g1$layout, name == "panel", se = t:r))
  g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                       pp$l, pp$b, pp$l)
  
  # axis tweaks
  ia <- which(g2$layout$name == "axis-l")
  ga <- g2$grobs[[ia]]
  ax <- ga$children[[2]]
  ax$widths <- rev(ax$widths)
  ax$grobs <- rev(ax$grobs)
  ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
  g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
  g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
  
  # draw it
  grid.draw(g)
}



