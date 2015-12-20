# 预存数据
#library(stringr)
#library(jsonlite)
#library(rvest)
prestore_industral_category = function(url, filename){
  #获取数据的json主体
  temp = fromJSON(str_extract_all(read_html(url), "\\[.+\\]")[[1]][2], 
                  simplifyDataFrame = data.frame)[[1]]
  colnames(temp$items) = temp$fields
  #只存前4列的值
  write.table(temp$items[,1:4],filename, sep = "\t", 
              append = TRUE, row.names = FALSE, col.names = TRUE, quote = FALSE)
}
