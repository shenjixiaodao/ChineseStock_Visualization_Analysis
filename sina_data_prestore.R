# 预存数据
#library(stringr)
#library(jsonlite)
#library(rvest)
source("datafiles_name.R")
#============================行业类别数据======================================
prestore_industral_category = function(url, filename){
  #获取数据的json主体
  temp = fromJSON(str_extract_all(read_html(url), "\\[.+\\]")[[1]][2], 
                  simplifyDataFrame = data.frame)[[1]]
  colnames(temp$items) = temp$fields
  #只存前4列的值, 每次写入都是覆写
  write.table(temp$items[,1:4],filename, sep = "\t", 
              append = FALSE, row.names = FALSE, col.names = TRUE, quote = FALSE)
}
prestore_industral_category(IndustryCategory_url, IndustryCategory_file)

#============================指数数据======================================
prestore_index_category = function(url, filename){
  #获取数据的json主体
  temp = fromJSON(str_extract_all(read_html(url), "\\[.+\\]")[[1]][2], 
                  simplifyDataFrame = data.frame)[[1]]
  colnames(temp$items) = temp$fields
  #只存前3列的值, 每次写入都是覆写
  write.table(temp$items[,1:3],filename, sep = "\t", 
              append = FALSE, row.names = FALSE, col.names = TRUE, quote = FALSE)
}
prestore_index_category(Index_url, IndexCategory_file)




