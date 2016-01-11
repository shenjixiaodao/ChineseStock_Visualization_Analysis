
AllAndHotStock_url = function(type = 1){
  return(paste0("http://guba.eastmoney.com/remenba.aspx?type=",type))
}

#http://guba.eastmoney.com/list,600000.html
Guba_url = function(stockid){
  return(paste0("http://guba.eastmoney.com/list,",stockid ,".html"))
}


#http://guba.eastmoney.com/catch_web.aspx?type=2&code=601766
CommonTop5_url = function(stockid){
  return(paste0("http://guba.eastmoney.com/catch_web.aspx?type=2&code=",stockid))
}