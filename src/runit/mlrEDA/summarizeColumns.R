testSummarizeColumns = function(){
  data = data.frame(x=as.integer(1:5), y=as.character(c("a","b", "c", "d", "e")), 
          z=c(TRUE, TRUE, TRUE, FALSE, FALSE), stringsAsFactors=FALSE)
     
  data2 = summarizeColumns(data)
  checkEquals(dim(data2), c(ncol(data), 9))
  checkEquals(data2$na, c(0,0,0))
  checkEquals(data2$mean, c(3, NA,NA))
  
  data= data.frame(x=as.Date(c("2011-01-01", "2011-01-02", "2011-01-03")), y=c(1:3), 
        z=as.Date(c("2011-01-01", "2011-01-02", "2011-01-03")))
  data3 = summarizeColumns(data)
  data4 = summarizeColumns(data, "2000-01-01")
  data5 = summarizeColumns(data, c(x=("2001-01-01"), z=("2002-01-01")))
  checkEquals(data3$disp, data4$disp)
  checkEquals(data3[2,], data4[2,])
  checkEquals(data4, data5)
}