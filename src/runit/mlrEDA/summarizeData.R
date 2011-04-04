testSummarizeData = function() {
  data = iris
  
  data2 = summarizeData(data, target="Species", large=5)
  checkEquals(data2["obs"], c(obs=150))
  checkEquals(data2["dim"], c(dim=4))
  checkEquals(data2["num"], c(num=4))
  checkEquals(data2["int"], c(int=0))
  checkEquals(data2["fact"], c(fact=0))
  checkEquals(data2["large.row.max"],c(large.row.max=2))
  checkEquals(data2["large.col.max"],c(large.col.max=128))
  
  data3 = summarizeData(data, target="Sepal.Length", large=5, feat.perc=TRUE, class.perc=TRUE, large.perc=TRUE)
  checkEquals(data3["num"], c(num=0.75))
  checkEquals(data3["fact"], c(fact=0.25))
  checkEquals(data3["large.row.max"], c(large.row.max=0.5))
  
}