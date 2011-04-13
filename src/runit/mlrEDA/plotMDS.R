testPlotMDS = function() {
  require("cluster")
  plotMDS(iris, "Species")
  plotMDS(iris, "Species", exclude=c("Sepal.Length", "Sepal.Width"))
  checkException(plotMDS(iris, "Species", exclude=c("Sepal.Length", "Sepal.Width", "Petal.Length")))
  
  plotMDS(BostonHousing, "medv")
}
