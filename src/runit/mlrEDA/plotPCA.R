testPlotPCA = function() {
  plotPCA(iris, "Species", scale=TRUE)
  plotPCA(iris, "Species", scale=FALSE)
  plotPCA(iris, "Species", exclude=c("Sepal.Length"))
  checkException(plotPCA(iris, "Species", exclude=c("Sepal.Length", "Sepal.Width", "Petal.Length")))
  
  plotPCA(BostonHousing, "medv")
}
