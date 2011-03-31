testPlotFeatDistr = function() {
  plotFeatDistr(iris, "Species", "Sepal.Length")
  plotFeatDistr(iris, "Species", 1)
  plotFeatDistr(BostonHousing, "medv", "chas")
}
