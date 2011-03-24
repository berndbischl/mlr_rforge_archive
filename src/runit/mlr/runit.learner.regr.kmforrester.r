test.forrester <- function() {
  library(DiceKriging)
  parset.list <- list(
   # list(covtype="gauss"),
    list(covtype="matern5_2")
  )
  dd = regr.df[1:50, c(1:3, 14)]
  rt = makeRegrTask(data=dd, target=regr.target)
  res = makeResampleDesc("Holdout")
  r = resample("regr.kmforrester", rt, res)
  checkTrue(!is.na(r$aggr["mse.test.mean"]))
}
