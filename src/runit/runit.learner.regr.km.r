test.km.regr <- function() {
  library(DiceKriging)
  parset.list <- list(
    #list(covtype="gauss"),
    list(covtype="matern5_2")
  )
  dd = regr.df[1:50, c(1:3, 14)]
  old.predicts.list = list()
  des1 = dd[1:25, setdiff(colnames(dd), regr.target)]
  des2 = dd[26:50, setdiff(colnames(dd), regr.target)]
  y = dd[1:25, regr.target]
  for (i in 1:length(parset.list)) {
    parset <- parset.list[[i]]
    pars <- list(~1, design=des1, response=y)
    pars <- c(pars, parset)
    set.seed(debug.seed)
    m <- do.call(km, pars)
    old.predicts.list[[i]] = predict(m, newdata=des2, type="SK")$mean
  }
  simple.test.parsets("regr.km", dd, regr.target, 1:25, old.predicts.list, parset.list)
}
