test.fnn.regr <- function() {
  library(FNN)
  parset.list <- list(
    list(),
    list(k=1),
    list(k=4),
    list(k=10)
  )
  
  # todo: does not work as test task has factors...
  
  old.predicts.list1 = list()
  
  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    j = which(colnames(regr.train) == regr.target)
    print(j)
    pars = list(train=regr.train[,-c(j,4)], test=regr.test[,-c(j,4)], y=regr.train[,j])
    pars = c(pars, parset)
    set.seed(debug.seed)
    old.predicts.list1[[i]] = do.call(FNN::knn.reg, pars)
  }
  
  simple.test.parsets("regr.fnn", regr.df, regr.target, regr.train.inds, old.predicts.list1, parset.list)
  
  tt <- function (formula, data, k=1) {
    j = which(colnames(data) == as.character(formula)[2])
    list(train=data[,-j], y=data[,j], k=k, target=j)
  }
  tp <- function(model, newdata) {
    newdata = newdata[, -model$target]
    FNN::knn(train=model$train, test=newdata, y=model$y, k=model$k)
  }
  
  cv.test.parsets("regr.fnn", regr.df, regr.target, tune.train=tt, tune.predict=tp, parset.list=parset.list)
}
