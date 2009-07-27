
test.rpart <- function() {

  parset.list <- list(
    list(),
    list(minsplit=10, cp= 0.005),
    list(minsplit=50, cp=0.05),
    list(minsplit=50, cp=0.999),
    list(minsplit=1, cp=0.0005)
  )

  old.predicts.list = list()
  old.probs.list = list()
  
  for (i in 1:length(parset.list)) {
    parset <- parset.list[[i]]
    pars <- list(formula=testsuite.formula, data=testsuite.train)
    pars <- c(pars, parset)
    set.seed(debug.seed)
    m <- do.call(rpart, pars)
    p  <- predict(m, newdata=testsuite.test, type="class")
	p2 <- predict(m, newdata=testsuite.test, type="prob")
	old.predicts.list[[i]] <- p
	old.probs.list[[i]] <- p2
}

  simple.test.parsets("rpart.classif", testsuite.df, testsuite.formula, testsuite.train.inds, old.predicts.list, parset.list)
  prob.test.parsets  ("rpart.classif", testsuite.df, testsuite.formula, testsuite.train.inds, old.probs.list, parset.list)

  tt <- "rpart"
  tp <- function(model, newdata) predict(model, newdata, type="class")

  cv.test.parsets("rpart.classif", testsuite.df, testsuite.formula, tune.train=tt, tune.predict=tp, parset.list=parset.list)

}

