
test.mda <- function() {

  parset.list <- list(
    list(),
    list(subclasses=2),
    list(subclasses=7)
  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in 1:length(parset.list)) {
    parset <- parset.list[[i]]
    pars <- list(formula=testsuite.formula, data=testsuite.train)
    pars <- c(pars, parset)
    set.seed(debug.seed)
    m <- do.call(mda, pars)
    p <-  predict(m, newdata=testsuite.test)
	p2 <- predict(m, newdata=testsuite.test, type="posterior")
	old.predicts.list[[i]] <- p
	old.probs.list[[i]] <- p2
}

  simple.test.parsets("mda", testsuite.df, testsuite.formula, testsuite.train.inds, old.predicts.list, parset.list)
  prob.test.parsets  ("mda", testsuite.df, testsuite.formula, testsuite.train.inds, old.probs.list, parset.list)
  
  tt <- "mda"
  tp <- function(model, newdata) predict(model, newdata)

  cv.test.parsets("mda", testsuite.df, testsuite.formula, tune.train=tt, tune.predict=tp, parset.list=parset.list)

}

