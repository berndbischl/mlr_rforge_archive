
test.gbm.classif <- function() {
	
	parset.list <- list(
			list(),
			list(n.trees=600),
			list(interaction.depth = 2)
	)
	
	
	old.predicts.list = list()
	old.probs.list = list()
	
	mydata=binaryclass.train
	mydata[, binaryclass.target] = as.numeric(mydata[, binaryclass.target] ==  binaryclass.task["positive"])
	for (i in 1:length(parset.list)) {
		parset <- parset.list[[i]]
		pars <- list(binaryclass.formula, data=mydata, distribution="bernoulli")
		pars <- c(pars, parset)
		set.seed(debug.seed)
		capture.output(
				m <- do.call(gbm, pars)
		)
		set.seed(debug.seed)
		p = predict(m, newdata=binaryclass.test, n.trees=length(m$trees), type="response")
		old.probs.list[[i]] = p
		p = as.factor(ifelse(p > 0.5, "M", "R"))
		old.predicts.list[[i]] = p
	}
	
	simple.test.parsets("classif.gbm", binaryclass.df, binaryclass.formula, binaryclass.train.inds, old.predicts.list, parset.list)
	prob.test.parsets("classif.gbm", binaryclass.df, binaryclass.formula, binaryclass.train.inds, old.probs.list, parset.list)
}
