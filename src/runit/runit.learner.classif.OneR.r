
test.OneR <- function() {
  library(RWeka)
	parset.list <- list(
			list(),
			list(B=3)
	)
	
	old.predicts.list = list()
	old.probs.list = list()
	
	for (i in 1:length(parset.list)) {
		parset <- parset.list[[i]]
		ctrl = do.call(Weka_control, parset)
		set.seed(debug.seed)
		m = OneR(formula=multiclass.formula, data=multiclass.train, control=ctrl)
		p  <- predict(m, newdata=multiclass.test, type="class")
		p2 <- predict(m, newdata=multiclass.test, type="prob")
		old.predicts.list[[i]] <- p
		old.probs.list[[i]] <- p2
	}
	
	simple.test.parsets("classif.OneR", multiclass.df, multiclass.target, multiclass.train.inds, old.predicts.list, parset.list)
	prob.test.parsets  ("classif.OneR", multiclass.df, multiclass.target, multiclass.train.inds, old.probs.list, parset.list)
	
	tt <- function (formula, data, subset, ...) {
		OneR(formula, data=data[subset,], control=Weka_control(...))
	}
	
	tp <- function(model, newdata) predict(model, newdata, type="class")
	
	cv.test.parsets("classif.OneR", multiclass.df, multiclass.target, tune.train=tt, tune.predict=tp, parset.list=parset.list)
	
}