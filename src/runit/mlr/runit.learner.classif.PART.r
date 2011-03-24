
test.PART <- function() {
	
	parset.list <- list(
			list(),
			list(M=10),
			list(M=5, C=0.4),
			list(M=5, R=TRUE)
	)
	
	old.predicts.list = list()
	old.probs.list = list()
	
	for (i in 1:length(parset.list)) {
		parset <- parset.list[[i]]
    set.seed(debug.seed)
    parset$Q = as.integer(runif(1, min=-.Machine$integer.max, max=.Machine$integer.max))
		ctrl = do.call(Weka_control, parset)
		m = PART(formula=multiclass.formula, data=multiclass.train, control=ctrl)
    set.seed(debug.seed)
    p  <- predict(m, newdata=multiclass.test, type="class")
    set.seed(debug.seed)
    p2 <- predict(m, newdata=multiclass.test, type="prob")
		old.predicts.list[[i]] <- p
		old.probs.list[[i]] <- p2
	}
	
	simple.test.parsets("classif.PART", multiclass.df, multiclass.target, multiclass.train.inds, old.predicts.list, parset.list)
	prob.test.parsets  ("classif.PART", multiclass.df, multiclass.target, multiclass.train.inds, old.probs.list, parset.list)
	
	tt <- function (formula, data, subset, ...) {
    set.seed(debug.seed)
		PART(formula, data=data[subset,], control=Weka_control(..., Q = as.integer(runif(1, min=-.Machine$integer.max, max=.Machine$integer.max))))
	}
	
	tp <- function(model, newdata) predict(model, newdata, type="class")
	
	cv.test.parsets("classif.PART", multiclass.df, multiclass.target, tune.train=tt, tune.predict=tp, parset.list=parset.list)
	
}