


test.lda <- function() {
	
	set.seed(debug.seed)
	m <- lda(formula=multiclass.formula, data=multiclass.train)
	set.seed(debug.seed)
	p <- predict(m, newdata=multiclass.test)
	
	simple.test("lda", multiclass.df, multiclass.formula, multiclass.train.inds, p$class)
	prob.test  ("lda", multiclass.df, multiclass.formula, multiclass.train.inds, p$posterior)
	
	tt <- "lda"
	tp <- function(model, newdata) predict(model, newdata)$class
	
	cv.test("lda", multiclass.df, multiclass.formula, tune.train=tt, tune.predict=tp )
	
}
