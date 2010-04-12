


test.lda <- function() {
	
	set.seed(debug.seed)
	m <- lda(formula=multiclass.formula, data=multiclass.train)
	set.seed(debug.seed)
	p <- predict(m, newdata=multiclass.test)
	
	simple.test("classif.lda", multiclass.df, multiclass.formula, multiclass.train.inds, p$class)
	prob.test  ("classif.lda", multiclass.df, multiclass.formula, multiclass.train.inds, p$posterior)
	
	tt <- "lda"
	tp <- function(model, newdata) predict(model, newdata)$class
	
	cv.test("classif.lda", multiclass.df, multiclass.formula, tune.train=tt, tune.predict=tp )
	
	# test with constant col to produce an error in lda 
	mc2 = transform(multiclass.df, bam=1)
	ct = make.classif.task(data=mc2, target=multiclass.target)
	res = make.res.desc("cv", iters=2)
	rf = resample.fit("classif.lda", ct, resampling=res, type="response")
	rf = resample.fit("classif.lda", ct, resampling=res, type="prob")
	
	
}
