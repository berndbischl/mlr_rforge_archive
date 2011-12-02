


test.lda <- function() {
	
	set.seed(debug.seed)
	m <- lda(formula=multiclass.formula, data=multiclass.train)
	set.seed(debug.seed)
	p <- predict(m, newdata=multiclass.test)
	
	simple.test("classif.lda", multiclass.df, multiclass.target, multiclass.train.inds, p$class)
	prob.test  ("classif.lda", multiclass.df, multiclass.target, multiclass.train.inds, p$posterior)
	
	tt <- "lda"
	tp <- function(model, newdata) predict(model, newdata)$class
	
	cv.test("classif.lda", multiclass.df, multiclass.target, tune.train=tt, tune.predict=tp )
	
	# test with constant col to produce an error in lda 
	mc2 = transform(multiclass.df, bam=1)
	ct = makeClassifTask(data=mc2, target=multiclass.target)
	res = makeResampleDesc("CV", iters=2)
	lrn = makeLearner("classif.lda")
	rf = resample(lrn, ct, resampling=res)
	lrn = makeLearner("classif.lda", predict.type="prob")
	rf = resample(lrn, ct, resampling=res)
	# todo check na
	
}
