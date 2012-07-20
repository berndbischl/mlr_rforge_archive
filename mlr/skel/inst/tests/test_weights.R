context("weights")

test_that("weights", {
	ws = 1:nrow(regr.df)
	rt = makeRegrTask(target=regr.target, data=regr.df, weights=ws)
	m = train(makeLearner("regr.lm"), task=rt)
	p = predict(m, task=rt, subset=30:100)
	df = as.data.frame(p)
	cns = colnames(df)
	expect_equal(cns, c("id", "truth", "response"))

	# glm bug, we need do.call
	m2 = do.call(lm, list(regr.formula, data=regr.df, weights=ws))
	p2 = predict(m2, newdata=regr.df[30:100,])
	expect_equal(p2, p$df$response, checkNames=FALSE)
	
	
	checkException(makeClassifTask(data=multiclass.df, target=multiclass.target, weights=1:2))
	s = geterrmessage()
	expect_true(length(grep("Argument weights must be of length", s)) >0 )
})
