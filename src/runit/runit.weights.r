test.weights <- function() {
	ws = 1:nrow(regr.df)
	rt = make.task(target=regr.target, data=regr.df, weights=ws)
	m = train("regr.lm", task=rt)
	p = predict(m, task=rt, subset=30:100)
	df = as.data.frame(p)
	cns = colnames(df)
	checkEquals(cns, c("id", "truth", "response"))

	# glm bug, we need do.call
	m2 = do.call(lm, list(regr.formula, data=regr.df, weights=ws))
	p2 = predict(m2, newdata=regr.df[30:100,])
	checkEquals(p2, p["response"], checkNames=FALSE)
	
	
	checkException(make.task(data=multiclass.df, target=multiclass.target, weights=1:2))
	s = geterrmessage()
	checkTrue(length(grep("Weights have to be of the same length", s)) >0 )
}
