test.novars <- function() {
  ct = subset(multiclass.task, vars=c())
	wl = makeLearner("classif.lda", predict.type="prob")
	m = train(wl, ct)
  checkTrue(is(m["learner.model"], "novars"))	
  checkTrue(is(m@learner, "classif.lda"))	
	checkEquals(m@learner["predict.type"], "prob")	
	p = predict(m, newdata=multiclass.df)
	checkTrue(setequal(colnames(as.data.frame(p)), c("prob.setosa", "prob.virginica", "prob.versicolor", "truth", "response")))	
	res = makeResampleDesc("cv", iter=2)
	rf = resample(wl, ct, res)
	checkTrue(setequal(colnames(as.data.frame(p)), c("prob.setosa", "prob.virginica", "prob.versicolor", "truth", "response")))	
	
	rt = subset(regr.task, vars=c())
  m = train("regr.lm", rt)
	p = predict(m, newdata=regr.df)
	checkTrue(all(p@df$response == mean(p@df$response))) 
  rf = resample("regr.lm", rt, res)$pred
	checkEquals(length(unique(rf@df$response)), 2) 
}