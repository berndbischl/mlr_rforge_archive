test.novars <- function() {
	wl = make.learner("classif.lda", predict.type="prob")
	m = train(wl, multiclass.task, vars=character(0))
  checkTrue(is(m["learner.model"], "novars"))	
  checkTrue(is(m["learner"], "classif.lda"))	
	checkEquals(m["learner"]["predict.type"], "prob")	
	p = predict(m, newdata=multiclass.df)
	checkTrue(setequal(colnames(as.data.frame(p)), c("prob.setosa", "prob.virginica", "prob.versicolor", "truth", "response")))	
	res = make.res.desc("cv", iter=2)
	rf = resample(wl, multiclass.task, res, vars=c())
	checkTrue(setequal(colnames(as.data.frame(p)), c("prob.setosa", "prob.virginica", "prob.versicolor", "truth", "response")))	
	
	m = train("regr.lm", regr.task, vars=c())
	p = predict(m, newdata=multiclass.df)
	checkTrue(all(p["response"] == mean(p["response"]))) 
  rf = resample("regr.lm", regr.task, res, vars=c(), predictions=TRUE)$pred
	checkEquals(length(unique(rf["response"])), 2) 
}