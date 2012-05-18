context("novars")

test_that("novars", {
  ct = subsetTask(multiclass.task, features=character(0))
	wl = makeLearner("classif.lda", predict.type="prob")
	m = train(wl, ct)
  expect_is(m$learner.model, "NoFeaturesModel")	
  expect_is(m$learner, "classif.lda")	
	expect_equal(m$learner$predict.type, "prob")	
	p = predict(m, newdata=multiclass.df)
	expect_true(setequal(colnames(as.data.frame(p)), c("prob.setosa", "prob.virginica", "prob.versicolor", "truth", "response")))	
	res = makeResampleDesc("CV", iter=2)
	rf = resample(wl, ct, res)
	expect_true(setequal(colnames(as.data.frame(p)), c("prob.setosa", "prob.virginica", "prob.versicolor", "truth", "response")))	
	
	rt = subsetTask(regr.task, features=character(0))
  m = train("regr.lm", rt)
	p = predict(m, newdata=regr.df)
	expect_true(all(p$df$response == mean(p$df$response))) 
  rf = resample(makeLearner("regr.lm"), rt, res)$pred
	expect_equal(length(unique(rf$df$response)), 2) 
})