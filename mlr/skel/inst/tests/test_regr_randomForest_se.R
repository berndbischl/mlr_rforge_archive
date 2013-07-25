context("random_forest_standard_error")

test_that("standard error should always be positive", {
	data(BostonHousing)

	n = nrow(BostonHousing)
	train.set.idx = sample(1:n, floor(n * 0.9))
	test.set.idx = setdiff(1:n, train.set.idx)
	train.set = BostonHousing[train.set.idx,]
	test.set = BostonHousing[test.set.idx,]

	method.list = names(getSupportedSEEstimators())
	
	for (method in method.list) {
		task = makeRegrTask(data=train.set, target="medv")
		learner = makeLearner("regr.randomForest", 
			predict.type="se", 
			ntree=30, 
			ntree.for.se=20,
			se.method=method,
			nr.of.bootstrap.samples=5,
			keep.inbag=TRUE)
		model = train(learner, task)
		preds = predict(model, newdata=test.set)
		se.preds = preds$data$se
		expect_true(all(se.preds >= 0), info=paste("Standard error not < 0 for method", method, "and ntree equal to", ntree))
	}
})