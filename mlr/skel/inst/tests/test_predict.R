context("predict")

test_that("predict", {
	inds = multiclass.train.inds
	data = multiclass.df
	formula = multiclass.formula
	
	wl.lda = makeLearner("classif.lda", predict.type="prob")
  
	cm2 <- train(makeLearner("classif.lda"), multiclass.task, subset=inds)
	cp2 <- predict(cm2, newdata=data[inds,])
	cp2b <- predict(cm2, newdata=data[inds,-5])
	ext2 <- lda(formula, data=data[inds,])
	pred2 <- predict(ext2,newdata=data[inds,])$class
	
	expect_equal(cp2$df$response, pred2)
	expect_equal(cp2b$df$response, pred2)
	
	cm3 = train(wl.lda, multiclass.task, subset=inds)
	cp3 = predict(cm3, newdata=data[multiclass.test.inds,])
	ext3 = lda(formula, data=data[inds,])
	pred3 = predict(ext3,newdata=data[multiclass.test.inds,])$class
  prob3 = predict(ext3,newdata=data[multiclass.test.inds,])$post
	expect_equal(cp3$df$response, pred3)
  expect_equal(prob3, as.matrix(getProb(cp3, colnames(prob3))))
  expect_true(is.numeric(getProb(cp3, "setosa")))
  expect_equal(colnames(getProb(cp3, c("setosa", "versicolor"))), c("setosa", "versicolor"))
  expect_equal(colnames(getProb(cp3, c("versicolor", "setosa"))), c("versicolor", "setosa"))
  
	cp4 <- predict(cm3, task=multiclass.task, subset=multiclass.test.inds)
	expect_equal(cp4$df$response, pred3)
	expect_equal(cp4$df$truth, data[multiclass.test.inds, multiclass.target])
	expect_equal(cp4$df$id, multiclass.test.inds)
	
	df3 = as.data.frame(cp3)
	df4 = as.data.frame(cp4)
	expect_equal(df3, df4[,-1])
	
	cm5 = train(wl.lda, binaryclass.task, subset=binaryclass.train.inds)
	cp5a = predict(cm5, task=binaryclass.task, subset=binaryclass.test.inds)
	cp5b = predict(cm5, task=binaryclass.task, subset=binaryclass.test.inds)
	cp5c = setThreshold(cp5b, 0)
  cp5d = setThreshold(cp5b, 1)
	cp5e = predict(cm5, task=binaryclass.task, subset=1)
	expect_equal(cp5a$df$response, cp5b$df$response)
	f1 = factor(rep(binaryclass.task$desc$positive, length(binaryclass.test.inds)), levels=binaryclass.task$desc$class.levels)
	expect_equal(cp5c$df$response, f1)
	f2 = factor(rep(binaryclass.task$desc$negative, length(binaryclass.test.inds)), levels=binaryclass.task$desc$class.levels)
	expect_equal(cp5d$df$response, f2)
	expect_true(setequal(levels(cp5e$df$response), c("M", "R")))
		
	# check strange chars in labels
	df = binaryclass.df
	levels(df[,binaryclass.target]) = c(-1,1)
	ct = makeClassifTask(data=df, target=binaryclass.target)
	cm7 = train(wl.lda, task=ct)
	cp7 = predict(cm7, task=ct)
	expect_equal(colnames(cp7$df), c("id", "truth", "prob.-1", "prob.1", "response"))
	
	# check error in predict
	df = na.omit(BreastCancer[,-1]) 
	ct = makeClassifTask(data=df, target="Class")
	res = makeResampleDesc("CV", iters=10)
	p = resample(makeLearner("classif.randomForest"), ct, res)
	expect_true(all(is.na(p$measures.test$mmce)))
	
	#FIXME dec values!!!
})