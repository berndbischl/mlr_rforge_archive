test.predict <- function() {
	
	inds = multiclass.train.inds
	data = multiclass.df
	formula = multiclass.formula
	
	wl.lda = make.learner("classif.lda", predict.type="prob")
  
	cm2 <- train("classif.lda", multiclass.task, subset=inds)
	cp2 <- predict(cm2, newdata=data[inds,])
	cp2b <- predict(cm2, newdata=data[inds,-5])
	ext2 <- lda(formula, data=data[inds,])
	pred2 <- predict(ext2,newdata=data[inds,])$class
	
	checkEquals(cp2["response"], pred2)
	checkEquals(cp2b["response"], pred2)
	
	cm3 <- train(wl.lda, multiclass.task, subset=inds)
	cp3 <- predict(cm3, newdata=data[multiclass.test.inds,])
	ext3 <- lda(formula, data=data[inds,])
	pred3 <- predict(ext3,newdata=data[multiclass.test.inds,])$class
	
	checkEquals(cp3["response"], pred3)
	
	cp4 <- predict(cm3, task=multiclass.task, subset=multiclass.test.inds)
	checkEquals(cp4["response"], pred3)
	checkEquals(cp4["truth"], data[multiclass.test.inds, multiclass.target])
	checkEquals(cp4["id"], multiclass.test.inds)
	
	df3 = as.data.frame(cp3)
	df4 = as.data.frame(cp4)

	checkEquals(df3$truth, df4$truth)
	checkEquals(df3$response, df4$response)
	cn3 = colnames(df3)
	cn4 = colnames(df4)
	checkTrue(setequal(cn3, c("response", "truth")))
	checkTrue(setequal(cn4, c("id", "response", "truth")))
	
	cm5 = train(wl.lda, binaryclass.task, subset=binaryclass.train.inds)
	cp5a = predict(cm5, task=binaryclass.task, subset=binaryclass.test.inds)
	cp5b = predict(cm5, task=binaryclass.task, subset=binaryclass.test.inds)
	cp5c = set.threshold(cp5b, 0)
  cp5d = set.threshold(cp5b, 1)
	cp5e = predict(cm5, task=binaryclass.task, subset=1)
	checkEquals(cp5a["response"], cp5b["response"])
	f1 = factor(rep(binaryclass.task["positive"], length(binaryclass.test.inds)), levels=binaryclass.task["class.levels"])
	checkEquals(cp5c["response"], f1)
	f2 = factor(rep(binaryclass.task["negative"], length(binaryclass.test.inds)), levels=binaryclass.task["class.levels"])
	checkEquals(cp5d["response"], f2)
	checkTrue(setequal(levels(cp5e["response"]), c("M", "R")))
		
	# check strange chars in labels
	df = binaryclass.df
	levels(df[,binaryclass.target]) = c(-1,1)
	ct = make.task(data=df, target=binaryclass.target)
	cm7 = train(wl.lda, task=ct)
	cp7 = predict(cm7, task=ct)
	checkEquals(colnames(cp7@df), c("id", "truth", "prob.-1", "prob.1", "response"))
	
	# check error in predict
	df = na.omit(BreastCancer[,-1]) 
	ct = make.task(data=df, target="Class")
	res = make.res.desc("cv", iters=10)
	p = resample("classif.randomForest", ct, res)
	perf = performance(p)
	checkTrue(all(is.na(perf$measures$mmce)))
	
	#todo dec values!!!
}