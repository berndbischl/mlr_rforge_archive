test.predict <- function() {
	
	inds = multiclass.train.inds
	data = multiclass.df
	formula = multiclass.formula
	
	
	cm2 <- train("lda", multiclass.task, subset=inds)
	cp2 <- predict(cm2, newdata=data[inds,])
	ext2 <- lda(formula, data=data[inds,])
	pred2 <- predict(ext2,newdata=data[inds,])$class
	
	checkEquals(cp2["response"], pred2)
	
	cm3 <- train("lda", multiclass.task, subset=inds)
	cp3 <- predict(cm3, newdata=data[multiclass.test.inds,])
	ext3 <- lda(formula, data=data[inds,])
	pred3 <- predict(ext3,newdata=data[multiclass.test.inds,])$class
	
	checkEquals(cp3["response"], pred3)
	
	cp4 <- predict(cm3, task=multiclass.task, subset=multiclass.test.inds)
	checkEquals(cp4["response"], pred3)
	checkEquals(cp4["truth"], data[multiclass.test.inds, multiclass.target])
	checkEquals(cp4["id"], multiclass.test.inds)
	
	
	# find a good test for pred.fct.pars....  
	
}