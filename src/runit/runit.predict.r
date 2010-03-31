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
	cp3 <- predict(cm2, newdata=data[-inds,])
	ext3 <- lda(formula, data=data[inds,])
	pred3 <- predict(ext2,newdata=data[-inds,])$class
	
	checkEquals(cp3["response"], pred3)
	
	# find a good test for pred.fct.pars....  
	
}