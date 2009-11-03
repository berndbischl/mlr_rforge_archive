test.predict <- function() {
	
	data = multiclass.df
	formula = multiclass.formula
	inds <- multiclass.train.inds
	
	ct2 <- make.classif.task("lda", data=data, formula=formula)
	cm2 <- train(ct2, subset=inds)
	cp2 <- predict(cm2, newdata=data[inds,])
	ext2 <- lda(formula, data=data[inds,])
	pred2 <- predict(ext2,newdata=data[inds,])$class
	
	checkEquals(cp2, pred2)
	
	ct3 <- make.classif.task("lda", data=data, formula=formula)
	cm3 <- train(ct2, subset=inds)
	cp3 <- predict(cm2, newdata=data[-inds,])
	ext3 <- lda(formula, data=data[inds,])
	pred3 <- predict(ext2,newdata=data[-inds,])$class
	
	checkEquals(cp3, pred3)
	
	# find a good test for pred.fct.pars....  
	
}