test.predict <- function() {
	
	data = testsuite.df
	formula = testsuite.formula
	inds <- testsuite.train.inds
	
	ct1 <- new("classif.task", new("lda"), data=data, formula=formula)
	cm1 <- train(ct1)
	cp1 <- predict(ct1, cm1)
	ext1 <- lda(formula, data=data)
	pred1 <- predict(ext1,newdata=data)$class
	
	checkEquals(cp1, pred1)
	
	
	ct2 <- new("classif.task", new("lda"), data=data, formula=formula)
	cm2 <- train(ct2, subset=inds)
	cp2 <- predict(ct2, cm2)
	ext2 <- lda(formula, data=data[inds,])
	pred2 <- predict(ext2,newdata=data[inds,])$class
	
	checkEquals(cp2, pred2)
	
	ct3 <- new("classif.task", new("lda"), data=data, formula=formula)
	cm3 <- train(ct2, subset=inds)
	cp3 <- predict(ct2, cm2, newdata=data[-inds,])
	ext3 <- lda(formula, data=data[inds,])
	pred3 <- predict(ext2,newdata=data[-inds,])$class
	
	checkEquals(cp3, pred3)
	
	# find a good test for pred.fct.pars....  
	
}