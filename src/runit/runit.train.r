
test.train <- function() {
	data <- testsuite.df
	formula <- testsuite.formula
	inds <- testsuite.train.inds
	k <- 3
	
	#----------------------------------lda----------------------------------------
	ct1 <- new("classif.task", wrapped.learner=new("lda"), data=data, formula=formula)
	cm1 <- train(ct1)
	ext1 <- try(lda(formula, data=data))
	
	
	checkTrue(ifelse(class(cm1@learner.model)== "learner.failure", 
					checkTrue(class(ext1)=="try-error") , checkEqualsNumeric(cm1@learner.model$means, ext1$means)))
	
	checkTrue(ifelse(class(cm1@learner.model)== "learner.failure", 
					checkTrue(class(ext1)=="try-error") , checkEqualsNumeric(cm1@learner.model$scaling, ext1$scaling)))
	
	
	
	checkEquals(cm1@subset, 1:nrow(data))
	
	ct2 <- new("classif.task", wrapped.learner=new("lda"), data=data, formula=formula)
	cm2 <- train(ct2, subset=inds)
	ext2 <- lda(formula, data=data[inds,])
	
	checkTrue(ifelse(class(cm2@learner.model)[1]== "learner.failure", 
					checkTrue(class(ext2)=="try-error") , checkEqualsNumeric(cm2@learner.model$means, ext2$means)))
	
	checkTrue(ifelse(class(cm2@learner.model)[1]== "learner.failure", 
					checkTrue(class(ext2)=="try-error") , checkEqualsNumeric(cm2@learner.model$scaling, ext2$scaling)))
	
	checkEquals(cm2@subset, inds)
	
	#------------------------------rpart------------------------------------------
	ct3 <- new("classif.task", wrapped.learner=new("rpart.classif"), data=data, formula=formula)
	cm3 <- train(ct3, subset=inds ,parset=list(minsplit=10, cp= 0.005))
	ext3 <- try(rpart(formula = formula, data = data[inds,], minsplit=10, cp= 0.005))
	
	checkEquals(cm3@subset, inds) 
	checkEquals(cm3@parset, list(minsplit=10, cp= 0.005)) 
	
	checkTrue(ifelse(class(cm3@learner.model)[1]== "learner.failure", 
					checkTrue(class(ext3)=="try-error") , checkEqualsNumeric(cm3@learner.model$splits, ext3$splits)))
	
	checkTrue(ifelse(class(cm3@learner.model)[1]== "learner.failure", 
					checkTrue(class(ext3)=="try-error") , checkEquals(cm3@learner.model$control, ext3$control)))
	
	
#  checkEqualsNumeric(cm3@learner.model$splits, ext3$splits)
#  checkEquals(cm3@learner.model$control, ext3$control)
#  
# print(data)
# print(formula) 
#  ct4 <- new("t.rpart", data=data, formula=formula, train.fct.pars=list(weights=1:nrow(data)))
#  cm4 <- train(ct4)  
#  w <- 1:nrow(data)    
#  print(w)
#  w <- 1:150
#  rpart(formula, data=data, weight=w)
#
#  checkEquals(cm4@learn.task@train.fct.pars, list(weights=1:nrow(data)))
#  
# checkTrue(ifelse(class(cm4@learner.model)== "learner.failure", 
# checkTrue(class(ext4)=="try-error") ,checkEqualsNumeric(cm4@learner.model$splits, ext4$splits)))
	#checkEqualsNumeric(cm4@learner.model$splits, ext4$splits)
	
	
	#--------------------------------qda------------------------------------------
	ct5 <- new("classif.task", wrapped.learner=new("qda"), data=data, formula=formula)
	cm5 <- train(ct5)
	ext5 <- try(qda(formula, data=data))
	
	
	checkTrue(ifelse(class(cm5@learner.model)[1]== "learner.failure", 
					checkTrue(class(ext5)=="try-error") , checkEqualsNumeric(cm5@learner.model$means, ext5$means)))
	
	checkTrue(ifelse(class(cm5@learner.model)[1]== "learner.failure", 
					checkTrue(class(ext5)=="try-error"), checkEqualsNumeric(cm5@learner.model$scaling, ext5$scaling)))
	
	checkTrue(ifelse(class(cm5@learner.model)[1]== "learner.failure", 
					checkTrue(class(ext5)=="try-error"), checkEqualsNumeric(cm5@learner.model$ldet, ext5$ldet)))
	
	#checkEqualsNumeric(cm5@learner.model$scaling, ext5$scaling)
	#checkEqualsNumeric(cm5@learner.model$ldet, ext5$ldet)
	#checkEquals(cm5@subset, 1:nrow(data))
	
	
	
	ct6 <- new("classif.task", wrapped.learner=new("qda"), data=data, formula=formula)
	cm6 <- train(ct6, subset=inds)
	ext6 <- try(qda(formula, data=data[inds,]))
	
	
	checkTrue(ifelse(class(cm6@learner.model)== "learner.failure", 
					checkTrue(class(ext6)=="try-error"),checkEqualsNumeric(cm6@learner.model$means, ext6$means)))
	
	checkTrue(ifelse(class(cm6@learner.model)== "learner.failure", 
					checkTrue(class(ext6)=="try-error"),checkEqualsNumeric(cm6@learner.model$scaling, ext6$scaling)))
	
	checkTrue(ifelse(class(cm6@learner.model)== "learner.failure", 
					checkTrue(class(ext6)=="try-error"), checkEqualsNumeric(cm6@learner.model$ldet, ext6$ldet)))
	
	#checkEqualsNumeric(cm6@learner.model$means, ext6$means)
	#checkEqualsNumeric(cm6@learner.model$scaling, ext6$scaling)
	#checkEqualsNumeric(cm6@learner.model$ldet, ext6$ldet)
	checkEquals(cm6@subset, inds) 
	
	#-------------------------knn-------------------------------------------------
	ct7 <- new("classif.task", wrapped.learner=new("kknn.knn.classif"), data=data, formula=formula)
	cm7 <- train(ct7, parset = list(k=k))
	
	checkEquals(cm7@parset,  list(k=k))
	checkEquals(cm7@subset, 1:nrow(data))
	
	ct8 <- new("classif.task", wrapped.learner=new("kknn.knn.classif"), data=data, formula=formula)
	cm8 <- train(ct8, subset=inds, parset = list(k=k))
	
	checkEquals(cm8@subset, inds)
	
	
	#-------------------------randomForest----------------------------------------
	ct9 <- new("classif.task", wrapped.learner=new("randomForest.classif"), data=data, formula=formula)
	cm9 <- train(ct9, parset = list(ntree= 100, mtry= floor(sqrt((ncol(data) - 1 )))))
	
	set.seed(debug.seed)                   
	ext9 <- try(randomForest(formula= formula, data=data, ntree= 100, mtry= floor(sqrt((ncol(data) - 1 )))))
	
	
	checkEquals(cm9@parset,  list(ntree= 100, mtry= floor(sqrt((ncol(data) - 1 )))))
	checkEquals(cm9@subset, 1:nrow(data))
	
	if(class(cm9@learner.model)[1]== "learner.failure") {
		checkTrue(class(ext9)=="try-error")
	} else {
		checkEquals(cm9@learner.model$predicted, ext9$predicted)
	}
	if(class(cm9@learner.model)[1]== "learner.failure") {
		checkTrue(class(ext9)=="try-error")
	} else {
		checkEquals(cm9@learner.model$err.rate, ext9$err.rate)
	}
	if(class(cm9@learner.model)[1]== "learner.failure") {
		checkTrue(class(ext9)=="try-error")
	} else {
		checkEquals(cm9@learner.model$importance, ext9$importance)
	}
	if(class(cm9@learner.model)[1]== "learner.failure") {
		checkTrue(class(ext9)=="try-error")
	} else {
		checkEquals(cm9@learner.model$forest, ext9$forest)
	}
	
	#checkEquals(cm9@learner.model$predicted, ext9$predicted)
#  checkEqualsNumeric(cm9@learner.model$err.rate, ext9$err.rate)
#  checkEqualsNumeric(cm9@learner.model$importance, ext9$importance)
#  checkEquals(cm9@learner.model$forest, ext9$forest)
	
	
	ct10 <- new("classif.task", wrapped.learner=new("randomForest.classif"), data=data, formula=formula)
	cm10 <- train(ct10,subset=inds, parset = list(ntree= 100, mtry= floor(sqrt((ncol(data) - 1 )))))
	
	set.seed(debug.seed)                   
	ext10 <- try(randomForest(formula= formula, data=data[inds,], ntree= 100, mtry= floor(sqrt((ncol(data) - 1 )))))
	
	
	checkEquals(cm10@subset, inds)
	
	if(class(cm10@learner.model)[1]== "learner.failure") {
		checkTrue(class(ext10)=="try-error")
	} else {
		checkEquals(cm10@learner.model$predicted, ext10$predicted)
	}
	if(class(cm10@learner.model)[1]== "learner.failure") {
		checkTrue(class(ext10)=="try-error")
	} else {
		checkEquals(cm10@learner.model$err.rate, ext10$err.rate)
	}
	if(class(cm10@learner.model)[1]== "learner.failure") {
		checkTrue(class(ext10)=="try-error")
	} else {
		checkEquals(cm10@learner.model$importance, ext10$importance)
	}
	if(class(cm10@learner.model)[1]== "learner.failure") {
		checkTrue(class(ext10)=="try-error")
	} else {
		checkEquals(cm10@learner.model$forest, ext10$forest)
	}
	
#checkEquals(cm10@learner.model$predicted, ext10$predicted)
#checkEqualsNumeric(cm10@learner.model$err.rate, ext10$err.rate)
#checkEqualsNumeric(cm10@learner.model$importance, ext10$importance)
#checkEquals(cm10@learner.model$forest, ext10$forest)  
}