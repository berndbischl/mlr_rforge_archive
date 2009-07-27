test.clr.learn.task <- function() {
	data = testsuite.df
	formula = testsuite.formula
	inds <- testsuite.train.inds
	k <- 3
	
#------------------------------lda----------------------------------------------
	
	ct1 <- new("classif.task", new("lda"), data=data, formula=formula)
	
	checkEquals(ct1@formula, testsuite.formula)
	checkEquals(ct1@data, data)
	
	checkEquals(ct1@wrapped.learner@learner.pack, "MASS")
	checkEquals(ct1@wrapped.learner@train.fct.pars, list())
	checkEquals(ct1@wrapped.learner@predict.fct.pars, list())
	
	wl <- new("lda")
	wl@train.fct.pars <- list(tol=0.5)
	wl@predict.fct.pars <- list(dimen=2)
	
	ct2 <- new("classif.task", wl, data=data, formula=formula) 
	
	checkEquals(ct2@wrapped.learner@train.fct.pars, list(tol=0.5))
	checkEquals(ct2@wrapped.learner@predict.fct.pars, list(dimen=2)) 
	
#-----------------------------rpart---------------------------------------------  
#	
#	
#	ct3 <- new("classif.task", new("rpart.classif", data=data, formula=formula))
#	
#	checkEquals(ct3@formula, testsuite.formula)
#	checkEquals(ct3@data, data)
#	
#	checkEquals(ct3@wrapped.learner@learner.pack, "rpart")
#	checkEquals(ct3@wrapped.learner@train.fct.pars, list())
#	
#	
	##  ct4 <- new("classif.rpart", data=data, formula=formula, train.fct.pars=list(method="class"))  
	##  checkEquals(ct4@train.fct.pars, list(method="class"))                          Max: hab keine sinnvollen predict.fct.pars gefunden
	##  #checkEquals(ct4@predict.fct.pars, list(type="class", na.action=na.pass))      
#	
#	
	##-----------------------------------qda-----------------------------------------
#	
#	ct5 <- new("classif.task", new("qda"), data=data, formula=formula)
#	
#	checkEquals(ct5@formula, testsuite.formula)
#	checkEquals(ct5@data, data)
#	
#	checkEquals(ct5@wrapped.learner@learner.pack, "MASS")
#	checkEquals(ct5@train.fct.pars, list())
#	checkEquals(ct5@predict.fct.pars, list())
#	
#	
#	ct6 <- new("classif.task", new("qda"), data=data, formula=formula, 
#			train.fct.pars=list(method="mle"),predict.fct.pars= list(method="debiased"))
#	
#	checkEquals(ct6@train.fct.pars, list(method="mle"))
#	checkEquals(ct6@predict.fct.pars, list(method="debiased")) 
#	
#	
#	
#	#------------------------------knn---------------------------------------------
#	
#	
#	ct7 <- new("classif.task", new("classif.knn"), data=data, formula=formula)
#	
#	checkEquals(ct7@formula, testsuite.formula)
#	checkEquals(ct7@data, data)
#	
#	checkEquals(ct7@wrapped.learner@learner.pack, "kknn")
#	checkEquals(ct7@train.fct.pars, list())
#	checkEquals(ct7@predict.fct.pars, list())
#	
#	
#	ct8 <- new("classif.task", new("knn.classif"), data=data, formula=formula, predict.fct.pars= list(kernel="triangular")) 
#	
#	
#	checkEquals(ct8@predict.fct.pars, list(kernel="triangular")) 
#	
#	
#	#------------------------randomForest-----------------------------------------
#	
#	ct9 <- new("classif.task", new("randomForest.classif"), data=data, formula=formula)
#	
#	checkEquals(ct9@formula, testsuite.formula)
#	checkEquals(ct9@data, data)
#	
#	checkEquals(ct9@wrapped.learner@learner.pack, "randomForest")
#	checkEquals(ct9@train.fct.pars, list())
#	
#	
#	ct10 <- new("randomForest.classif", data=data, formula=formula, 
#			train.fct.pars=list(replace=FALSE),predict.fct.pars= list(norm.votes=FALSE) )
#	
#	checkEquals(ct10@train.fct.pars, list(replace=FALSE))
#	checkEquals(ct10@predict.fct.pars, list(norm.votes=FALSE)) 
	
}