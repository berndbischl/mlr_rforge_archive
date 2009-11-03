test.mlr.learn.task <- function() {
	data = multiclass.df
	formula = multiclass.formula
	inds <- multiclass.train.inds
	k <- 3
	
#------------------------------lda----------------------------------------------
	
	ct1 <- make.classif.task("lda", data=data, formula=formula)
	
	checkEquals(ct1["target"], "Species")
	checkEquals(ct1["targets"], data$Species)
	checkEquals(ct1@data, model.frame(formula, data))
	
	checkEquals(ct1@wrapped.learner@learner.pack, "MASS")
	
	wl <- new("lda")
	
	ct2 <- make.classif.task(wl, data=data, formula=formula) 
	
	
#-----------------------------rpart---------------------------------------------  
#	
#	
#	ct3 <- new("classif.task", new("rpart.classif", data=data, formula=formula))
#	
#	checkEquals(ct3@formula, multiclass.formula)
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
#	checkEquals(ct5@formula, multiclass.formula)
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
#	checkEquals(ct7@formula, multiclass.formula)
#	checkEquals(ct7@data, data)
#	
#	checkEquals(ct7@wrapped.learner@learner.pack, "kknn")
#	checkEquals(ct7@train.fct.pars, list())
#	checkEquals(ct7@predict.fct.pars, list())
#	
#	
#	ct8 <- new("classif.task", new("knn.knn.classif"), data=data, formula=formula, predict.fct.pars= list(kernel="triangular")) 
#	
#	
#	checkEquals(ct8@predict.fct.pars, list(kernel="triangular")) 
#	
#	
#	#------------------------randomForest-----------------------------------------
#	
#	ct9 <- new("classif.task", new("randomForest.classif"), data=data, formula=formula)
#	
#	checkEquals(ct9@formula, multiclass.formula)
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