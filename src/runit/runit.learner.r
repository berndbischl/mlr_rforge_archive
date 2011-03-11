


test.learner <- function() {
	wl = makeLearner("classif.rpart", minsplit=3)
	checkEquals(wl@desc@type, "classif")
	checkEquals(wl@desc@id, "classif.rpart")
  checkEquals(wl@desc@classes["oneclass"], F, checkNames=FALSE)
  checkEquals(wl@desc@classes["twoclass"], T, checkNames=FALSE)
  checkEquals(wl@desc@classes["multiclass"], T, checkNames=FALSE)
	checkEquals(wl@desc@predict["prob"], T, checkNames=FALSE)
	checkEquals(wl@desc@predict["decision"], F, checkNames=FALSE)
	checkEquals(wl@desc@missings, T)
	checkEquals(wl@desc@weights, T)
	checkEquals(wl@desc@costs, T)
	checkEquals(wl@desc@feat["numerics"], T, checkNames=FALSE)
	checkEquals(wl@desc@feat["factors"], T, checkNames=FALSE)

	wl = makeLearner("regr.lm")
	checkEquals(wl@desc@type, "regr")
	checkEquals(wl@desc@id, "regr.lm")
  checkEquals(wl@desc@classes["oneclass"], F, checkNames=FALSE)
  checkEquals(wl@desc@classes["twoclass"], F, checkNames=FALSE)
  checkEquals(wl@desc@classes["multiclass"], F, checkNames=FALSE)
  checkEquals(wl@desc@missings, F)
  checkEquals(wl@desc@weights, T)
  checkEquals(wl@desc@costs, F)
  checkEquals(wl@desc@feat["numerics"], T, checkNames=FALSE)
  checkEquals(wl@desc@feat["factors"], T, checkNames=FALSE)
	
  checkException(makeLearner("classif.lvq1", predict.type="prob"), silent=TRUE)
  s = geterrmessage()
  checkTrue(length(grep("Trying to predict probs, but", s)) >0 )
  
  checkException(makeLearner("regr.lm", predict.type="prob"), silent=TRUE)
  s = geterrmessage()
  checkTrue(length(grep("Trying to predict prob, but", s)) >0 )
  
  wl = makeLearner("classif.lvq1")
  checkException(setPredictType(wl, "prob"), silent=TRUE)
  s = geterrmessage()
  checkTrue(length(grep("Trying to predict probs, but", s)) >0 )
}
