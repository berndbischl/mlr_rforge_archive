


test.learner <- function() {
	wl = makeLearner("classif.rpart", minsplit=3)
	checkEquals(wl@properties[["type"]], "classif")
	checkEquals(wl@id, "classif.rpart")
  checkEquals(getProperty(wl, "oneclass"), F, checkNames=FALSE)
  checkEquals(getProperty(wl, "twoclass"), T, checkNames=FALSE)
  checkEquals(getProperty(wl, "multiclass"), T, checkNames=FALSE)
	checkEquals(getProperty(wl, "prob"), T, checkNames=FALSE)
	checkEquals(getProperty(wl, "decision"), F, checkNames=FALSE)
	checkEquals(getProperty(wl, "missings"), T)
	checkEquals(getProperty(wl, "weights"), T)
	checkEquals(getProperty(wl, "costs"), T)
	checkEquals(getProperty(wl, "numerics"), T, checkNames=FALSE)
	checkEquals(getProperty(wl, "factors"), T, checkNames=FALSE)

	wl = makeLearner("regr.lm")
	checkEquals(wl@properties[["type"]], "regr")
	checkEquals(wl@id, "regr.lm")
  checkEquals(getProperty(wl, "oneclass"), F, checkNames=FALSE)
  checkEquals(getProperty(wl, "twoclass"), F, checkNames=FALSE)
  checkEquals(getProperty(wl, "multiclass"), F, checkNames=FALSE)
  checkEquals(getProperty(wl, "missings"), F)
  checkEquals(getProperty(wl, "weights"), T)
  checkEquals(getProperty(wl, "costs"), F)
  checkEquals(getProperty(wl, "numerics"), T, checkNames=FALSE)
  checkEquals(getProperty(wl, "factors"), T, checkNames=FALSE)
	
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
