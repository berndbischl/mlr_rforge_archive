


test.learner <- function() {
	wl = makeLearner("classif.rpart", minsplit=3)
	checkEquals(wl@desc@type, "classif")
	checkEquals(wl@desc@id, "classif.rpart")
  checkEquals(wl@desc@classes["oneclass"], F)
  checkEquals(wl@desc@classes["twoclass"], T)
  checkEquals(wl@desc@classes["multiclass"], T)
	checkEquals(wl@desc@predict["prob"], T)
	checkEquals(wl@desc@predict["decision"], F)
	checkEquals(wl["missings"], T)
	checkEquals(wl["weights"], T)
	checkEquals(wl["costs"], T)
	checkEquals(wl["doubles"], T)
	checkEquals(wl["factors"], T)

	wl = makeLearner("regr.lm")
	checkEquals(wl@desc@type, "regr")
	checkEquals(wl@desc@id, "regr.lm")
	checkEquals(wl["multiclass"], NULL)
	checkEquals(wl["probs"], NULL)
	checkEquals(wl["decision"], NULL)
	checkEquals(wl["missings"], F)
	checkEquals(wl["weights"], T)
	checkEquals(wl["costs"], NULL)
	checkEquals(wl["doubles"], T)
	checkEquals(wl["factors"], T)
	
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
