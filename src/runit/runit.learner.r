


test.learner <- function() {
	wl = makeLearner("classif.rpart", minsplit=3)
	checkEquals(wl["is.classif"], T)
	checkEquals(wl["is.regr"], F)
	checkEquals(wl@id, "classif.rpart")
  checkEquals(wl["oneclass"], F)
  checkEquals(wl["twoclass"], T)
  checkEquals(wl["multiclass"], T)
	checkEquals(wl["prob"], T)
	checkEquals(wl["decision"], F)
	checkEquals(wl["missings"], T)
	checkEquals(wl["weights"], T)
	checkEquals(wl["costs"], T)
	checkEquals(wl["doubles"], T)
	checkEquals(wl["factors"], T)

	wl = makeLearner("regr.lm")
	checkEquals(wl["is.classif"], F)
	checkEquals(wl["is.regr"], T)
	checkEquals(wl@id, "regr.lm")
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
  checkException(set.predict.type(wl, "prob"), silent=TRUE)
  s = geterrmessage()
  checkTrue(length(grep("Trying to predict probs, but", s)) >0 )
}
