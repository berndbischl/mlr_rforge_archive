


test.learner <- function() {
	wl = make.learner("classif.rpart", minsplit=3)
	checkEquals(wl["is.classif"], T)
	checkEquals(wl["is.regr"], F)
	checkEquals(wl["id"], "classif.rpart")
	checkEquals(wl["multiclass"], T)
	checkEquals(wl["probs"], T)
	checkEquals(wl["decision"], F)
	checkEquals(wl["missings"], T)
	checkEquals(wl["weights"], T)
	checkEquals(wl["costs"], T)
	checkEquals(wl["numerics"], T)
	checkEquals(wl["factors"], T)
	checkEquals(wl["characters"], F)

	wl = make.learner("regr.lm")
	checkEquals(wl["is.classif"], F)
	checkEquals(wl["is.regr"], T)
	checkEquals(wl["id"], "regr.lm")
	checkEquals(wl["multiclass"], F)
	checkEquals(wl["probs"], F)
	checkEquals(wl["decision"], F)
	checkEquals(wl["missings"], F)
	checkEquals(wl["weights"], T)
	checkEquals(wl["costs"], F)
	checkEquals(wl["numerics"], T)
	checkEquals(wl["factors"], T)
	checkEquals(wl["characters"], F)
	
}
