


test.learner <- function() {
	wl = make.learner("classif.rpart", minsplit=3)
	checkEquals(wl["is.classif"], T)
	checkEquals(wl["is.regr"], F)
	checkEquals(wl["id"], "classif.rpart")
	checkEquals(wl["label"], "RPart")
	checkEquals(wl["supports.multiclass"], T)
	checkEquals(wl["supports.probs"], T)
	checkEquals(wl["supports.decision"], F)
	checkEquals(wl["supports.missings"], T)
	checkEquals(wl["supports.weights"], T)
	checkEquals(wl["supports.costs"], T)
	checkEquals(wl["supports.numerics"], T)
	checkEquals(wl["supports.factors"], T)
	checkEquals(wl["supports.characters"], F)

	wl = make.learner("regr.lm")
	checkEquals(wl["is.classif"], F)
	checkEquals(wl["is.regr"], T)
	checkEquals(wl["id"], "regr.lm")
	checkEquals(wl["supports.multiclass"], F)
	checkEquals(wl["supports.probs"], F)
	checkEquals(wl["supports.decision"], F)
	checkEquals(wl["supports.missings"], F)
	checkEquals(wl["supports.weights"], T)
	checkEquals(wl["supports.costs"], F)
	checkEquals(wl["supports.numerics"], T)
	checkEquals(wl["supports.factors"], T)
	checkEquals(wl["supports.characters"], F)
	
}
