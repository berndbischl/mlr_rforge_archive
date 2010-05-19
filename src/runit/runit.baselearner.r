


test.learner <- function() {
	wl1 = make.learner("classif.rpart", minsplit=3)
	checkEquals(wl1["is.classif"], T)
	checkEquals(wl1["is.regr"], F)
	checkEquals(wl1["id"], "classif.rpart")
	checkEquals(wl1["label"], "RPart")
	checkEquals(wl1["supports.multiclass"], T)
	checkEquals(wl1["supports.probs"], T)
	checkEquals(wl1["supports.decision"], F)
	checkEquals(wl1["supports.missings"], T)
	checkEquals(wl1["supports.weights"], T)
	checkEquals(wl1["supports.costs"], T)
	checkEquals(wl1["supports.numerics"], T)
	checkEquals(wl1["supports.factors"], T)
	checkEquals(wl1["supports.characters"], F)
	to.string(wl1)
	
	wl2 = make.learner("regr.lm")
	checkEquals(wl2["is.classif"], F)
	checkEquals(wl2["is.regr"], T)
	checkEquals(wl2["id"], "regr.lm")
	checkEquals(wl2["supports.multiclass"], F)
	checkEquals(wl2["supports.probs"], F)
	checkEquals(wl2["supports.decision"], F)
	checkEquals(wl2["supports.missings"], F)
	checkEquals(wl2["supports.weights"], T)
	checkEquals(wl2["supports.costs"], F)
	checkEquals(wl2["supports.numerics"], T)
	checkEquals(wl2["supports.factors"], T)
	checkEquals(wl2["supports.characters"], F)
	to.string(wl2)
	
	fun = function(data) data
	wl3 = make.preproc.wrapper(wl1, fun=fun)
	to.string(wl3)
	
}
