


test.learner <- function() {
	wl1 = make.learner("classif.rpart", minsplit=3)
	checkEquals(wl1["is.classif"], T)
	checkEquals(wl1["is.regr"], F)
	checkEquals(wl1["id"], "classif.rpart")
	checkEquals(wl1["label"], "RPart")
	checkEquals(wl1["multiclass"], T)
	checkEquals(wl1["probs"], T)
	checkEquals(wl1["decision"], F)
	checkEquals(wl1["missings"], T)
	checkEquals(wl1["weights"], T)
	checkEquals(wl1["costs"], T)
	checkEquals(wl1["numerics"], T)
	checkEquals(wl1["factors"], T)
	checkEquals(wl1["characters"], F)
	to.string(wl1)
	
	wl2 = make.learner("regr.lm")
	checkEquals(wl2["is.classif"], F)
	checkEquals(wl2["is.regr"], T)
	checkEquals(wl2["id"], "regr.lm")
	checkEquals(wl2["multiclass"], F)
	checkEquals(wl2["probs"], F)
	checkEquals(wl2["decision"], F)
	checkEquals(wl2["missings"], F)
	checkEquals(wl2["weights"], T)
	checkEquals(wl2["costs"], F)
	checkEquals(wl2["numerics"], T)
	checkEquals(wl2["factors"], T)
	checkEquals(wl2["characters"], F)
	to.string(wl2)
	
	fun = function(data) data
	wl3 = make.preproc.wrapper(wl1, fun=fun)
	to.string(wl3)
	
}
