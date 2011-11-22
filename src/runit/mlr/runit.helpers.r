test.helpers = function() {
	if (!use.package) {
		xs = list("a", "b")
	
		checkTrue(checkListElementClass(xs, "character"))
		checkException(checkListElementClass(xs, "integer"))
		checkTrue(checkListElementClass(xs, c("character", "numeric")))
		
		xs = list("a", "b", 1, 2)
		checkTrue(checkListElementClass(xs, c("character", "numeric")))
		checkException(checkListElementClass(xs, c("Learner", "logical")))
		
		xs = list(makeLearner("classif.rpart"))
		checkTrue(checkListElementClass(xs, c("rlearner")))
		
		checkTrue(isProperlyNamed(list()))
		checkTrue(isProperlyNamed(list(x=1)))
		checkTrue(isProperlyNamed(list(x=1, y=2)))
		checkTrue(!isProperlyNamed(list(1,2)))
		xs = list(1,2)
		names(xs)[1] = "a"
		checkTrue(!isProperlyNamed(xs))
		names(xs)[2] = "b"
		checkTrue(isProperlyNamed(xs))
	}
}