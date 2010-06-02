test.helpers = function() {
	
	xs = list("a", "b")

	checkTrue(check.list.type(xs, "character"))
	checkException(check.list.type(xs, "integer"))
	checkTrue(check.list.type(xs, c("character", "numeric")))
	
	xs = list("a", "b", 1, 2)
	checkTrue(check.list.type(xs, c("character", "numeric")))
	checkException(check.list.type(xs, c("learner", "logical")))
	

	xs = list(make.learner("classif.rpart"))
	checkTrue(check.list.type(xs, c("rlearner")))
}