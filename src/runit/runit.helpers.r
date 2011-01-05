test.helpers = function() {
	if (!use.package) {
		xs = list("a", "b")
	
		checkTrue(check.list.type(xs, "character"))
		checkException(check.list.type(xs, "integer"))
		checkTrue(check.list.type(xs, c("character", "numeric")))
		
		xs = list("a", "b", 1, 2)
		checkTrue(check.list.type(xs, c("character", "numeric")))
		checkException(check.list.type(xs, c("learner", "logical")))
		
		xs = list(make.learner("classif.rpart"))
		checkTrue(check.list.type(xs, c("rlearner")))
		
		checkTrue(all.els.named(list()))
		checkTrue(all.els.named(list(x=1)))
		checkTrue(all.els.named(list(x=1, y=2)))
		checkTrue(!all.els.named(list(1,2)))
		xs = list(1,2)
		names(xs)[1] = "a"
		checkTrue(!all.els.named(xs))
		names(xs)[2] = "b"
		checkTrue(all.els.named(xs))
	}
}