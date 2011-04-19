test.mlr.learn.task <- function() {
	
	ct1 <- multiclass.task
	
	checkEquals(ct1@desc@target, "Species")
	checkEquals(getTargets(ct1), multiclass.df[,multiclass.target])
	
	ct = binaryclass.task
	pn = c(ct@desc@positive, ct@desc@negative)
	checkEquals(sort(getClassLevels(ct)), sort(pn))
	
	# wrong vars
	checkError(subset(multiclass.task, vars=c("Sepal.Length", "x", "y")), "undefined columns")
	
	# check missing accessors
	df = multiclass.df
	df[1,1:3] = NA
	df[2,1:3] = NA
	ct = makeClassifTask(data=df, target=multiclass.target)	
	checkTrue(ct["has.missing"])
}
