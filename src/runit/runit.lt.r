test.mlr.learn.task <- function() {
	
	ct1 <- multiclass.task
	
	checkEquals(ct1["target"], "Species")
	checkEquals(ct1["targets"], multiclass.df[,multiclass.target])
	
	ct = binaryclass.task
	pn = c(ct["positive"], ct["negative"])
	checkEquals(sort(ct["classes"]), sort(pn))
	
	# wrong vars
	x=checkException(
			train("classif.lda", multiclass.task, vars=c("Sepal.Length", "x", "y")),		 
			silent=T
	)
}