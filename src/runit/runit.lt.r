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
	
	# trafo
	ct = make.task(data=iris, formula = Species ~ log(Sepal.Length))
	dd = ct["data"]
	checkTrue(setequal(colnames(dd), c("log(Sepal.Length)", "Species")))
	checkEquals(dd[,2], log(iris[, "Sepal.Length"]))
	
}