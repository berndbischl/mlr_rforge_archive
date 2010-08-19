test.mlr.learn.task <- function() {
	
	ct1 <- multiclass.task
	
	checkEquals(ct1["target"], "Species")
	checkEquals(ct1["targets"], multiclass.df[,multiclass.target])
	
	ct = binaryclass.task
	pn = c(ct["positive"], ct["negative"])
	checkEquals(sort(ct["class.levels"]), sort(pn))
	
	# wrong vars
	x=checkException(
			train("classif.lda", multiclass.task, vars=c("Sepal.Length", "x", "y")),		 
			silent=T
	)
	
	# y contains missings
	df = multiclass.df
	df[1, multiclass.target] = NA
	checkException(make.task(data=df, target=multiclass.target), silent=TRUE)
	s = geterrmessage()
	checkTrue(length(grep("Target values contain missings!", s)) >0 )
}