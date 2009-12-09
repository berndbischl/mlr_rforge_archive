test.check.data <- function() {
	checkException(
			ct <- make.classif.task(data=binaryclass.df, target= "foo"), 
			silent=TRUE
	)
	checkTrue(length(grep("foo", geterrmessage()))>0)
	
}