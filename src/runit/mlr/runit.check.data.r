test.check.data <- function() {
	checkException(
			ct <- makeClassifTask(data=binaryclass.df, target= "foo"), 
			silent=TRUE
	)
	s = geterrmessage()
	checkTrue(length(grep("foo", s)) >0 )
}