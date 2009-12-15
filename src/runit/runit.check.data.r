test.check.data <- function() {
	checkException(
			ct <- make.classif.task(data=binaryclass.df, target= "foo"), 
			silent=TRUE
	)
	s = geterrmessage()
	checkTrue(length(grep("foo", s)) >0 )
	
	mydata = binaryclass.df
	colnames(mydata)[1] = "foo(bar)"
	ct <- make.classif.task(data=mydata, target=binraryclass.target)
	
}