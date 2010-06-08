test.check.data <- function() {
	checkException(
			ct <- make.task(data=binaryclass.df, target= "foo"), 
			silent=TRUE
	)
	s = geterrmessage()
	checkTrue(length(grep("foo", s)) >0 )
	
	mydata = binaryclass.df

	colnames(mydata)[1] = "foo[bar]"
	checkException(make.task(data=mydata, target=binaryclass.target), silent=TRUE)
	s = geterrmessage()
	checkTrue(length(grep("Column names should not contain", s)) >0 )
	colnames(mydata)[1] = "foo,"
	checkException(make.task(data=mydata, target=binaryclass.target), silent=TRUE)
	s = geterrmessage()
	checkTrue(length(grep("Column names should not contain", s)) >0 )
	colnames(mydata)[1] = "foo(bar"
	checkException(make.task(data=mydata, target=binaryclass.target), silent=TRUE)
	s = geterrmessage()
	checkTrue(length(grep("Column names should not contain", s)) >0 )
	colnames(mydata)[1] = "foo bar"
	checkException(make.task(data=mydata, target=binaryclass.target), silent=TRUE)
	s = geterrmessage()
	checkTrue(length(grep("Column names should not contain", s)) >0 )
}