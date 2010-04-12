test.costs <- function() {
	mydata = binaryclass.df
	mydata[, binaryclass.target] = as.character(mydata[, binaryclass.target]) 
	ct = make.task(data=binaryclass.df, target= binaryclass.target)
	checkEquals(ct["costs"], matrix(c(0,1,1,0), 2, 2))
	
}