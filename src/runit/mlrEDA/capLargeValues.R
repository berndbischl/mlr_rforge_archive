testCapLargeValues = function() {
	data= data.frame(x=1:10, y=c(1:9,Inf), z=c(-11:-20))
	data2 = capLargeValues(data, threshold=10, impute=10)
	data3 = capLargeValues(data, threshold=50, impute=10, cols="y")
	data4 = capLargeValues(data, threshold=1, impute=2, cols=3)
	checkEquals(data2, data.frame(x=1:10, y=c(1:10), z=rep(-10,10)))
	checkEquals(data3, data.frame(x=1:10, y=c(1:9,50), z=c(-11:-20) ))
	checkEquals(data4, data.frame(x=1:10, y=c(1:9,Inf), z=rep(-2,10)) )
}