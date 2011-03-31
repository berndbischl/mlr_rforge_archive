testCapLargeValues = function() {
	data= data.frame(x=1:10, y=c(1:9,Inf), z=c(-11:-20))
	data2 = capLargeValues(data, threshold=10, impute=10)
	checkEquals(data2, data.frame(x=1:10, y=c(1:10), z=rep(-10,10)))
}