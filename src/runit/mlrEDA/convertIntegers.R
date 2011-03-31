testConvertIntegers = function() {
	data=data.frame(x=as.integer(1:5),y=as.character(c("a","b", "c", "d", "e")), z=c(T,T,T,F,F), stringsAsFactors=FALSE)
	data2 =convertColumns(data)
	
	
}