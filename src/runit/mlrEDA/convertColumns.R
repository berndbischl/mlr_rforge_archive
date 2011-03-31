testConvertColumns = function() {
	data = data.frame(x=as.integer(1:5), y=as.character(c("a","b", "c", "d", "e")), 
    z=c(TRUE, TRUE, TRUE, FALSE, FALSE), stringsAsFactors=FALSE)
	data2 = convertColumns(data)
	checkEquals(data2$x, as.numeric(data$x))
  checkEquals(data2$y, as.factor(data$y))
  checkEquals(data2$z, as.factor(data$z))
  
  data2 = convertColumns(data, integers.as=identity, logicals.as=as.numeric)
  checkEquals(data2$x, data$x)
  checkEquals(data2$y, as.factor(data$y))
  checkEquals(data2$z, as.numeric(data$z))
}