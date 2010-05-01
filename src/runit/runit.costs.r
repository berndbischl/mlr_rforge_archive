test.costs <- function() {
	cc = matrix(c(0,3,77,0), 2, 2)
	colnames(cc) = rownames(cc) = levels(binaryclass.df[,binaryclass.target])
	ct = make.task(data=binaryclass.df, target= binaryclass.target, costs=cc)
	
	m = train("classif.rpart", task=ct)
	m2 = rpart(binaryclass.formula, data=binaryclass.df, parms=list(loss=cc))
	checkEquals(m["learner.model"]$splits, m2$splits)}