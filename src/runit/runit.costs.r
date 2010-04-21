test.costs <- function() {
	costs = matrix(c(0,1000,1,0), 2, 2)
	rownames(costs) = colnames(costs) =  c("M", "R")
	ct = make.task(data=binaryclass.df, target=binaryclass.target, costs=costs)
	checkEquals(ct["costs"], costs)
	
#	m1 = train("classif.rpart", task=ct)
#	p1 = predict(m1, task=ct)
#	m2 = rpart(binaryclass.formula, data=binaryclass.df, parms=list(loss=costs))
#	p2 = predict(m2, type="class")
#	names(p2) = NULL
#	checkEquals(p1["response"], p2)
	
	set.seed(1)
	m1 = train("classif.ada", task=ct)
	set.seed(1)
	p1 = predict(m1, task=ct)
	set.seed(1)
	m2 = ada(binaryclass.formula, data=binaryclass.df, parms=list(loss=costs))
	set.seed(1)
	p2 = predict(m2, newdata=binaryclass.df)
	print(table(p1["response"], p2))
	checkEquals(p1["response"], p2)
	#m = train("classif.ada", task=ct)
}