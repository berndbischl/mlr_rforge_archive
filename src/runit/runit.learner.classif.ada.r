


test.ada <- function() {
	
	
	set.seed(debug.seed)
	m <- ada(formula=binaryclass.formula, data=binaryclass.train) 
	set.seed(debug.seed)
	p <- predict(m, newdata=binaryclass.test, type="prob")
	p.class <- as.factor(binaryclass.class.levs[ifelse(p[,2] > 0.5, 2, 1)])
	
	simple.test("ada", binaryclass.df, binaryclass.formula, binaryclass.train.inds, p.class)
	
	p = p[,1]
	prob.test("ada", binaryclass.df, binaryclass.formula, binaryclass.train.inds, p)
	
   # more tests!!!!	
	 
}

