test.blackboost.classif <- function(){
	
	
	m = blackboost(formula=binaryclass.formula, data=binaryclass.train, family=AdaExp()) 
	p.class = predict(m, newdata=binaryclass.test, type="class")
	
	simple.test("blackboost.classif", binaryclass.df, binaryclass.formula, binaryclass.train.inds, p.class)
	
}



