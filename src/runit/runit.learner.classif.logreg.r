test.logreg <- function(){
	
	# "did not converge":
	m <- glm(formula=binaryclass.formula, data=binaryclass.train, family=binomial) 
	
	p <- predict(m, newdata=binaryclass.test)
	
	simple.test("logreg", binaryclass.df, binaryclass.formula, binaryclass.train.inds, p$class)
	
	prob.test  ("logreg", binaryclass.df, binaryclass.formula, binaryclass.train.inds, p$posterior)

	tt <- "logreg"
	tp <- function(model, newdata) predict(model, newdata)$class

	cv.test("logreg", binaryclass.df, binaryclass.formula, tune.train=tt, tune.predict=tp )
}



