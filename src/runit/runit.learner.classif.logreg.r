test.logreg <- function(){
	
	# "did not converge":
	m <- glm(formula=binaryclass.formula, data=binaryclass.train, family=binomial, maxit=100) 
	
	p <- predict(m, newdata=binaryclass.test, type="response")
	
	p.class <- as.factor(binaryclass.class.levs[ifelse(p > 0.5, 2, 1)])
	
	simple.test("logreg", binaryclass.df, binaryclass.formula, binaryclass.train.inds, p.class)
	
#	prob.test  ("logreg", binaryclass.df, binaryclass.formula, binaryclass.train.inds, p)

	tt <- "logreg"
	tp <- function(model, newdata) predict(model, newdata)$class

	cv.test("logreg", binaryclass.df, binaryclass.formula, tune.train=tt, tune.predict=tp )
}



