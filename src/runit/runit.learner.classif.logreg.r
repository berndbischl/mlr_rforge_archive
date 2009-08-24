test.logreg <- function(){
	
	# "did not converge":
	m <- glm(formula=binaryclass.formula, data=binaryclass.train, family=binomial, maxit=500) 
	
	p <- predict(m, newdata=binaryclass.test, type="response")
	
	p.class <- as.factor(binaryclass.class.levs[ifelse(p > 0.5, 2, 1)])
	
	simple.test("logreg", binaryclass.df, binaryclass.formula, binaryclass.train.inds, p.class)

	# not done, annoying to build matrix manually
	#prob.test("logreg", binaryclass.df, binaryclass.formula, binaryclass.train.inds, p)

	tt <- function(formula, data) {glm(formula, data=data, family=binomial, maxit=500)}
	tp <- function(model, newdata) {
		p <- predict(model, newdata)
		as.factor(binaryclass.class.levs[ifelse(p > 0.5, 2, 1)])
	}

	cv.test("logreg", binaryclass.df, binaryclass.formula, tune.train=tt, tune.predict=tp )
}



