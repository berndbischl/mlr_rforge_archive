test.novars <- function() {
	data1 = multiclass.df
	ct <- make.classif.task(formula=multiclass.formula, data=multiclass.df)
	m <- train("lda", ct, vars=c())
	p <- predict(m, newdata=multiclass.df)
}