test.novars <- function() {
	data1 = multiclass.df
	ct <- make.classif.task("lda", formula=multiclass.formula, data=multiclass.df)
	m <- train(ct, vars=c())
	p <- predict(m, newdata=multiclass.df)
}