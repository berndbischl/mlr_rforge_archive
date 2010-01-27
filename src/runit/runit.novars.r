test.novars <- function() {
	m <- train("lda", multiclass.task, vars=c())
	p <- predict(m, newdata=multiclass.df)
}