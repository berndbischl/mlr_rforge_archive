test.novars <- function() {
	m <- train("classif.lda", multiclass.task, vars=c())
	p <- predict(m, newdata=multiclass.df)
}