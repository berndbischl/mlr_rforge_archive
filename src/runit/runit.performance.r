test.performance <- function() {

	data2 = multiclass.df
	
	ct <- make.classif.task(data=multiclass.df, target=multiclass.target)
	m <- train("lda", ct)
	p <- predict(m, newdata=data2)
	
	p1=performance(data2[, multiclass.target], p)
	data2[, multiclass.target] = as.character(data2[, multiclass.target])
	p2=performance(data2[, multiclass.target], p)
	checkEquals(p1$aggr, p2$aggr)
}