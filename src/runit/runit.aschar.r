test.aschar <- function() {

	ct <- make.classif.task(data=binaryclass.df, target=binaryclass.target)
	as.character(ct)
	
	rt <- make.regr.task(data=regr.df, target=regr.target)
	as.character(rt)
	
	wl = make.learner("lda")
	as.character(wl)
}
