test.aschar <- function() {

	ct <- make.task(data=binaryclass.df, target=binaryclass.target)
	to.string(ct)
	
	rt <- make.task(data=regr.df, target=regr.target)
	to.string(rt)
	
	wl = make.learner("classif.lda")
	to.string(wl)
}
