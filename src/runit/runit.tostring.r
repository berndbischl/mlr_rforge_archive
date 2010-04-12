test.tostring <- function() {

	ct <- make.task(data=binaryclass.df, target=binaryclass.target)
	to.string(binaryclass.task)
	
	to.string(regr.task)
	
	wl = make.learner("classif.lda")
	to.string(wl)
}
