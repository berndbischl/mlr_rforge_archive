test.tostring <- function() {

	ct <- make.classif.task(data=binaryclass.df, target=binaryclass.target)
	to.string(binaryclass.task)
	
	to.string(regr.task)
	
	wl = make.learner("lda")
	to.string(wl)
}
