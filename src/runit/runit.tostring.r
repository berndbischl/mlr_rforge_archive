test.tostring <- function() {
	print(binaryclass.task)
	print(regr.task)
	wl = make.learner("classif.lda")
	print(wl)
  fun = function(data) data
  wl = make.preproc.wrapper(wl, fun=fun)
  print(wl)  
  
}
