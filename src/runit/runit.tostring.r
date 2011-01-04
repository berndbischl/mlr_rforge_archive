test.tostring <- function() {
	print(binaryclass.task)
	print(regr.task)
	wl = make.learner("classif.lda")
	print(wl)
  f1 = function(data, targetvar, args) list(data=data, control=list()) 
  f2 = function(data, targetvar, args, control) data
  wl = make.preproc.wrapper(wl, train=f1, predict=f2)
  print(wl)  
  
}
