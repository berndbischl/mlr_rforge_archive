test.tostring <- function() {
	print(binaryclass.task)
	print(regr.task)
	wl = makeLearner("classif.lda")
	print(wl)
  f1 = function(data, targetvar, args) list(data=data, control=list()) 
  f2 = function(data, targetvar, args, control) data
  wl = makePreprocWrapper(wl, train=f1, predict=f2)
  print(wl)  
  
}
