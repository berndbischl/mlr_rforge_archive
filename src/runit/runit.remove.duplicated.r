

test.remove.duplicated <- function() {
	
	set.seed(1)
	i <- c(1:5, 51:55, 101:105) 
	df <- iris[i,]
	
#	print(" ")
	
	rin <- make.bs.instance(size=nrow(df), iters=1)
	
	ti <- rin["train.inds", 1]

	ct <- new("classif.task", new("kknn.knn.classif"), data=df, formula=Species~.)
	ct2 <- restrict.learn.task(ct, ti)
	
	cv.inst <- make.cv.instance(size=nrow(ct2@data), iters=2)
	
	rr <- resample.fit(ct2, resample.instance=cv.inst, parset=list(k=1))			
	
#	print(rr)
	
#	print(resample.performance(rr))	

}



