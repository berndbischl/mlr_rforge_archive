tune.ps <- function(learn.task, resample.instance, start, measure) {
	measure = make.default.measure(learn.task)
	wrapper <- function(p) {
		parset <- as.list(p)
		names(parset) <- names(start)
		resample.result <- resample.fit(learn.task, resample.instance, parset)
		cp <- resample.performance(learn.task=learn.task, resample.instance=resample.instance, resample.result=resample.result, measure=measure)
		print("mean error")
		print(cp$aggr)
		return(cp$aggr)
	}
	
	pattern.search(f=wrapper, start=as.numeric(start), stop=10^(-3), Nmax=10, expansion=0.5)	
}
