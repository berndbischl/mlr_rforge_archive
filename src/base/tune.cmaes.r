
tune.cmaes <- function(learn.task, resample.instance, measure, init, lower, upper) {
	
	wrapper <- function(parset) {
		resample.result <- resample.fit(learn.task, resample.instance, parset)
		cp <- resample.performance(resample.result, measure=measure)
		return(cp$mean)
	}
	cma.es(init, wrapper, lower, upper)
}
