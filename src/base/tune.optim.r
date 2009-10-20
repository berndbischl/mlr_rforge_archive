#' Internal function for tune.optim
#' one resampling + performance eval
#' @param p passed from optimisation function, we have to change the type to a named list


tune.optim <- function(learn.task, resample.instance, measure,
		method, start, lower=rep(-Inf, length(start)),	upper=rep(Inf, length(start)),
		control=NULL) {
	
	measure = make.default.measure(learn.task)
	
	n.eval <- 0
	
	tune.wrapper <- function(p) {
		parset <- as.list(p)
		names(parset) <- names(start)
		rr <- resample.fit(learn.task, resample.instance, parset)
		cp <- resample.performance(learn.task, resample.instance, rr, measure)
		z = cp$aggr
		n.eval <<- n.eval + 1
		logger.debug("parset ", as.character(parset))
		logger.debug("mean error = ", z)
		logger.debug("Number of evaluations: ", n.eval)
		return(z)
	}
	
	if (method == "pattern")
		optim.func <- ps.wrapper
	
	if(method == "cmaes")
		optim.func <- cmaes.wrapper
	
	if(method == "subplex")
		optim.func <- subplex.wrapper
	
	start2 <- as.numeric(start) 
	names(start2) <- names(start) 
	or <- optim.func(f=tune.wrapper, start=start2, lower=lower, upper=upper, control=control)
	or$n.eval <- n.eval
	return(or)			
}