#' Internal function for tune.optim
#' one resampling + performance eval
#' @param p passed from optimisation function, we have to change the type to a named list
#' @export 

tune <- function(learner, task, resampling, names, fixed=list(), method="grid", control=NULL, loss, model=F, scale=I) {	
	if (missing(loss))
		loss = default.loss(task)
	if (is.character(loss))
		loss <- make.loss(loss)
	
	n.eval <- 0
	
	tune.wrapper <- function(p, resampling) {
		parset.scaled   <- as.list(scale(p))
		names(parset.scaled) <- names
		parset = c(fixed, parset.scaled)
		rr <- resample.fit(learner, task, resampling, parset)
		cp <- resample.performance(task, rr, loss)
		z = cp$aggr1
		n.eval <<- n.eval + 1
		#print(n.eval)
		logger.debug("parset ", as.character(parset))
		logger.debug("mean error = ", z)
		logger.debug("Number of evaluations: ", n.eval)
		return(z)
	}
	
	if (method == "grid")
		optim.func <- tune.grid
	
	if (method == "pattern")
		optim.func <- tune.ps
	
	if(method == "cmaes")
		optim.func <- tune.cmaes
	
	or <- optim.func(learner, task, loss=loss, resampling=resampling, control=control, tune.wrapper)
	names(or$par) = names
	or$par = scale(or$par)
	if (model) {
		parset = c(fixed, or$par)
		or$model = train(learner, task, parset=parset) 	
	}
	
	or$n.eval <- n.eval
	return(or)			
}