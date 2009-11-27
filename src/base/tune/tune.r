#' Internal function for tune.optim
#' one resampling + performance eval
#' @param p passed from optimisation function, we have to change the type to a named list
#' @export 

tune <- function(learner, task, resampling, fixed=list(), method="grid", control=NULL, loss, model=F, scale=I) {	
	if (missing(loss))
		loss = default.loss(task)
	if (is.character(loss))
		loss <- make.loss(loss)
	
	
	if (method == "grid")
		optim.func <- tune.grid
	
	if (method == "pattern")
		optim.func <- tune.ps
	
	if(method == "cmaes")
		optim.func <- tune.cmaes

	export.tune(learner, task, fixed, loss, scale)
	or <- optim.func(learner, task, resampling, loss, control)
	or$par = scale(or$par)
	if (model) {
		parset = c(fixed, or$par)
		or$model = train(learner, task, parset=parset) 	
	}
	
	return(or)			
}