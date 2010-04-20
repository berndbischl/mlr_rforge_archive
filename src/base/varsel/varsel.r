
#' @export

varsel <- function(learner, task, resampling, method="forward", control=NULL, measures, aggr, model=F) {
	if (missing(measures))
		measures = default.measures(task)
	measures = make.measures(measures)
	
	if (missing(aggr))
		aggr = default.aggr(task)
	aggr = make.aggrs(aggr)
	
	if (is.character(method)) {
		sel.func = switch(method,
				sfs = varsel.seq,
				sbs = varsel.seq,
				sffs = varsel.seq,
				sfbs = varsel.seq,
				random = varsel.random,
				bestcor = varsel.bestcor,
				hybrid = varsel.hybrid,
				hybrid2 = varsel.hybrid2,
				stop(paste("Method", method, "does not exist!"))
		)
	} else {
		sel.func = method
	}	
	assign(".mlr.vareval", 0, envir=.GlobalEnv)
	
	op = sel.func(learner=learner, task=task, resampling=resampling, 
			measures=measures, aggr=aggr, method=method, control=control) 
	
	return(op)
}
