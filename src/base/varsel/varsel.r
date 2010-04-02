

varsel <- function(learner, task, resampling, method="forward", control=NULL, measures, aggr, model=F) {
	if (missing(measures))
		measures = default.measures(task)
	measures = make.measures(measures)
	
	if (missing(aggr))
		aggr = default.aggr(task)
	aggr = make.aggrs(aggr)
	
	sel.func = switch(method,
			sfs = varsel.seq,
			sbs = varsel.seq,
			sffs = varsel.seq,
			sfbs = varsel.seq,
			random = varsel.random,
			stop(paste("Method", method, "does not exist!"))
	)
	
	op = sel.func(learner=learner, task=task, resampling=resampling, 
			measures=measures, aggr=aggr, method=method, control=control) 
	
	return(op)
}
