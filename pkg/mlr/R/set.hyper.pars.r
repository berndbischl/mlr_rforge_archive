set.hyper.pars = function(learner, parset, types) {
	if (missing(types)) 
		types = "train"
	if(length(types) == 1) {  
		types = rep(types, length(parset))
		names(types) = names(parset)
	}
	learner@hyper.pars = insert(learner@hyper.pars, parset)
	learner@hyper.types = insert(learner@hyper.types, types)
	return(learner)
} 