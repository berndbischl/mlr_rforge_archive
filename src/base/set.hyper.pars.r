set.hyper.pars = function(learner, parset, types) {
	if (missing(types) || types=="train") {  
		types = rep("train", length(parset))
		names(types) = names(parset)
	}
	learner@hyper.pars = insert(learner@hyper.pars, parset)
	learner@hyper.types = insert(learner@hyper.types, types)
	return(learner)
} 