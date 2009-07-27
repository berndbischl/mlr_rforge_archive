

make.class.t = function(method, data, formula,
		train.fct.pars=list(), 
		predict.fct.pars=list()) {
	
	if (method == "lda") {
		return(new("t.lda", data=data, formula=formula, 
				train.fct.pars=train.fct.pars, predict.fct.pars=predict.fct.pars))
	}
	if (method == "lda") {
		return(new("t.qda", data=data, formula=formula, 
					train.fct.pars=train.fct.pars, predict.fct.pars=predict.fct.pars))		
	} else {
		stop(paste("Trying to generate classification learn.task for of unkown type:", method))
	}
	
	
}
	