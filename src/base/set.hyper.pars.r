#' Set hyperparameters of learner object. 
#' 
#' @param learner [\code{\linkS4class{learner}}]\cr 
#'        Learner object.   
#' @param ... [any] \cr
#'        Optional named (hyper)parameters. Alternatively, you can pass via the "par.vals" argument.
#' @param par.vals [list] \cr
#'       Optional list of named (hyper)parameters. Alternatively, you can pass via the ... argument.
#' 		    
#' @return \code{\linkS4class{learner}} with changed hyperparameters.
#' @exportMethod set.hyper.pars
#' @rdname set.hyper.pars 
#' @title Set hyperparamters of learner object.
 
setGeneric(
		name = "set.hyper.pars",
		def = function(learner, ..., par.vals) {
			if (missing(par.vals))
				par.vals = list()
			par.vals = insert(par.vals, list(...))
			standardGeneric("set.hyper.pars")
		}
)

setMethod(
	f = "set.hyper.pars",
	
	signature = signature(
			learner="learner", 
			par.vals="list" 
	),
	
	def = function(learner, ..., par.vals) {
		ns = names(par.vals)
		pds = learner["par.descs"]
		pds.n = learner["par.descs.name"]
		for (i in seq(length=length(par.vals))) {
			n = ns[i]
			p = par.vals[[i]]
			learner@par.vals[[n]] = p
			if (!(n %in% pds.n)) {		
				warning(class(learner), ": Setting par ", n, " without description!")
				pd = new("par.desc.unknown", par.name=n, when="train", data.type=as.character(NA))
				learner@par.descs = append(learner@par.descs, pd)
			}
		}
		return(learner)
	} 
)


