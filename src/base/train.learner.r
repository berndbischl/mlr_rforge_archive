#' @include wrapped.learner.r
roxygen()

setGeneric(
		name = "train.learner",
		def = function(wrapped.learner, formula, data, weights, parset) {
			standardGeneric("train.learner")
		}
)



#' Mainly for internal use. Trains a wrapped learner on a giving training set, 
#' possibly w.r.t. some hyperparamters and case weights. 
#' @param wrapped.learner [\code{\link{wrapped.learner}}] \cr  
#'        Wrapped learner from this package. 
#' @param formula [\code{\link{formula}}] \cr
#' 		  Specifies inputs and output.
#' @param data [\code{\link{data.frame}}] \cr
#' 		  Training set
#' @param weights [\code{\link{numeric}}] \cr
#' 		  Optional case weights, default is 1.
#' @param parset [\code{\link{numeric}}] \cr
#' 		  Named list of hyperparameters. Default is empty list.  
#' @return Model of the underlying learner.
#' @export 
#' @aliases train.learner 
#' @title train.learner 

setMethod(
		f = "train.learner",
		signature = signature(
				wrapped.learner="wrapped.learner", 
				formula="formula", 
				data="data.frame", 
				weights="numeric", 
				parset="list"
		),
		
		def = function(wrapped.learner, formula, data, weights, parset) {

			wl <- wrapped.learner
			g <- wl@train.fct
			
			g.pars <- list(formula, data=data)
			g.pars <- c(g.pars, wl@train.fct.pars)
			# let hyperparamters overwrite train.fct.pars
			for (i in seq(1, along=parset)) {
				pn <- names(parset)[i] 
				g.pars[pn] <- parset[i]
			}
			
			if (wl@learner.props@supports.weights) {
				g.pars$weights = weights 
			}		
			
			m <- do.call(g, g.pars) 		
			return(m)
		}
)
