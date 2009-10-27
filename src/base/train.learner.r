#' @include wrapped.learner.r
roxygen()

setGeneric(
		name = "train.learner",
		def = function(wrapped.learner, target, data, weights, costs, parset) {
			if (missing(costs) && is(wrapped.learner, "wrapped.learner.classif")) {
				n <- length(levels(data[,target]))
				costs <- matrix(1,n,n) - diag(1,n)
			}
			standardGeneric("train.learner")
		}
)



#' Mainly for internal use. Trains a wrapped learner on a giving training set, 
#' possibly w.r.t. some hyperparamters and case weights. 
#' @param wrapped.learner [\code{\link{wrapped.learner}}] \cr  
#'        Wrapped learner from this package. 
#' @param target [\code{\link{character}}] \cr
#' 		  Name of the target variable.
#' @param data [\code{\link{data.frame}}] \cr
#' 		  Training set.
#' @param weights [\code{\link{numeric}}] \cr
#' 		  Optional case weights, default is 1, which means every case is assigned equal weight.
#' 		  Are ignored if the learner does not support weight-sensitive training. 
#' @param costs [\code{\link{matrix}}] \cr
#' 		  Optional misclassification costs, which should be used during training. 
#' 		  Are ignored if the learner is not a classifier or does not support cost-sensitive training.
#'        Default costs are from zero-one-loss, which are used if none are passed.  
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
				target="character", 
				data="data.frame", 
				weights="numeric", 
				costs="ANY", 
				parset="list"
		),
		
		def = function(wrapped.learner, target, data, weights, costs, parset) {
			print(parset)
			wl <- wrapped.learner
			g <- wl@train.fct

			f <- as.formula(paste(target, "~."))
			g.pars <- list(f, data=data)
			g.pars <- c(g.pars, wl@train.fct.pars)
			# let hyperparamters overwrite train.fct.pars
			for (i in seq(1, along=parset)) {
				pn <- names(parset)[i] 
				g.pars[pn] <- parset[i]
			}
			if (!all(weights==1)) {
				if (wl@learner.props@supports.weights) {
					g.pars$weights = weights 
				} else {
					warning("Learner ", wl@learner.name, " does not support case weights, but weights were passed to train.learner!")
				}		
			}
			m <- do.call(g, g.pars) 	
			return(m)
		}
)
