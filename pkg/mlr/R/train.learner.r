#' @include wrapped.learner.r
roxygen()

#' @export
setGeneric(
		name = "train.learner",
		def = function(wrapped.learner, formula, data, weights, parset) {
			standardGeneric("train.learner")
		}
)

setMethod(
		f = "train.learner",
		signature = c(
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
