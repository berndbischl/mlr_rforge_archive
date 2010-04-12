#' @include resample.instance.r
roxygen()
#' @include resample.instance.make.r
roxygen()
#' @include resample.desc.r
roxygen()
#' @include resample.desc.make.r
roxygen()
#' @include resample.prediction.r
roxygen()


#' Given a resampling strategy, which defines sets of training and test indices, 
#' \code{resample.fit} fits the selected learner using the training sets and performs predictions for the test sets. 
#' 
#' Optionally information from the fitted models, e.g. the complete model, can be extracted and returned.
#'
#' @param learner [\code{\linkS4class{wrapped.learner}} or \code{\link{character}}]\cr 
#'        Learning algorithm.   
#' @param task [\code{\linkS4class{learn.task}}] \cr
#'        Learning task.
#' @param resampling [\code{\linkS4class{resample.desc}} or \code{\linkS4class{resample.instance}}] \cr
#'        Resampling strategy. 
#' @param parset [list] \cr 
#'        Named list of hyperparamter values. Will overwrite the one specified in the learner object. Default is empty list().
#' @param vars [\code{\link{character}}] \cr 
#'        Vector of variable names to use in training the model. Default is to use all variables.
#' @param type [\code{\link{character}}] \cr 
#' 		Only used for classification tasks; specifies the type of predictions -
#' 		either probability ("prob") or class ("response").
#' @param extract [\code{\link{function}}] \cr 
#' 		Function used to extract information from fitted models, e.g. can be used to save the complete list of fitted models. 
#'      Default is to extract nothing. 
#' 	   
#'             
#' @return \code{\linkS4class{resample.prediction}}.
#' 
#' @export
#' @rdname resample.fit 
#' 
#' @usage resample.fit(learner, task, resampling, parset, vars, type, extract)
#'
#' @title Fit models according to a resampling strategy.


setGeneric(
		name = "resample.fit",
		def = function(learner, task, resampling, parset, vars, type, extract) {
			if (is.character(learner))
				learner = make.learner(learner)
			if (missing(parset))
				parset = list()
			if (missing(vars))
				vars <- task["input.names"]
			if (missing(type))
				type = "response"
			if (missing(extract))
				extract <- function(x){}

			standardGeneric("resample.fit")
		}
)

#' @export
#' @rdname resample.fit 
setMethod(
		f = "resample.fit",
		signature = signature(learner="wrapped.learner", task="learn.task", resampling="resample.instance", 
				parset="list", vars="character", type="character", extract="function"),
		def = function(learner, task, resampling, parset, vars, type, extract) {
			n = task["size"]
			r = resampling["size"]
			if (n != r)
				stop(paste("Size of data set:", n, "and resampling instance:", r, "differ!"))
			
			resample.instance <- resampling
			iters <- resample.instance["iters"]
			
			# let parset overwrite pars in learner
			for (i in seq(1, along=parset)) {
				pn <- names(parset)[i] 
				learner@train.fct.pars[pn] = parset[i]
			}
			
			rs = mylapply(1:iters, resample.fit.iter, from="resample", learner=learner, task=task, rin=resample.instance, vars=vars, type=type, extract=extract)
		
			ps = lapply(rs, function(x) x$pred)
			es = lapply(rs, function(x) x$extracted)
			
			return(new("resample.prediction", instance=resample.instance, preds=ps, extracted=es))
		}
)

#' @export
#' @rdname resample.fit 

setMethod(
		f = "resample.fit",
		signature = signature(learner="wrapped.learner", task="learn.task", resampling="resample.desc", 
				parset="list", vars="character", type="character", extract="function"),
		def = function(learner, task, resampling, parset, vars, type, extract) {
			resampling = make.res.instance(resampling, size=task["size"])
			resample.fit(learner, task, resampling, parset, vars, type, extract)
		}
)


