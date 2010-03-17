#' @include resample.instance.r
roxygen()
#' @include resample.instance.make.r
roxygen()
#' @include resample.desc.r
roxygen()
#' @include resample.desc.make.r
roxygen()
#' @include resample.result.r
roxygen()


#' Given a resampling strategy, which defines sets of training and test indices, 
#' \code{resample.fit} fits the selected learner using the training sets and performs predictions for the test sets. These 
#' predictions are returned - encapsulated in a \code{\link{resample.result}} object.
#' Optionally information from the fitted models, e.g. the complete model, can be extracted and returned.
#'
#' @param learner [\code{\linkS4class{wrapped.learner}} or \code{\link{character}}]\cr 
#'        Learning algorithm.   
#' @param task [\code{\linkS4class{learn.task}}] \cr
#'        Learning task.
#' @param resampling [\code{\linkS4class{resample.desc}} or \code{\linkS4class{resample.instance}}] \cr
#'        Resampling strategy. 
#' @param parset [\code{\link{list}}]\cr 
#'        A list of named elements which specify the hyperparameters of the learner.
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
#' @return An object of class \code{\linkS4class{resample.result}}.
#' 
#' @export
#' @rdname resample.fit 
#' 
#' @usage resample.fit(learner, task, resampling, parset, vars, type, extract)
#'
#' @examples
#' ct <- make.classif.task(data=iris, target="Species")
#' res <- make.res.instance("cv", task=ct, iters=3)
#' f1 <- resample.fit("rpart.classif", ct, res)	
#' f2 <- resample.fit("rpart.classif", ct, res, parset=list(minsplit=10, cp=0.03))
#'  
#' @title resample.fit


setGeneric(
		name = "resample.fit",
		def = function(learner, task, resampling, parset, vars, type, extract) {
			if (is.character(learner))
				learner = make.learner(learner, task)
			n = task["size"]
			if (is(resampling, "resample.desc"))
				resampling = new(resampling@instance.class, resampling, size=n)
			r = resampling["size"]
			if (n != r)
				stop(paste("Size of data set:", n, "and resampling instance:", r, "differ!"))
			if (missing(parset))
				parset <- list()
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
setMethod(
		f = "resample.fit",
		signature = signature(learner="wrapped.learner", task="learn.task", resampling="resample.instance", 
				parset="list", vars="character", type="character", extract="function"),
		def = function(learner, task, resampling, parset, vars, type, extract) {
			n = task["size"]
			resample.instance <- resampling
			iters <- resample.instance["iters"]
			
			rs = mylapply(1:iters, resample.fit.iter, from="resample", learner=learner, task=task, rin=resample.instance, parset=parset, vars=vars, type=type, extract=extract)
		
			ps = lapply(rs, function(x) x$pred)
			es = lapply(rs, function(x) x$extracted)
			
			return(new("resample.result", instance=resample.instance, preds=ps, extracted=es))
		}
)

#' @export

setMethod(
		f = "resample.fit",
		signature = signature(learner="wrapped.learner", task="learn.task", resampling="resample.desc", parset="list", vars="character", type="character", extract="function"),
		def = function(learner, task, resampling, parset, vars, type, extract) {
			resample.fit(learner, task, i, parset, vars, type, extract)
		}
)


