#' @include resample.instance.r
#' @include resample.result.r
roxygen()




setGeneric(
		name = "resample.fit",
		def = function(learn.task, resample.instance, parset, models, type) {
			if (missing(type))
				type <- "class"
			if (missing(parset))
				parset <- list()
			if (missing(models))
				models <- FALSE
			standardGeneric("resample.fit")
		}
)

#' Given the training and test indices (e.g. generated by cross-validation and generally specified by the \code{linkS4class{resample.instance}} object) \code{resample.fit} 
#' fits the selected learner using the training sets and performs predictions for the test sets. These 
#' predictions are returned - encapsulated in a \code{\link{resample.result}} object. 
#' Optionally the fitted models are also stored.
#'
#' @param learn.task [\code{\linkS4class{learn.task}}] \cr
#'   Specifies the learning task for the problem.
#' @param resample.instance [\code{\linkS4class{resample.instance}}] \cr
#'   Specifies the training and test indices of the resampled data. 
#' @param parset [list]\cr A list of named elements which specify the hyperparameters of the learner.     
#' @param models [logical] \cr If TRUE a list of the fitted models is included in the result.
#' @param type [character] \cr 
#' 		Only used for classification tasks; specifies the type of the predicitons -
#' 		either probability ("probs") or class ("class").
#' 	   
#'             
#' @return An object of class \code{\linkS4class{resample.result}}.
#' 
#' @export
#' @rdname resample.fit 
#' 
#' @usage resample.fit(learn.task, resample.instance, parset, models, type)
#'
#' @examples
#' library(mlr) 
#' ct1 <- make.classif.task("lda", data=iris, formula=Species~.)
#' ct2 <- make.classif.task("rpart.classif", data=iris, formula=Species~.)
#' rin <- make.cv.instance(iters=3, size=nrow(iris))
#' f1 <- resample.fit(ct1, resample.instance=rin)	
#' f2 <- resample.fit(ct2, resample.instance=rin, parset=list(minsplit=10, cp=0.03))
#'  
#' @title resample.fit

setMethod(
		f = "resample.fit",
		signature = signature(learn.task="learn.task", resample.instance="resample.instance", parset="list", models="logical", type="character"),
		def = function(learn.task, resample.instance, parset, models, type) {
			df <- learn.task@data
			n <- nrow(df)  
			ps <- list()
			ms <- list()
			iters <- resample.instance["iters"]
			
			wrapper <- function(i) {
				resample.fit.iter(learn.task, resample.instance, parset, type, i, return.model=models)
			}
			
			if (.parallel.setup$mode == "snowfall" && .parallel.setup$level == "resample") {
				sfExport("parset")
				if (!is.null(parent.frame()$caller) && !parent.frame()$caller == "tune") {
					sfExport("learn.task")
					sfExport("resample.instance")
				}
				rs <- sfClusterApplyLB(1:iters, wrapper)
			} else {
				rs <- lapply(1:iters, wrapper)
			}
			ps = lapply(rs, function(x) x$pred)
			if (models)			
				ms = lapply(rs, function(x) x$model)
			else
				ms = list()
			return(new("resample.result", ri.name=resample.instance["name"], ri.class=class(resample.instance), preds=ps, models=ms))
		}
)



