#' @include task.learn.r
roxygen()

#' Given a \code{\linkS4class{learn.task}} \code{train} creates a model for the learning machine 
#' which can be used for predictions on new data. 
#'
#' @param learn.task [\code{\linkS4class{learn.task}}]\cr 
#'        Specifies learning task.   
#' @param subset [\code{\link{integer}}] \cr 
#'        An index vector specifying the training cases from the data contained in the learning task. By default the complete dataset is used. 
#' @param parset [\code{\link{list}}] \cr
#'       Named list which contains the hyperparameters of the learner. Default is an empty list, which means no hyperparameters are specifically set and defaults of the underlying learner are used.
#' @param vars [\code{\link{character}}] \cr
#'       Vector of variable names to use in training the model. Default is to use all variables.
#'
#' @return An object of class \code{\linkS4class{wrapped.model}} containing the generated model of the underlying learner and the paramater and index set used for training. 
#'
#' @export
#'
#' @usage train(learn.task, subset, parset)  
#'
#' @examples 
#' library(MASS)
#' train.inds <- seq(1,150,2)
#' test.inds <- seq(2,150,2)
#'
#' ct <- make.classif.task("lda", data=iris, formula=Species~.)
#' cm <- train(ct, subset=train.inds)
#' ps <- predict(ct, cm, newdata=iris[test.inds,])
#' 
#' ct <- make.classif.task("kknn.knn.classif", data=iris, formula=Species~.)
#' cm <- train(ct, subset=train.inds, parset=list(k=3))
#' ps <- predict(ct, cm, newdata=iris[test.inds,])
#'  
#' @seealso \code{\link{predict}}, \code{\link{make.classif.task}}, \code{\link{make.regr.task}} 
#' 
#' @title train

setGeneric(
		name = "train",
		def = function(learn.task, subset, parset, vars) {
			if (missing(subset))
				subset <- 1:nrow(learn.task@data)
			if (missing(parset))
				parset <- list()
			if (missing(vars))
				vars <- learn.task["input.names"]
			standardGeneric("train")
		}
)



train.generic <- function(learn.task, wrapped.learner, subset, parset, vars) {
	
	wl <- wrapped.learner
	tn <- learn.task["target.name"]
	data.subset <- learn.task@data[subset, c(vars, tn), drop=FALSE]
	ws <- learn.task@weights[subset]
	if(exists("debug.seed") && !is.null(debug.seed)) {
		set.seed(debug.seed)
		logger.warn("DEBUG SEED USED!!!REALLY SURE?")
	}
	logger.debug("mlr train:", wl@learner.name, "with pars:")
	logger.debug(parset)
	logger.debug("on", length(subset), "examples:")
	logger.debug(subset)
	
	if (length(vars) > 0)
		learner.model <- train.learner(wrapped.learner=wl, formula=learn.task@formula, data=data.subset, weights=ws, parset=parset)
	else {
		if (is(learn.task, "classif.task"))
			learner.model <- new("novars.model.classif", targets=data.subset[, tn])
		else if (is(learn.task, "regr.task"))
			learner.model <- new("novars.model.regr", targets=data.subset[, tn])
		else
			stop("Novars model not implemented for this task!")
	}
	
	
	if(class(learner.model)[1]=="try-error") {
		msg <- as.character(learner.model)
		logger.warn("Could not train the method: ", msg)	
		learner.model <- new("learner.failure", msg=msg)
	} 
	
	return(new("wrapped.model", task.class = class(learn.task), learner.class = class(wl),  
					learner.name=wl@learner.name, learner.model = learner.model, 
					subset=subset, parset=parset, vars=vars))
} 
