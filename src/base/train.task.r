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
#' @usage train(learn.task, subset, parset, vars)  
#'
#' @examples 
#' library(MASS)
#' train.inds <- seq(1,150,2)
#' test.inds <- seq(2,150,2)
#'
#' ct <- make.classif.task("lda", data=iris, target="Species")
#' cm <- train(ct, subset=train.inds)
#' ps <- predict(cm, newdata=iris[test.inds,])
#' 
#' ct <- make.classif.task("kknn.classif", data=iris, target="Species")
#' cm <- train(ct, subset=train.inds, parset=list(k=3))
#' ps <- predict(cm, newdata=iris[test.inds,])
#'  
#' @seealso \code{\link{predict}}, \code{\link{make.classif.task}}, \code{\link{make.regr.task}} 
#' 
#' @title train
#' @rdname train

setGeneric(
		name = "train",
		def = function(learner, learn.task, subset, parset, vars) {
			if (is.character(learner))
				learner <- new(learner)
			if (missing(subset))
				subset <- 1:nrow(learn.task@data)
			if (missing(parset))
				parset <- list()
			if (missing(vars))
				vars <- learn.task["input.names"]
			if (length(vars) == 0)
				vars <- character(0)
			standardGeneric("train")
		}
)


train.task2 <- function(learner, learn.task, subset, parset, vars, extra.train.pars, model.class, extra.model.pars, novars.class, check.fct) {
	
	check.result <- check.fct(learn.task, learner)
	if (check.result$msg != "") {
		stop(check.result$msg)
	}
	
	wl <- learner
	tn <- learn.task["target.name"]
	# reduce data to subset and selected vars
	data.subset <- learn.task@data[subset, c(vars, tn), drop=FALSE]
	ws <- learn.task@weights[subset]
	
	logger.debug("mlr train:", wl@learner.name, "with pars:")
	logger.debug(parset)
	logger.debug("on", length(subset), "examples:")
	logger.debug(subset)
	
	# no vars? then use no vars model
	if (length(vars) == 0) {
		wl = new(novars.class)
	}
	
	# make pars list for train call
	pars <- list(.wrapped.learner=wl, .target=tn, .data=data.subset, .weights=ws)
	pars <- c(pars, extra.train.pars, wl@train.fct.pars)
	# let hyperparamters overwrite pars
	for (i in seq(1, along=parset)) {
		pn <- names(parset)[i] 
		pars[pn] <- parset[i]
	}
	
	# set the seed
	if(!is.null(.mlr.local$debug.seed)) {
		set.seed(.mlr.local$debug.seed)
		warning("DEBUG SEED USED! REALLY SURE YOU WANT THIS?")
	}
	or <- capture.output(
		learner.model <- try(do.call(train.learner, pars))
	)
	logger.debug(or)
	
	# if error happened we use a failure model
	if(is(learner.model, "try-error")) {
		msg <- as.character(learner.model)
		warning("Could not train the learner: ", msg)	
		learner.model <- new("learner.failure", msg=msg)
	} 

	pars = list(model.class, task.class = class(learn.task), wrapped.learner = wl,  
			learner.model = learner.model, target=tn, subset=subset, parset=parset, vars=vars)
	pars = c(pars, extra.model.pars)
	do.call("new", pars)
}
	

#' @export
setMethod(
		f = "train",
		
		signature = signature(
				learner="wrapped.learner.classif", 
				learn.task="classif.task", 
				subset="numeric", 
				parset="list", 
				vars="character"),
		
		def = function(learner, learn.task, subset, parset, vars) {
			extra.train.pars = list(.costs = learn.task@costs, .type = learn.task@type)
			extra.model.pars = list(class.levels = learn.task["class.levels"], type = learn.task@type)
			train.task2(learner, learn.task, subset, parset, vars, 
					extra.train.pars, "wrapped.model.classif", extra.model.pars, "novars.classif",
					check.task.classif
			)
		}
)

#' @export
setMethod(
		f = "train",
		
		signature = signature(
				learner="wrapped.learner.regr", 
				learn.task="regr.task", 
				subset="numeric", 
				parset="list", 
				vars="character"),
		
		def = function(learner, learn.task, subset, parset, vars) {
			extra.train.pars = list()
			extra.model.pars = list()
			train.task2(learn.task, subset, parset, vars, extra.train.pars, "wrapped.model.regr", extra.model.pars, "novars.regr")
		}
)

