#' @include task.learn.r
roxygen()

#' Given a \code{\linkS4class{learn.task}} \code{train} creates a model for the learning machine 
#' which can be used for predictions on new data. 
#'
#' @param learner [\code{\linkS4class{wrapped.learner}} or \code{\link{character}}]\cr 
#'        Learning algorithm.   
#' @param task [\code{\linkS4class{learn.task}}]\cr 
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
#' @usage train(learner, task, subset, parset, vars)  
#'
#' @examples 
#' library(MASS)
#' train.inds <- seq(1,150,2)
#' test.inds <- seq(2,150,2)
#'
#' ct <- make.classif.task(data=iris, target="Species")
#' cm <- train("lda", ct, subset=train.inds)
#' ps <- predict(cm, newdata=iris[test.inds,])
#' 
#' ct <- make.classif.task(data=iris, target="Species")
#' cm <- train("kknn.classif", ct, subset=train.inds, parset=list(k=3))
#' ps <- predict(cm, newdata=iris[test.inds,])
#'  
#' @seealso \code{\link{predict}}, \code{\link{make.classif.task}}, \code{\link{make.regr.task}} 
#' 
#' @title train
#' @rdname train

setGeneric(
		name = "train",
		def = function(learner, task, subset, parset, vars, type) {
			if (is.character(learner))
				learner <- make.learner(learner, task)
			if (missing(subset))
				subset <- 1:task["size"]
			if (missing(parset))
				parset <- list()
			if (missing(vars))
				vars <- task["input.names"]
			if (length(vars) == 0)
				vars <- character(0)
			if (missing(type))
				type = "response"
			standardGeneric("train")
		}
)


train.task2 <- function(learner, task, subset, parset, vars, type, extra.train.pars, model.class, novars.class, check.fct) {

	if(learner@learner.pack != "mlr" && !require(learner@learner.pack, character.only=TRUE)) {
		stop(paste("Learner", learner@learner.name, "could not be constructed! package", learner.pack, "missing!"))
	}
	
	check.result <- check.fct(task, learner)
	if (check.result$msg != "") {
		stop(check.result$msg)
	}
	
	wl <- learner
	tn <- task["target.name"]
	# reduce data to subset and selected vars
	data.subset <- task["data", subset, select=c(vars, tn), drop=F]
	ws <- task["weights"][subset]
	
	logger.debug("mlr train:", wl@learner.name, "with pars:")
	logger.debug(parset)
	logger.debug("on", length(subset), "examples:")
	logger.debug(subset)
	
	# no vars? then use no vars model
	if (length(vars) == 0) {
		wl = new(novars.class)
	}
	
	# make pars list for train call
	pars = list(.wrapped.learner=wl, .target=tn, .data=data.subset, .weights=ws)
	if (is(task, "classif.task"))
		pars$.type = type
	
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
		learner.model <- try(do.call(train.learner, pars), silent=TRUE)
	)
	logger.debug(or)
	
	# if error happened we use a failure model
	if(is(learner.model, "try-error")) {
		msg <- as.character(learner.model)
		warning("Could not train the learner: ", msg)	
		learner.model <- new("learner.failure", msg=msg)
	} 

	pars = list(model.class, wrapped.learner = wl, learner.model = learner.model, 
			data.desc=task@data.desc, task.desc=task@task.desc, subset=subset, parset=parset, vars=vars)
	do.call("new", pars)
}
	

#' @export
setMethod(
		f = "train",
		
		signature = signature(
				learner="wrapped.learner.classif", 
				task="classif.task", 
				subset="numeric", 
				parset="list", 
				vars="character",
				type="character"
		),
		
		def = function(learner, task, subset, parset, vars, type) {
			extra.train.pars = list(.costs = task["costs"])
			train.task2(learner, task, subset, parset, vars, type, 
					extra.train.pars, "wrapped.model.classif", "novars.classif",
					check.task.learner.classif
			)
		}
)

#' @export
setMethod(
		f = "train",
		
		signature = signature(
				learner="wrapped.learner.regr", 
				task="regr.task", 
				subset="numeric", 
				parset="list", 
				vars="character",
				type="character"				
		),
		
		def = function(learner, task, subset, parset, vars, type) {
			extra.train.pars = list()
			train.task2(learner, task, subset, parset, vars, type, 
					extra.train.pars, "wrapped.model.regr", "novars.regr",
					check.task.learner
			)
		}
)

