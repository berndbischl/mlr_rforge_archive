#' @include task.learn.r
roxygen()

#' Given a \code{\linkS4class{learn.task}}, creates a model for the learning machine 
#' which can be used for predictions on new data. 
#'
#' @param learner [\code{\linkS4class{learner}} or string]\cr 
#'        Learning algorithm. See \code{\link{learners}}.  
#' @param task [\code{\linkS4class{learn.task}}]\cr 
#'        Specifies learning task.   
#' @param subset [\code{\link{integer}}] \cr 
#'        An index vector specifying the training cases to be used for fitting. By default the complete data set is used. 
#' @param par.vals [list] \cr 
#'        Named list of hyperparameter values. Will overwrite the ones specified in the learner object. Default is empty list.
#' @param vars [\code{\link{character}}] \cr
#'       Vector of variable names to use in training the model. Default is to use all variables, except the excluded in the task.
#' @param type [string] \cr
#'        Classification: "response" | "prob" | "decision", specifying the type to predict later.
#' 		  Default is "response". Very rarely you have to set this during training as well, as the fitted models differ.	 
#'
#' @return \code{\linkS4class{wrapped.model}}. 
#'
#' @export
#'
#' @usage train(learner, task, subset, par.vals, vars, type)  
#'
#' @seealso \code{\link{predict}}
#' 
#' @title Train a learning algorithm.
#' @rdname train

setGeneric(
		name = "train",
		def = function(learner, task, subset, par.vals, vars, type) {
			if (is.character(learner))
				learner <- make.learner(learner)
			if (missing(par.vals))
				par.vals = list()
			if (missing(subset))
				subset <- 1:task["size"]
			if (missing(vars))
				vars <- task["input.names"]
			if (length(vars) == 0)
				vars <- character(0)
			if (missing(type))
				type = "response"
			standardGeneric("train")
		}
)


train.task2 <- function(learner, task, subset, par.vals, vars, type, extra.train.pars, check.fct) {

	# todo: do we still need this, and the loading when exporting a learner? 
	# pack is loaded when learner is constructed
	# export: probably yes...
	if(learner["pack"] != "mlr" && !require(learner["pack"], character.only=TRUE)) {
		stop(paste("Learner", learner["id"], "could not be constructed! package", learner["pack"], "missing!"))
	}
	
	check.result <- check.fct(task, learner)
	if (check.result$msg != "") {
		stop(check.result$msg)
	}
	
	wl <- learner
	tn <- task["target.name"]
		
	
	# reduce data to subset and selected vars
	x = setdiff(vars, task["input.names"])
	if (length(x) > 0)
		stop("Trying to train with vars which are not inputs: ", paste(x, collapse=","))
	data.subset <- task["data", row=subset, col=c(vars, tn), drop=F]
	
	# todo: maybe don't pass weights for performance reasons when none set?
	if (task["has.weights"])
		ws = task["weights"][subset]
	else
		ws = rep(1, length(subset)) 
	
	wl = set.hyper.pars(wl, par.vals=par.vals)
	
	# make pars list for train call
	pars = list(.learner=wl, .target=tn, .data=data.subset, .data.desc=task@data.desc, .task.desc=task@task.desc, .weights=ws)
	# only pass train hyper pars to rlearner
	hps = wl["par.vals", par.when="train"]
	pars = c(pars, extra.train.pars, hps)
	
	logger.debug(level="train", "mlr train:", wl["id"], "with pars:")
	logger.debug(level="train", hps)
	logger.debug(level="train", "on", length(subset), "examples:")
	logger.debug(level="train", subset)
	
	# no vars? then use no vars model
	if (length(vars) == 0) {
		learner.model = new("novars", targets=data.subset[, tn], data.desc=task@data.desc, task.desc=task@task.desc)
		time.train = 0
	} else {
		# set the seed
		if(!is.null(.mlr.local$debug.seed)) {
			set.seed(.mlr.local$debug.seed)
			warning("DEBUG SEED USED! REALLY SURE YOU WANT THIS?")
		}
		
		st = system.time(or <- capture.output(
							learner.model <- try(do.call(train.learner, pars), silent=TRUE)
						), gcFirst = FALSE)
		logger.debug(level="train", or)
		time.train = st[3]
	}
	
	
	# if error happened we use a failure model
	if(is(learner.model, "try-error")) {
		msg <- as.character(learner.model)
		warning("Could not train the learner: ", msg)	
		learner.model <- new("learner.failure", msg=msg)
		time.train = as.numeric(NA)
	} 
	
	#set to "train" if not specified
	hyper.types = rep("train", length(hps))
	names(hyper.types) = names(hps)
	hyper.types = insert(hyper.types, wl["hyper.types"])
	
	new("wrapped.model", learner = wl, learner.model = learner.model, 
			data.desc=task@data.desc, task.desc=task@task.desc, subset=subset, 
			vars=vars, time = time.train)
}
	

#' @export
#' @rdname train 
setMethod(
		f = "train",
		
		signature = signature(
				learner="learner", 
				task="learn.task", 
				subset="numeric", 
				par.vals="list",
				vars="character",
				type="character"
		),
		
		def = function(learner, task, subset, par.vals, vars, type) {
			if (is(task, "classif.task")) {
				extra.train.pars = list(.costs = task["costs"])
				ctf = check.task.learner.classif
			} else {
				extra.train.pars = list()
				ctf = check.task.learner
			}
			train.task2(learner, task, subset, par.vals, vars, type, extra.train.pars, ctf)
		}
)
