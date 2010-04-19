#' include wrapped.model.r
roxygen()

#' Predict the target variable of new data using a fitted model. 
#' 
#' @param object [\code{\linkS4class{wrapped.model}}] \cr 
#'        Wrapped model, trained from a learn task.  
#' @param task [\code{\linkS4class{learn.task}}]\cr 
#'        Specifies learning task. If this is passed, data from this task is predicted.   
#' @param subset [integer] \cr 
#'        Index vector to subset the data in the task to use for prediction. 
#' @param newdata [\code{\link{data.frame}}] \cr 
#'        New observations which should be predicted. Alternatively pass this instead of task. 
#' @param type [character] \cr
#'        Classification: vector of "response" | "prob" | "decision", specifying the types to predict.
#'        Default is "response".
#' 		  Ignored for regression.	 
#' @return \code{\linkS4class{prediction}}.
#'
#' @export
#' @rdname predict
#' @importFrom stats predict
#' @seealso \code{\link{train}}
#' @title Predict new data.

setMethod(
		f = "predict",
		signature = signature(object="wrapped.model"),
		def = function(object, task, newdata, subset, type) {
			
			if (!missing(task) && !missing(newdata)) 
				stop("Pass either a task object or a newdata data.frame to predict, but not both!")
			
			if (missing(newdata)) {
				if (missing(subset))
					subset = 1:task["size"]
				newdata = task["data", subset]
			}
			if (missing(type))
				type = "response"
			
			model <- object
			wl <- model@wrapped.learner
			td = model@task.desc
			dd = model@data.desc
			
			cns = colnames(newdata)
			tn = td["target"]
			if (tn %in% cns)
				truth = newdata[, tn]
			else
				truth = NULL
			
			# drop target col
			newdata <- newdata[, -which(cns == tn)]					
			if (is(model, "wrapped.model.classif")) {
				if ("prob" %in% type && !wl@props@supports.probs) {
					stop("Trying to predict probs, but ", wl["id"], " does not support that!")
				}
				if ("decision" %in% type && !wl@props@supports.decision) {
					stop("Trying to predict decision values, but ", wl["id"], " does not support that!")
				}
			}

			logger.debug("mlr predict:", wl["id"], "with pars:")
			logger.debug(wl@predict.fct.pars)
			logger.debug("on", nrow(newdata), "examples:")
			logger.debug(rownames(newdata))
			
			if (is(model, "wrapped.model.classif")) {
				levs = dd["class.levels"]
			}

			
			response = NULL
			prob = decision = NULL
			time.predict = as.numeric(NA)
			
			# was there an error in building the model? --> return NAs
			if(is(model["learner.model"], "learner.failure")) {
				if (is(model, "wrapped.model.classif")) {
					response = factor(rep(NA, nrow(newdata)), levels=levs)
					mm = matrix(NA, nrow=nrow(newdata), ncol=length(levs))
					colnames(mm) = levs
					prob = decision = mm; 
				} else {
					response = as.numeric(rep(NA, nrow(newdata)))
				}
			} else {
				pars <- list(
						.wrapped.learner = wl,
						.wrapped.model = model, 
						.newdata=newdata
				)
				pars <- c(pars, wl@predict.fct.pars) 
				
				if(!is.null(.mlr.local$debug.seed)) {
					set.seed(.mlr.local$debug.seed)
					warning("DEBUG SEED USED! REALLY SURE YOU WANT THIS?")
				}
				st = system.time({
					for (tt in type) {
					
						if (is(model, "wrapped.model.classif"))
							pars$.type = tt
						p <- do.call(predict.learner, pars)
						
						if (is(model, "wrapped.model.classif")) {
							if (tt == "response") {
								# the levels of the predicted classes might not be complete....
								# be sure to add the levels at the end, otherwise data gets changed!!!
								if (!is.factor(p))
									stop("predict.learner for ", class(wl), " has returned a class ", class(p), " instead of a factor!")
								levels(p) <- union(levels(p), levs)
							} else if (tt %in% c("prob")) {
								if (!is.matrix(p))
									stop("predict.learner for ", class(wl), " has returned a class ", class(p), " instead of a matrix!")
								if (any(sort(colnames(p)) != sort(levs)))
									stop("predict.learner for ", class(wl), " has returned not the class levels as column names:", colnames(p))
								if (dd["class.nr"] == 2)
									p = p[,td["positive"]]
							} else if (tt %in% c("decision")) {
								if (!is.matrix(p))
									stop("predict.learner for ", class(wl), " has returned a class ", class(p), " instead of a matrix!")
							} else {
								stop(paste("Unknown type", tt, "in predict!"))
							}	
						} else if (is(model, "wrapped.model.regr")) {
							if (class(p) != "numeric")
								stop("predict.learner for ", class(wl), " has returned a class ", class(p), " instead of a numeric!")
						}
						logger.debug("prediction:")
						logger.debug(p)
						if (tt == "response") 
							response = p
						if (tt == "prob") 
							prob = p
						if (tt == "decision") 
							decision = p
					}
				})
				time.predict = st[3]
			}
			if (missing(task))
				ids = NULL
			else
				ids = subset
			
			make.prediction(data.desc=dd, task.desc=td, id=ids, response=response, prob=prob, decision=decision, truth=truth,  
					time.train=model["time"], time.predict=time.predict)
		}
)



