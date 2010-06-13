#' include wrapped.model.r
roxygen()

#' Predict the target variable of new data using a fitted model. If the type is set to "prob" or "decision"
#' probabilities or decision values will be stored in the resulting object. The resulting class labels are 
#' the classes with the maximum values or thresholding can also be used.
#' 
#' @param object [\code{\linkS4class{wrapped.model}}] \cr 
#'        Wrapped model, trained from a learn task.  
#' @param task [\code{\linkS4class{learn.task}}]\cr 
#'        Specifies learning task. If this is passed, data from this task is predicted.   
#' @param subset [integer] \cr 
#'        Index vector to subset the data in the task to use for prediction. 
#' @param newdata [\code{\link{data.frame}}] \cr 
#'        New observations which should be predicted. Alternatively pass this instead of task. 
#' @param type [string] \cr
#'        Classification: "response" | "prob" | "decision", specifying the type to predict.
#'        Default is "response". "decision" is experimental.
#' 		  Ignored for regression.	 
#' @param threshold [numeric] \cr
#'        Threshold to produce class labels if type is not "response". 
#' 	      Currently only supported for binary classification and type="prob", where it represents the required predicted probability
#'        for the positive class, so that a positive class is predicted as "response".
#'        Default is 0.5 for type="prob".
#' 		  Ignored for regression.	 
#' @return \code{\linkS4class{prediction}}.
#'
#' @export
#' @rdname predict
#' @importFrom stats predict
#' @seealso \code{\link{train}}
#' @title Predict new data.


#todo decision
setMethod(
		f = "predict",
		signature = signature(object="wrapped.model"),
		def = function(object, task, newdata, subset, type, threshold) {
			if (!missing(task) && !missing(newdata)) 
				stop("Pass either a task object or a newdata data.frame to predict, but not both!")
			
			if (missing(newdata)) {
				if (missing(subset))
					subset = 1:task["size"]
				newdata = task["data", row=subset]
			}

			model = object
			wl = model["learner"]
			td = model@task.desc
			dd = model@data.desc
			
			if (missing(type))
				type = wl["predict.type"]
			if (missing(threshold))
				threshold = wl["predict.threshold"]
			if (is.null(threshold))
				threshold = switch(type, response=numeric(0), prob=0.5, decision=0)
			
			
			cns = colnames(newdata)
			tn = td["target"]
			t.col = which(cns == tn)
			# get truth and drop target col, if target in newdata
			if (length(t.col) == 1) {
				truth = newdata[, t.col]
				newdata = newdata[, -t.col]					
				
			} else {
				truth = NULL
			}
			
			if (wl["is.classif"]) {
				if ("prob" == type && !wl["supports.probs"]) {
					stop("Trying to predict probs, but ", wl["id"], " does not support that!")
				}
				if ("decision" == type && !wl["supports.decision"]) {
					stop("Trying to predict decision values, but ", wl["id"], " does not support that!")
				}
			}

			logger.debug("mlr predict:", wl["id"], "with pars:")
			logger.debug(wl["hyper.pars"])
			logger.debug("on", nrow(newdata), "examples:")
			logger.debug(rownames(newdata))
			
			if (wl["is.classif"]) {
				levs = dd["class.levels"]
			}
			
			response = NULL
			prob = decision = NULL
			time.predict = as.numeric(NA)
			
			# was there an error in building the model? --> return NAs
			if(is(model["learner.model"], "learner.failure")) {
				if (wl["is.classif"]) {
					p = switch(type, 
							response = factor(rep(NA, nrow(newdata)), levels=levs),
							matrix(NA, nrow=nrow(newdata), ncol=length(levs), dimnames=list(NULL, levs))
					)
				} else {
					p = as.numeric(rep(NA, nrow(newdata)))
				}
			} else {
				pars <- list(
						.learner = wl,
						.model = model, 
						.newdata=newdata
				)
				pars = c(pars, wl["hyper.pars", type="predict"]) 
				if (wl["is.classif"]) {
					pars$.type = type
				}
				#pars = insert.matching(pars, list()) 
				
				if(!is.null(.mlr.local$debug.seed)) {
					set.seed(.mlr.local$debug.seed)
					warning("DEBUG SEED USED! REALLY SURE YOU WANT THIS?")
				}
				
				if(is(model["learner.model"], "novars")) {
					p = predict(model["learner.model"], newdata, type)
					time.predict = 0
				} else {
					st = system.time(p <- do.call(pred.learner, pars), gcFirst=FALSE)
					time.predict = st[3]
				}
				if (wl["is.classif"]) {
					if (type == "response") {
						# the levels of the predicted classes might not be complete....
						# be sure to add the levels at the end, otherwise data gets changed!!!
						if (!is.factor(p))
							stop("pred.learner for ", class(wl), " has returned a class ", class(p), " instead of a factor!")
						levels(p) <- union(levels(p), levs)
					} else if (type == "prob") {
						if (!is.matrix(p))
							stop("pred.learner for ", class(wl), " has returned a class ", class(p), " instead of a matrix!")
						cns = colnames(p)
						if (is.null(cns) || length(cns) == 0)
							stop("pred.learner for ", class(wl), " has returned not the class levels as column names, but no column names at all!")
						if (!setequal(cns, levs))
							stop("pred.learner for ", class(wl), " has returned not the class levels as column names:", colnames(p))
					} else if (type == "decision") {
						if (!is.matrix(p))
							stop("pred.learner for ", class(wl), " has returned a class ", class(p), " instead of a matrix!")
					} else {
						stop(paste("Unknown type", type, "in predict!"))
					}	
				} else if (is(model, "wrapped.model.regr")) {
					if (class(p) != "numeric")
						stop("pred.learner for ", class(wl), " has returned a class ", class(p), " instead of a numeric!")
				}
				logger.debug("prediction:")
				logger.debug(p)
			}
			if (missing(task))
				ids = NULL			else
				ids = subset
			make.prediction(data.desc=dd, task.desc=td, id=ids, truth=truth, 
					type=type, y=p, threshold=threshold,  
					time.train=model["time"], time.predict=time.predict)
		}
)



