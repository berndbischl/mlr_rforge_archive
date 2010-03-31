#' include wrapped.model.r
roxygen()

#' Predicts the target classes of a new data set based on 
#' an already fitted wrapped.model of a classifcation task.   
#' 
#' See documentation super method. 
#' 
#' @param object [\code{\linkS4class{wrapped.model}}] \cr 
#'        Wrapped model, trained from learn task  
#' @param newdata [\code{\link{data.frame}}] \cr 
#'        Contains new observations which should be predicted (by default the train data of the wrapped model).
#' @param type [\code{\link{character}}] \cr 
#' 		  Specifies the type of predictions for classification - either probability ("prob") or class ("response"). 
#'        If not given, the type specified in the classification task is used (which is per default "response").
#' 		  Ignored in case of regression.
#' 
#'
#' @return For classification: Either a factor of predicted classes or a matrix of probabilities. The rows correspond to the 
#'      predicted observations and the columns to the classes. Each column has the name of its respective class. 
#'      For regression: A numeric vector of predictions.    
#'
#' @export
#' @importFrom stats predict
#' 
#' @usage \S4method{predict}{classif.task}(object, model, newdata, type="default")
#' @seealso \code{\link{predict,learn.task-method}}
#'
#' @examples
#' 
#' inds <- 2*(1:75)
#' test <- iris[-inds,]
#' 
#' ct <- make.classif.task(data=iris, target="Species")
#' model <- train("lda", ct, subset=inds)
#' predict(model, newdata = test)
#' 
#' data(BostonHousing)
#' inds <- seq(1, nrow(BostonHousing), 2)
#' test <- BostonHousing[-inds,]
#' 
#' rt <- make.regr.task(data=BostonHousing, target="medv")
#' model <- train(("stats.lm", rt, subset=inds)
#' predict(model, newdata = test)
#' 
#' @title predict

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
				trues = newdata[, tn]
			else
				trues = NULL
			
			# drop target col
			newdata <- newdata[, -which(cns == tn)]					
			if (is(model, "wrapped.model.classif")) {
				if ("prob" %in% type && !wl@learner.props@supports.probs) {
					stop("Trying to predict probs, but ", wl@learner.name, " does not support that!")
				}
				if ("decision" %in% type && !wl@learner.props@supports.decision) {
					stop("Trying to predict decision values, but ", wl@learner.name, " does not support that!")
				}
			}

			logger.debug("mlr predict:", wl@learner.name, "with pars:")
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
			weights = NULL
			if (!missing(task))
				weights = task["weights"][ids]
			make.prediction(data.desc=dd, task.desc=td, id=ids, response=response, prob=prob, decision=decision, target=trues, weights=weights, 
					time.train=model["time"], time.predict=time.predict)
		}
)



