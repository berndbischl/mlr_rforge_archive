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
#' 		  Specifies the type of predictions for classification - either probability ("prob") or class ("class"). 
#'        If not given, the type specified in the classification task is used (which is per default "class").
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
		def = function(object, newdata, type) {
			
			model <- object
			wl <- model@wrapped.learner
			
			# drop target col
			newdata <- newdata[, -which(colnames(newdata) == model["target"])]					
			if (is(model, "wrapped.model.classif")) {
				if (missing(type))
					type = model["type"]
				if (type == "prob" && !wl@learner.props@supports.probs) {
					stop("Trying to predict probs, but ", wl@learner.name, " does not support that!")
				}
			}

			logger.debug("mlr predict:", wl@learner.name, "with pars:")
			logger.debug(wl@predict.fct.pars)
			logger.debug("on", nrow(newdata), "examples:")
			logger.debug(rownames(newdata))
			
			# was there an error in building the model? --> return NAs
			if(is(model["learner.model"], "learner.failure")) {
				if (is(model, "wrapped.model.classif")) {
					if (type=="class") {
						p <- factor(rep(NA, nrow(newdata)), levels=model["class.levels"])
					} else if (type=="prob") {
						p = matrix(NA, nrow=nrow(newdata), colnames=model["class.levels"])
					}
				} else {
					p = as.numeric(rep(NA, nrow(newdata)))
				}
			} else {
				pars <- list(
						.wrapped.learner = wl,
						.wrapped.model = model, 
						.newdata=newdata
				)
				if (is(model, "wrapped.model.classif"))
					pars$.type = type
				pars <- c(pars, wl@predict.fct.pars) 
				
				if(!is.null(.mlr.local$debug.seed)) {
					set.seed(.mlr.local$debug.seed)
					warning("DEBUG SEED USED! REALLY SURE YOU WANT THIS?")
				}
				p <- do.call(predict.learner, pars)
				
				if (is(model, "wrapped.model.classif")) {
					if (type=="class") {
						# the levels of the predicted classes might not be complete....
						# be sure to add the levels at the end, otherwise data gets changed!!!
						if (!is.factor(p))
							stop("predict.learner for ", class(wl), " has returned a class ", class(p), " instead of a factor!")
						levels(p) <- union(levels(p), model["class.levels"])
					} else if (type=="prob") {
						if (!is.matrix(p))
							stop("predict.learner for ", class(wl), " has returned a class ", class(p), " instead of a matrix!")
						if (any(sort(colnames(p)) != sort(model["class.levels"])))
							stop("predict.learner for ", class(wl), " has returned not the class levels as column names:", colnames(p))
					} else {
						stop(paste("Unknown type", type, "in predict!"))
					}	
				} else if (is(model, "wrapped.model.regr")) {
					if (class(p) != "numeric")
						stop("predict.learner for ", class(wl), " has returned a class ", class(p), " instead of a numeric!")
				}
			}			
			logger.debug("prediction:")
			logger.debug(p)
			return(p)
			
			
 
		}
)



