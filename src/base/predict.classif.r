#' Predicts the target classes of a new data set based on 
#' an already fitted wrapped.model of a classifcation task.   
#' 
#' See documentation super method. 
#' 
#' @param object [\code{\linkS4class{learn.task}}] \cr 
#'        Learning task
#' @param model [\code{\linkS4class{wrapped.model}}] \cr 
#'        Wrapped model, trained from learn task  
#' @param newdata [\code{\link{data.frame}}] \cr 
#'        Contains new observations which should be predicted (by default the train data of the wrapped model).
#' @param type [\code{\link{character}}] \cr 
#' 		  Specifies the type of predictions -	either probability ("prob") or class ("class"). 
#'        Default is "default", which uses the type specified in the classification task.
#'
#' @return Either a factor of predicted classes or a matrix of probabilities. The rows correspond to the 
#'      predicted observations and the columns to the classes. Each column has the name of its respective class.   
#'
#' @export
#' 
#' @usage \S4method{predict}{classif.task}(object, model, newdata, type="default")
#' @seealso \code{\link{predict,learn.task-method}}
#'
#' @examples
#' 
#' inds <- 2*(1:75)
#' test <- iris[-inds,]
#' 
#' ct <- make.classif.task("lda", data=iris, target="Species")
#' model <- train(ct, subset=inds)
#' predict(ct, model, newdata = test)
#' 
#' @title predict
setMethod(
		f = "predict",
		signature = signature(object="classif.task"),
		def = function(object, model, newdata, type="default") {
			
			lt <- object	
			wl <- lt@wrapped.learner
			
			if (missing(newdata)) {
				newdata <- lt@data[model@subset,]
			}
					
			g <- wl@predict.fct
			cn <- lt["target.name"]
			
			# was there an error in building the model? --> return NAs
			if(class(model@learner.model)[1] == "learner.failure") {
				p <- factor(rep(NA, nrow(newdata)), levels=lt["class.levels"])
				return(p)
			}
			
			
			if (!wl@dummy.classes) {
				# standard: remove class col if it exists
				# some methods like naive bayes don't like the class column to be in the dataframe
				newdata[, cn] <- NULL
			}
			else {
				# if there is no class col in new data, generate dummy one
				# (stupid, but kknn and adaboost.M1 seem to force it....)
				if (!(cn %in% colnames(newdata))) {
					newdata[, cn] <- 0
				}
			}
			
			g.pars <- c(list(model@learner.model, newdata=newdata), wl@predict.fct.pars)
			if (type=="default")
				type = lt@type
			if (type == "class")
				g.pars <- c(g.pars, wl@predict.par.for.classes)		
			else if (type == "prob")
				g.pars <- c(g.pars, wl@predict.par.for.probs)		
			else {
				logger.error(paste("Unknown type", type, "in predict!"))
			}	
			
			if(!is.null(.mlr.local$debug.seed)) {
				set.seed(.mlr.local$debug.seed)
				warning("DEBUG SEED USED! REALLY SURE YOU WANT THIS?")
			}
		
			# if there are no vars in the model, directly predict with our dummy model
			if (is(model@learner.model, "novars.model.classif")) {
				p <- predict(model@learner.model, newdata, type)
				logger.debug("prediction with no vars model:")
				logger.debug(p)
			} else {
				logger.debug("mlr predict:", wl@learner.name, "with pars:")
				logger.debug(wl@predict.fct.pars)
				logger.debug("on", nrow(newdata), "examples:")
				logger.debug(rownames(newdata))
				p <- do.call(g, g.pars)
				logger.debug("raw prediction:")
				logger.debug(p)
				if (type == "class") {
					p <- wl@trafo.for.classes(p, object, model)
				} else if (type == "prob") {
					p <- wl@trafo.for.probs(p, object, model)
				} else {
					logger.error(paste("Unknown type", type, "in predict!"))
				}
				logger.debug("tranformed prediction:")
				logger.debug(p)
			} 
			
			# the levels of the predicted classes might not be complete....
			# be sure to add the levels at the end, otherwise data gets changed!!!
			if (type == "class") {
				levels(p) <- union(levels(p), lt["class.levels"])
			}				
			return(p)
		}
)
