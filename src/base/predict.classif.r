#' Predicts the class memberships for the observations of a new dataset based on 
#' the knowledge of an existing classification model.   
#'
#' @param model [\code{\linkS4class{model}}] \cr 
#'   Specifies classification task  
#' @param newdata [data.frame] \cr 
#'   Contains new observations which should be classified(by default the train data).
#'
#' @return predict returns a prediction object containing a factor vector of 
#' predicted classes.
#'
#' @export
#' 
#' @usage predict(model, newdata)
#'
#' @seealso \code{\link{train}}
#'
#' @examples
#' library(MASS)
#'
#' inds <- 2*(1:75)
#' test <- iris[-inds,]
#'
#' lda.learn.task <- new("t.lda", data=iris, formula=Species~.)
#' lda.model <- train(lda.learn.task, subset=inds)
#' lda.prediction <- predict(lda.model, newdata = test)
#' 
#'  @title predict

setMethod(
		f = "predict",
		signature = c(object="wrapped.classif.model"),
		def = function(object, newdata, type="default") {
			
			model <- object	
			lt <- model@learn.task
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
			
			if(exists("debug.seed") && !is.null(debug.seed)) {
				set.seed(debug.seed)
				logger.warn("DEBUG SEED USED!!!!!!!!!!!!!!! REALLY SURE????")
			}
			
			logger.debug("Clr predict:", wl@learner.name, "with pars:")
			logger.debug(wl@predict.fct.pars)
			logger.debug("on", nrow(newdata), "examples:")
			logger.debug(rownames(newdata))
			
			p <- do.call(g, g.pars)
			logger.debug("raw prediction:")
			logger.debug(p)
			
			if (type == "class") {
				p <- wl@trafo.for.classes(p)
				# the levels of the predicted classes might not be complete....
				# be sure to add the levels at the end, otherwise data gets changed!!!
				levels(p) <- union(levels(p), lt["class.levels"])
			} else if (type == "prob") {
				p <- wl@trafo.for.probs(p)
			} else {
				logger.error(paste("Unknown type", type, "in predict!"))
			}	
			
			logger.debug("tranformed prediction:")
			logger.debug(p)
			return(p)
		}
)
