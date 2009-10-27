#' @include task.learn.r
#' @include wrapped.model.r
roxygen()

#' Predicts the target values of a new data set based on 
#' an already fitted wrapped.model of a regression task.   
#' 
#' See documentation super method. 
#' 
#' @param object [\code{\linkS4class{learn.task}}] \cr 
#'  Learning task
#' @param model [\code{\linkS4class{wrapped.model}}] \cr 
#'   Wrapped model, trained from learn task  
#' @param newdata [\code{\link{data.frame}}] \cr 
#'   Contains new observations which should be predicted (by default the train data of the wrapped model).
#' 
#' @return A vector of numeric values.   
#'
#' @export
#' 
#' @usage \S4method{predict}{regr.task}(object, model, newdata)
#'
#' @seealso \code{\link{predict}}
#'
#' @examples
#' 
#' data(BostonHousing)
#' inds <- seq(1, nrow(BostonHousing), 2)
#' test <- BostonHousing[-inds,]
#' 
#' rt <- make.regr.task("stats.lm", data=BostonHousing, target="medv")
#' model <- train(rt, subset=inds)
#' predict(rt, model, newdata = test)
#' 
#' @title predict

setMethod(
		f = "predict",
		signature = signature(object="regr.task"),
		def = function(object, model, newdata) {
			
			lt <- object
			wl <- lt@wrapped.learner

			if (missing(newdata)) {
				newdata <- lt@data[model@subset,]
			}
			
			g <- wl@predict.fct
			#	cn <- lt["class.name"]
			
			# was there an error in building the model? --> return NAs
#			if(class(model@learner.model)[1] == "learner.failure") {
#				p <- factor(rep(NA, nrow(newdata)), levels=lt["class.levels"])
#				return(p)
#			}
			
			g.pars <- list(model@learner.model)
			g.pars[[wl@predict.newdata.arg]] <- newdata
			g.pars <- c(g.pars, wl@predict.fct.pars)
			
			if(!is.null(.mlr.local$debug.seed)) {
				set.seed(.mlr.local$debug.seed)
				warning("DEBUG SEED USED! REALLY SURE YOU WANT THIS?")
			}
			
			logger.debug("Regr. predict:", wl@learner.name, "with pars:")
			logger.debug(wl@predict.fct.pars)
			logger.debug("on", nrow(newdata), "examples:")
			logger.debug(rownames(newdata))
			
			p <- do.call(g, g.pars)
			
			logger.debug("Regr. prediction:")
			logger.debug(p)
			return(p)
		}
)
