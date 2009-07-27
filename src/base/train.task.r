#' @include task.learn.r
roxygen()


setGeneric(
		name = "train",
		def = function(learn.task, subset, parset) {
			if (missing(subset))
				subset <- 1:nrow(learn.task@data)
			if (missing(parset))
				parset <- list()
			standardGeneric("train")
		}
)



#' Given a \code{\linkS4class{learn.task}} train creates a classification model 
#' which can be used for predictions by predict. 
#'
#' @param ct [\code{\linkS4class{learn.task}}]\cr 
#'   Specifies classifier and classification task   
#' @param subset [integer] \cr An index vector specifying the 
#' cases to for the training sample.(by default the complete dataset is used) 
#' @param parset [list] \cr Contains the hyperparameters of the train function. 
#' 	 (default is an empty list. In this case no parameters are used)
#'
#' @return train returns an object of class "model" containing the 
#' learn.task and the generated external classification model . 
#'
#' @export 
#'
#' @usage train(learn.task, subset, parset)  
#'
#' @examples 
#' library(MASS)
#' inds <- 2*(1:75)
#'
#' lda.learn.task <- new("t.lda", data=iris, formula=Species~.)
#' lda.model <- train(lda.learn.task, subset=inds)
#' 
#' @title train

setMethod(
		f = "train",
		signature = c(learn.task="learn.task", subset="integer", parset="list"),
		def = function(learn.task, subset, parset) {
			
					
			wl <- learn.task@wrapped.learner
			data.subset <- learn.task@data[subset,]
			ws <- learn.task@weights[subset]
			if(exists("debug.seed") && !is.null(debug.seed)) {
				set.seed(debug.seed)
				logger.warn("DEBUG SEED USED!!!REALLY SURE?")
			}
			logger.debug("mlr train:", wl@learner.name, "with pars:")
			logger.debug(parset)
			logger.debug("on", length(subset), "examples:")
			logger.debug(subset)
			learner.model <- train.learner(wrapped.learner=wl, formula=learn.task@formula, data=data.subset, weights=ws, parset=parset)
			if(class(learner.model)[1]=="try-error") {
				msg <- as.character(learner.model)
				logger.warn("Could not train the method: ", msg)	
				learner.model <- new("learner.failure", msg=msg)
			} 
			
			return(new("wrapped.model", learn.task=learn.task, learner.model = learner.model, subset=subset, parset=parset))
		}
)
