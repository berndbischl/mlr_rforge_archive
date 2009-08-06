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



#' Given a \code{\linkS4class{learn.task}} \code{train} creates a model for the learning machine 
#' which can be used for predictions on new data. 
#'
#' @param learn.task [\code{\linkS4class{learn.task}}]\cr 
#'        Specifies learning task.   
#' @param subset [\code{\link{integer}}] \cr 
#'        An index vector specifying the training cases from the data contained in the learning task. By default the complete dataset is used. 
#' @param parset [\code{\link{list}}] \cr
#'       Named list which contains the hyperparameters of the learner. Default is an empty list, which means no hyperparameters are specifically set and defaults of the underlying learner are used.
#'
#' @return An object of class "model" containing the learn.task and the generated external classification model . 
#'
#' @export 
#'
#' @usage train(learn.task, subset, parset)  
#'
#' @examples 
#' library(MASS)
#' train.inds <- seq(1,150,2)
#' test.inds <- seq(2,150,2)
#'
#' ct <- make.classif.task("lda", data=iris, formula=Species~.)
#' cm <- train(ct, subset=train.inds)
#' ps <- predict(cm, subset=test.inds)
#' performance()
#' 
#' @seealso \code{\link{predict}}, \code{\link{make.classif.task.task}}, \code{\link{make.regr.task}} 
#' 
#' @title train

setMethod(
		f = "train",
		signature = signature(learn.task="learn.task", subset="numeric", parset="list"),
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
