#' @include wrapped.learner.classif.r
roxygen()

#' Wrapped learner for k-Nearest Neighbor from package \code{kknn} for classification problems.
#' 
#' \emph{Common hyperparameters:}
#' \describe{
#' 		\item{\code{k}}{Number of neighbors considered.} 	
#' 		\item{\code{distance}}{Parameter of Minkowski distance.}
#' }
#' @title kknn.classif
#' @seealso \code{\link[kknn]{kknn}}
#' @export
setClass(
		"kknn.classif", 
		contains = c("wrapped.learner.classif")
)


#----------------- train.kknn.model ---------------------------------------------------------

train.kknn.model <- function(formula, data, ...) {
  model <- list(formula=formula, data=data, parset=list(...))
  class(model) <- "kknn"
  return(model)
}

predict.kknn.model <- function(model, newdata, type="class", ...) {
	# this is stupid but kknn forces it....
	cl <- as.character(model$formula)[2]
	newdata[,cl] <- 0
	
	pars <- list(formula=model$formula, train=model$data, test=newdata)  
	pars <- c(pars, model$parset)
	m = do.call(kknn, pars)
	if (type=="class")
		return(m$fitted.values)
	else 
		return(m$prob)
}


#----------------- constructor ---------------------------------------------------------
#' Constructor.
#' @title kNN (classification) Constructor

setMethod(
  f = "initialize",
  signature = signature("kknn.classif"),
  def = function(.Object, train.fct.pars=list(), predict.fct.pars=list()) {
    train.fct <- train.kknn.model 
    predict.fct <- predict.kknn.model
     
    desc <- new("classif.props",
      supports.multiclass = TRUE,
      supports.missing = TRUE,
      supports.numerics = TRUE,
      supports.factors = TRUE,
      supports.characters = TRUE,
      supports.probs = TRUE,
	  supports.weights = FALSE
	)
      
    .Object <- callNextMethod(.Object, learner.name="knn", learner.pack="kknn", 
      learner.model.class="kknn", learner.model.S4 = FALSE,
      train.fct=train.fct, train.fct.pars=train.fct.pars, 
      predict.fct=predict.fct, predict.fct.pars=predict.fct.pars, 
	  learner.props=desc)
    return(.Object)
  }
)



