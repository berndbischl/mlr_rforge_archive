#' @include wrapped.learner.classif.r 
myrox()

#' Wrapped learner for Regularized Discriminant Analysis from package \code{klaR} for classification problems.
#' 
#' \emph{Common hyperparameters:}
#' \describe{
#' 		\item{\code{gamma}}{See details in \code{klaR}.}		
#' 		\item{\code{lambda}}{See details in \code{klaR}.}	
#' }
#' @title rda
#' @seealso \code{\link[klaR]{rda}}
#' @export
setClass(
		"rda", 
		contains = c("wrapped.learner.classif")
)


#----------------- constructor ---------------------------------------------------------
#' Constructor.
#' @title RDA Constructor
setMethod(
		f = "initialize",
		signature = signature("rda"),
		def = function(.Object) {
			
			desc = new("classif.props",
					supports.multiclass = TRUE,
					supports.missing = FALSE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = FALSE,
					supports.probs = TRUE,
					supports.weights = FALSE,			
					supports.costs = FALSE
			)
			
			callNextMethod(.Object, learner.name="rda", learner.pack="klaR", learner.props=desc)
		}
)

setMethod(
		f = "train.learner",
		signature = signature(
				.wrapped.learner="rda", 
				.targetvar="character", 
				.data="data.frame", 
				.weights="numeric", 
				.costs="matrix", 
				.type="character" 
		),
		
		def = function(.wrapped.learner, .targetvar, .data, .weights, .costs, .type, ...) {
			f = as.formula(paste(.targetvar, "~."))
			rda(f, data=.data, ...)
		}
)

setMethod(
		f = "predict.learner",
		signature = signature(
				.wrapped.learner = "rda", 
				.wrapped.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.wrapped.learner, .wrapped.model, .newdata, .type, ...) {
			p <- predict(.wrapped.model["learner.model"], newdata=.newdata, ...)
			if (.type=="class")
				return(p$class)
			else
				return(p$posterior)
			
		}
)	





