#' @include wrapped.learner.classif.r
myrox()

#' Wrapped learner for Logistic Regression from package \code{stats} for classification problems.
#' 
#' \emph{Common hyperparameters:}
#' \describe{
#' 		\item{\code{start}}{Starting values for the parameters in the linear predictor.}	
#' 		\item{\code{etastart}}{Starting values for the linear predictor.}
#' 		\item{\code{mustart}}{Starting values for the vector of means.}
#' }
#' @title logreg
#' @seealso \code{\link[stats]{glm}}
#' @export
setClass(
		"logreg", 
		contains = c("wrapped.learner.classif")
)


#----------------- constructor ---------------------------------------------------------
#' Constructor.
#' @title Logistic Regression Constructor
setMethod(
		f = "initialize",
		signature = signature("logreg"),
		def = function(.Object) {
			
			desc = new("classif.props",
					supports.multiclass = FALSE,
					supports.missing = TRUE,
					supports.numerics = TRUE,
					supports.factors = TRUE,
					supports.characters = FALSE,
					supports.probs = TRUE,
					supports.weights = TRUE,
					supports.costs = FALSE
			)
			
			callNextMethod(.Object, learner.name="logreg", learner.pack="stats", learner.props=desc)
		}
)

setMethod(
		f = "train.learner",
		signature = signature(
				.wrapped.learner="logreg", 
				.targetvar="character", 
				.data="data.frame", 
				.weights="numeric", 
				.costs="matrix", 
				.type = "character" 
		),
		
		def = function(.wrapped.learner, .targetvar, .data, .weights, .costs, .type,  ...) {
			f = as.formula(paste(.targetvar, "~."))
			glm(f, family="binomial", data=.data, ...)
		}
)

setMethod(
		f = "predict.learner",
		signature = signature(
				.wrapped.learner = "logreg", 
				.wrapped.model = "wrapped.model", 
				.newdata = "data.frame", 
				.type = "character" 
		),
		
		def = function(.wrapped.learner, .wrapped.model, .newdata, .type, ...) {
			
			x <- predict(.wrapped.model["learner.model"], newdata=.newdata, type="response", ...)
			
			if (.type == "prob") {
				y <- matrix(0, ncol=2, nrow=length(.newdata))
				colnames(y) <- .wrapped.model["class.levels"]
				y[,1] <- x
				y[,2] <- 1-x
				return(y)
			} else {
				levs <- .wrapped.model["class.levels"]
				p <- as.factor(ifelse(x > 0.5, levs[2], levs[1]))
				names(p) <- NULL
				return(p)
			}
		}
)	


