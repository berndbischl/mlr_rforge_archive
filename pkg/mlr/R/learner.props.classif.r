#' @include learner.props.r
roxygen()

#'  \describe{	
#' Since not all classifiers can deal with all kind of data properties of the data are compared 
#' with the possibilities of the classifier when a \code{\linkS4class{learn.task}} is generated. 
#' A classif.props object describes such classifier possibilities by logical values.}
#' 
#' \cr\cr\bold{Slots:}
#'  \describe{	
#'   \item{\code{supports.multiclass [logical]}}{Can the classifier deal with multiclass problems?}
#'   \item{\code{supports.missing [logical]}}{Can the classifier deal with missing values?}
#'   \item{\code{supports.numerics [logical]}}{Can the classifier deal with numeric variables?}
#'   \item{\code{supports.factors [logical]}}{Can the classifier deal with factor variables?}
#'   \item{\code{supports.characters [logical]}}{Can the classifier deal with character variables?}
#'   \item{\code{supports.probs [logical]}}{Can the classifier return class probabilities when predicting?}
#'  }
#' 
#'  @note  
#'  When a \code{\linkS4class{learn.task}} gets printed its classifier description is shown on the console.
#' 
#'  @title classif.props
#'  @export

setClass(
		"classif.props",
		contains = "learner.props",
		representation(
				supports.multiclass = "logical",
				supports.probs = "logical"
		)
)



setMethod(
		f = "as.character",
		signature = "classif.props",
		def = function(x) {
			return(
					paste(
							"Learner: \n",  
							"Supports multiclass: ", x@supports.multiclass, "\n",
							"Supported features Nums:", x@supports.numerics, " Factors:", x@supports.factors, " Chars:", x@supports.characters, "\n",
							"Supports missings: ", x@supports.missing, "\n", 
							"Supports probabilities: ", x@supports.probs, "\n", 
							"Supports weights: ", x@supports.weights, "\n", 
							sep=""
					)
			)
		}
)


