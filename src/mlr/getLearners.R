#' @include LearnTask.R
roxygen()

#' Returns the names of learning algorithms which have specific characteristics, e.g.
#' whether it supports missing values, misclassification costs, case weights,...
#' or which are are able to solve a given \code{\linkS4class{LearnTask}}. 
#' 
#' The default of all boolean parameters is NA, meaning: property is not required, don't care.
#' 
#' @param x [string | \code{\linkS4class{LearnTask}}] \cr
#' 			Type of the learning algorithm, either "classif" or "regr" or task to solve
#' @param doubles [boolean] \cr
#' 			Supports real-valued inputs? Pass only when x is a string.
#' @param factors [boolean] \cr
#' 			Supports factor inputs? Pass only when x is a string.
#' @param characters [boolean] \cr
#' 			Supports character inputs? Pass only when x is a string.
#' @param missings [boolean] \cr
#' 			Supports missing values? Pass only when x is a string.
#' @param multiclass [boolean] \cr
#' 			Supports multiclass problems? Pass only when x is a string.
#' @param weights [boolean] \cr
#' 			Supports case weights? Pass only when x is a string.
#' @param probs [boolean] \cr
#' 			Can predict probabilities?
#' @param decision [boolean] \cr
#' 			Supports decision values?
#' @param costs [boolean] \cr
#' 			Supports non-standard misclassification costs?
#' 
#' @rdname getLearners
#' @export 
#' 
#' @title Find matching learning algorithms.

setGeneric(
		name = "getLearners",
		def = function(x, ...) {
			standardGeneric("getLearners")
		}
)


#' @export
#' @rdname getLearners 
setMethod(
		f = "getLearners",
		
		signature = signature(
				x = "character" 
		),
		
		def = function(
				x = NA, 
				doubles = NA, 
				factors = NA,
				characters = NA,
				missings = NA,
				weights = NA,
        multiclass = NA,
        probs = NA,
				decision = NA,
				costs = NA){
      type = x
			mlr.classes <- getClasses(where = getNamespace("mlr"))
			if(is.na(type)) 
				type = "na"
			top.cl = switch(type, classif="rlearner.classif", regr="rlearner.regr", na="rlearner")
			ls <- Filter(function(x) extends(x, top.cl) && x != top.cl , mlr.classes)
			
			f <- function(x) {
				wl <- try(makeLearner(x))
				if(is (wl, "try-error")) 
					return(NULL)
				else
					return(wl)
			}
			
			ls <- lapply(ls, f)
			ls <- Filter(function(x) !is.null(x), ls)
			
			
			f <- function(x) {
				( is.na(doubles) || doubles == x["doubles"] ) &&
						( is.na(factors) || factors == x["factors"] ) &&
						( is.na(characters) || characters == x["characters"] ) &&
						( is.na(missings) || missings == x["missings"] ) &&
						( is.na(multiclass) || multiclass == x["multiclass"] ) &&
						( is.na(weights) || weights == x["weights"]  ) &&
						( is.na(probs) || probs == x["prob"] ) &&
						( is.na(decision) || decision == x["decision"]  ) &&
						( is.na(costs) || costs == x["costs"]  )
			}
			
			ls <- Filter(f, ls)
			ls <- sapply(ls, function(x) as.character(class(x)))
			
			return(ls)
		}
)			

#' @export
#' @rdname getLearners 
setMethod(
		f = "getLearners",
		
		signature = signature(x = "LearnTask"),
		
		def = function(x, probs=NA, decision=NA, costs=NA) {
			type = ifelse(x@desc@type, "classif", "regr")

      doubles = ifelse(x["n.feat"]["double"]>0, TRUE, NA)
      factors = ifelse(x["n.feat"]["fact"]>0, TRUE, NA)
      missings = ifelse(x["has.missing"], TRUE, NA)
      weights = ifelse(x["has.weights"], TRUE, NA)
      
      if (type == "classif") {
        multiclass = ifelse(length(getClassLevels(x)) == 2, NA, TRUE)
        costs = ifelse(x["has.costs"], TRUE, costs)
        wls = getLearners(type, doubles, factors, characters, missings, weights, 
            multiclass, probs, decision, costs)
      } else {
        wls = getLearners(type, doubles, factors, characters, missings, weights) 
      }	 
			return(wls)
		}
)			

