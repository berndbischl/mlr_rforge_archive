#' @include LearnTask.R
roxygen()

#' Returns the names of learning algorithms which have specific characteristics, e.g.
#' whether it supports missing values, case weights,...
#' or which are are able to solve a given \code{\linkS4class{LearnTask}}. 
#' 
#' The default of all boolean parameters is NA, meaning: property is not required, don't care.
#' 
#' @param x [string | \code{\linkS4class{LearnTask}}] \cr
#' 			Type of the learning algorithm, either "classif" or "regr" or task to solve
#' @param doubles [\code{logical(1)}] \cr
#' 			Supports real-valued inputs? Pass only when x is a string.
#' @param factors [\code{logical(1)}] \cr
#' 			Supports factor inputs? Pass only when x is a string.
#' @param characters [\code{logical(1)}] \cr
#' 			Supports character inputs? Pass only when x is a string.
#' @param missings [\code{logical(1)}] \cr
#' 			Supports missing values? Pass only when x is a string.
#' @param multiclass [\code{logical(1)}] \cr
#' 			Supports multiclass problems? Pass only when x is a string.
#' @param weights [\code{logical(1)}] \cr
#' 			Supports case weights? Pass only when x is a string.
#' @param probs [\code{logical(1)}] \cr
#' 			Can predict probabilities?
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
        probs = NA){
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
				( is.na(doubles) || doubles == x["numerics"] ) &&
						( is.na(factors) || factors == x["factors"] ) &&
						( is.na(characters) || characters == x["characters"] ) &&
						( is.na(missings) || missings == x["missings"] ) &&
						( is.na(multiclass) || multiclass == x["multiclass"] ) &&
						( is.na(weights) || weights == x["weights"]  ) &&
						( is.na(probs) || probs == x["prob"] )
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
		
		def = function(x, probs=NA) {
			type = ifelse(x@desc@type, "classif", "regr")

      doubles = ifelse(x@desc@n.feat["double"]>0, TRUE, NA)
      factors = ifelse(x@desc@n.feat["fact"]>0, TRUE, NA)
      missings = ifelse(x["has.missing"], TRUE, NA)
      weights = ifelse(x["has.weights"], TRUE, NA)
      
      if (type == "classif") {
        multiclass = ifelse(length(getClassLevels(x)) == 2, NA, TRUE)
        wls = getLearners(type, doubles, factors, characters, missings, weights, 
            multiclass, probs)
      } else {
        wls = getLearners(type, doubles, factors, characters, missings, weights) 
      }	 
			return(wls)
		}
)			

