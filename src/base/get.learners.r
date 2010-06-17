#' Returns the names of learning algorithms which have specific characteristics, e.g.
#' whether it supports missing values, misclassification costs, case weights,...
#' 
#' The default of all boolean parameter (supports.[...]) is NA, means: no specification no
#' inclusion in the search.
#' 
#' @param type [character] \cr
#' 			Type of the learning algorithm, either "classif" or "regr"
#' @param supports.numerics [boolean] \cr
#' 			Supports numeric inputs?
#' @param supports.factors [boolean] \cr
#' 			Supports factor inputs?
#' @param supports.characters [boolean] \cr
#' 			Supports character inputs?
#' @param supports.missings [boolean] \cr
#' 			Supports missing values?
#' @param supports.multiclass [boolean] \cr
#' 			Supports multiclass problems?
#' @param supports.weights [boolean] \cr
#' 			Supports case weights?
#' @param supports.probs [boolean] \cr
#' 			Can predict probabilities?
#' @param supports.decision [boolean] \cr
#' 			Supports decision values?
#' @param supports.costs [boolean] \cr
#' 			Supports non-standard misclassification costs?
#' 
#' @rdname get.learners
#' @export 
#' 
#' @title Find learning algorithms with specific properties.



get.learners <- function(
					type = c("classif","regr"), 
					supports.numerics = NA, 
					supports.factors = NA,
					supports.characters = NA,
					supports.missings = NA,
					supports.multiclass = NA,
					supports.weights = NA,
					supports.probs = NA,
					supports.decision = NA,
					supports.costs = NA){
					
		mlr.classes <- getClasses(where = getNamespace("mlr"))
		top.cl <- ifelse(type == "classif", "rlearner.classif", "rlearner.regr")
		ls <- Filter(function(x) extends(x, top.cl) && x != top.cl , mlr.classes)
		
		f <- function(x) {
			wl <- try(make.learner(x))
			if(is (wl, "try-error")) 
				return(NULL)
			else
				return(wl)
		}
	
		ls <- lapply(ls, f)
		ls <- Filter(function(x) !is.null(x), ls)
		
		
		f <- function(x) {
			( is.na(supports.numerics) || supports.numerics == x["supports.numerics"] ) &&
			( is.na(supports.factors) || supports.factors == x["supports.factors"] ) &&
			( is.na(supports.characters) || supports.characters == x["supports.characters"] ) &&
			( is.na(supports.missings) || supports.missings == x["supports.missings"] ) &&
			( is.na(supports.multiclass) || supports.multiclass == x["supports.multiclass"] ) &&
			( is.na(supports.weights) || supports.weights == x["supports.weights"]  ) &&
			( is.na(supports.probs) || supports.probs == x["supports.probs"] ) &&
			( is.na(supports.decision) || supports.decision == x["supports.decision"]  ) &&
			( is.na(supports.costs) || supports.costs == x["supports.costs"]  )
		}
		
		ls <- Filter(f, ls)
		ls <- sapply(ls, function(x) as.character(class(x)))
		
		return(ls)
}