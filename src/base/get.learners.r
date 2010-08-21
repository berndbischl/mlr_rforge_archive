#' Returns the names of learning algorithms which have specific characteristics, e.g.
#' whether it supports missing values, misclassification costs, case weights,...
#' 
#' The default of all boolean parameters is NA, meaning: property is not included in search.
#' 
#' @param type [character] \cr
#' 			Type of the learning algorithm, either "classif" or "regr" or NA (=don't care).
#' @param numerics [boolean] \cr
#' 			Supports numeric inputs?
#' @param factors [boolean] \cr
#' 			Supports factor inputs?
#' @param characters [boolean] \cr
#' 			Supports character inputs?
#' @param missings [boolean] \cr
#' 			Supports missing values?
#' @param multiclass [boolean] \cr
#' 			Supports multiclass problems?
#' @param weights [boolean] \cr
#' 			Supports case weights?
#' @param probs [boolean] \cr
#' 			Can predict probabilities?
#' @param decision [boolean] \cr
#' 			Supports decision values?
#' @param costs [boolean] \cr
#' 			Supports non-standard misclassification costs?
#' 
#' @rdname get.learners
#' @export 
#' 
#' @title Find learning algorithms with specific properties.



get.learners <- function(
					type = NA, 
					numerics = NA, 
					factors = NA,
					characters = NA,
					missings = NA,
					multiclass = NA,
					weights = NA,
					probs = NA,
					decision = NA,
					costs = NA){
					
		mlr.classes <- getClasses(where = getNamespace("mlr"))
		if(is.na(type)) 
			type = "na"
		top.cl = switch(type, classif="rlearner.classif", regr="rlearner.regr", na="rlearner")
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
			( is.na(numerics) || numerics == x["numerics"] ) &&
			( is.na(factors) || factors == x["factors"] ) &&
			( is.na(characters) || characters == x["characters"] ) &&
			( is.na(missings) || missings == x["missings"] ) &&
			( is.na(multiclass) || multiclass == x["multiclass"] ) &&
			( is.na(weights) || weights == x["weights"]  ) &&
			( is.na(probs) || probs == x["probs"] ) &&
			( is.na(decision) || decision == x["decision"]  ) &&
			( is.na(costs) || costs == x["costs"]  )
		}
		
		ls <- Filter(f, ls)
		ls <- sapply(ls, function(x) as.character(class(x)))
		
		return(ls)
}