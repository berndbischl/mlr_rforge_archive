roxygen()

#' get.learners returns the names of learning algorithms which have specific characteristics, e.g.
#' whether it supports missing values, misclassification costs, case weights,...
#' 
#' @param type [character] \cr
#' 			Type of the learning algorithm, either "classif" or "regr"
#' @param supports.numerics [boolean] \cr
#' 			Looking for an algorithm supporting numeric inputs?
#' @param supports.factors [boolean] \cr
#' 			Looking for an algorithm supporting factor inputs?
#' @param supports.characters [boolean] \cr
#' 			Looking for an algorithm supporting character inputs?
#' @param supports.missings [boolean] \cr
#' 			Looking for an algorithm which can handle missing values?
#' @param supports.multiclass [boolean] \cr
#' 			Looking for a classification algorithm supporting data sets with more than two classes?
#' @param supports.weights [boolean] \cr
#' 			Looking for an algorithm which can use case weights?
#' @param supports.probs [boolean] \cr
#' 			Looking for an algorithm which can predict probabilities?
#' @param supports.decision [boolean] \cr
#' 			Looking for an algorithm supporting decision values?
#' @param supports.costs [boolean] \cr
#' 			Looking for an algorithm which can handle misclassification costs?
#' 
#' @rdname get.learners
#' 
#' @usage get.learners(type, supports.numerics, supports.factors, supports.characters, supports.missings, 
#' supports.multiclass, supports.weights, supports.probs,supports.decision, supports.costs)
#' 
#' @title Function to get specific learning algorithms



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