# todo: maxit and maxvars should be integer?
# but this changes the object just check internally....


setClass(
		"varsel.control",
		contains = c("opt.control"),
		representation = representation(
				compare = "character",
				max.vars = "integer", 
				maxit = "integer",
				alpha = "numeric", 
				beta = "numeric", 
				gamma = "numeric", 
				delta = "numeric", 
				epsilon = "numeric"				
		)
)

#todo document

#' Control structure for variable selection. 
#' 
#' @param ranges [\code{\link{list}}] \cr 
#' 		A list of named vectors/lists of possible values for each hyperparameter. 
#'      You can also pass a list of such ranges by using [\code{\link{combine.ranges}}] 
#'      in the rare case when it does not make sense to search a complete cross-product of range values.     
#' @param minimize [logical] \cr 
#'       Minimize performance measure? Default is TRUE. 
#' @param maxit [integer] \cr 
#'       Maximal number of variable sets to evaluate. Default is 100.
#' @param maxvars [integer] \cr 
#'       Maximal number of allowd variable in the final set. Default is Inf.
#' 		    
#' @return Control structure for tuning.
#' @export 
#' @title Control for grid search tuning. 


varsel.control <- function(minimize=TRUE, compare="diff", maxit, max.vars,
		alpha=0.01, beta=0.01, gamma=0, delta=0, epsilon=0) {
	if (missing(maxit))
		maxit = .Machine$integer.max
	if (missing(max.vars))
		max.vars = .Machine$integer.max
	new("varsel.control", minimize=TRUE, method="varsel", compare=compare, max.vars=max.vars, maxit=maxit,
			alpha=alpha, beta=beta, gamma=gamma, delta=delta, epsilon=epsilon)
}

