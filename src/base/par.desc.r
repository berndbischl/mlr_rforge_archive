
#' Description class for a hyperparameter.
#'  
#' Getter.\cr
#' 
#' \describe{
#'  \item{par.name [string]}{Name of parameter in learning algorithm.}
#'  \item{default [any]}{Default value.}
#'  \item{data.type [string]}{'numeric', 'factor', 'integer', 'unknown'.}
#'  \item{when [string]}{Specifies when a cetrain hyperparameter is used. Possible entries are 'train', 'predict' or 'both'.}
#'  \item{requires [list]}{Requirements for a parameter to be effective.}
#' }
#' @exportClass par.desc
#' @title Description class for a hyperparameter. 


setClass(
	"par.desc",
	representation = representation(
		requires = "list",	
		par.name = "character",
		default = "ANY",
		data.type = "character",
		when = "character"
	)	
)



setClass(
	"par.desc.unknown",
	contains = c("par.desc")
)


setClass(
	"par.desc.num",
	contains = c("par.desc"),
	representation = representation(
		lower = "numeric",
		upper = "numeric"
	)	
)

setClass(
	"par.desc.disc",
	contains = c("par.desc"),
	representation = representation(
		vals = "list"	
	)	
)


setClass(
	"par.desc.complex",
	contains = c("par.desc"),
	representation = representation(
		prior = "ANY",
		estimate = "function"
	)	
)