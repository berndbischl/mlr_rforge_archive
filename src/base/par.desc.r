
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
		par.name = "character",
		default = "ANY",
		when = "character",
		optimize = "logical",
		requires = "expression"	
	)	
)

setMethod(
		f = "initialize",
		signature = signature("par.desc"),
		def = function(.Object, par.name, default, when="train", optimize=TRUE, requires=expression(TRUE)) {
			if (missing(par.name))
				return(.Object)
			.Object@par.name = par.name						
			.Object@default = default						
			.Object@when = when
			if (!(when %in% c("train", "predict", "when")))
				stop("Arg 'when' can only be 'train', 'predict' or 'both', not:", when)
			.Object@optimize = optimize						
			.Object@requires = requires						
			return(.Object)
		}
)




setClass(
	"par.desc.unknown",
	contains = c("par.desc")
)


setClass(
	"par.desc.num",
	contains = c("par.desc"),
	representation = representation(
		data.type = "character",
		lower = "numeric",
		upper = "numeric"
	)	
)

setMethod(
		f = "initialize",
		signature = signature("par.desc.num"),
		def = function(.Object, par.name, data.type, default="missing", when="train", lower, upper, optimize=TRUE, requires=expression(TRUE)) {
			if (!(data.type %in% c("integer", "numerical")))
				stop("Arg 'data.type' can only be 'integer' or 'numerical', not: ", data.type)
			.Object@data.type = data.type						
			.Object@lower = lower					
			.Object@upper = upper	
			if (!(default == "missing" || (lower <= default && upper >= default)))
				stop("Default value of par. ", par.name, " has to be in lower/upper limits or 'missing'!")
			callNextMethod(.Object, par.name, default, when, optimize, requires)
		}
)

setClass(
	"par.desc.disc",
	contains = c("par.desc"),
	representation = representation(
		vals = "list"	
	)	
)

setMethod(
		f = "initialize",
		signature = signature("par.desc.disc"),
		def = function(.Object, par.name, default="missing", when="train", vals, optimize=TRUE, requires=expression(TRUE)) {
			if (is.vector(vals))
				vals = as.list(vals)
			if (!(default %in% vals))
				stop("Default value of par. ", par.name,  " has to be among allowed values!")
			.Object@vals = vals					
			callNextMethod(.Object, par.name, default, when, optimize, requires)
		}
)

setClass(
	"par.desc.complex",
	contains = c("par.desc"),
	representation = representation(
		prior = "ANY",
		estimate = "function"
	)	
)