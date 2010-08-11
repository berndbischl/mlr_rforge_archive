
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
		flags = "list",
		requires = "expression"	
	)	
)

setMethod(
		f = "initialize",
		signature = signature("par.desc"),
		def = function(.Object, par.name, default, when="train", flags=list(), requires=expression(TRUE)) {
			if (missing(par.name))
				return(.Object)
			.Object@par.name = par.name						
			.Object@default = default						
			.Object@when = when
			if (!(when %in% c("train", "predict", "both")))
				stop("par.desc ", par.name, " : Arg 'when' can only be 'train', 'predict' or 'both', not '", when, "'!")
			.Object@flags = flags
			if (length(flags) > 0) { 
				ns = names(flags)
				if (!all.names(flags) || any(duplicated(ns)))
					stop("par.desc: ", par.name, " : All elements of flag list have to be uniquely named!")
				if (!(ns %in% c("optimize", "pass.default")))
					stop("par.desc: ", par.name, " : Only flags 'optimize' and 'pass.default' are supported!")
				if (!all(sapply(flags, function(x) is.logical(x) || length(x)==1)))
					stop("par.desc: ", par.name, " : Only boolean flags are supported!")
			}
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
		def = function(.Object, par.name, data.type, default="missing", when="train", lower=-Inf, upper=Inf, flags=list(), requires=expression(TRUE)) {
			if (missing(data.type))
				data.type = ifelse(is.integer(lower) || is.infinite(upper) || is.integer(default), "integer", "numerical")
			.Object@data.type = data.type						
			if (!(data.type %in% c("integer", "numerical")))
				stop("Arg 'data.type' can only be 'integer' or 'numerical', not: ", data.type)
			.Object@lower = lower					
			.Object@upper = upper	
			if (!(default == "missing" || (lower <= default && upper >= default)))
				stop("Default value of par. ", par.name, " has to be in lower/upper limits or 'missing'!")
			callNextMethod(.Object, par.name, default, when, flags, requires)
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
		def = function(.Object, par.name, default="missing", when="train", vals, flags=list(), requires=expression(TRUE)) {
			if (is.vector(vals))
				vals = as.list(vals)
			if (default != "missing" && !(default %in% vals))
				stop("Default value of par. ", par.name,  " has to be among allowed values!")
			.Object@vals = vals					
			callNextMethod(.Object, par.name, default, when, flags, requires)
		}
)

setClass(
		"par.desc.log",
		contains = c("par.desc")
)

setMethod(
		f = "initialize",
		signature = signature("par.desc.log"),
		def = function(.Object, par.name, default="missing", when="train", flags=list(), requires=expression(TRUE)) {
			if (!(is.logical(default) && length(default) == 1))
				stop("Default value of par. ", par.name,  " has to be a single boolean!")
			callNextMethod(.Object, par.name, default, when, flags, requires)
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