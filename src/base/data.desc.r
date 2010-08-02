#' @include object.r
roxygen()

#' Description object for data.
#' 
#' Getter.\cr
#' 
#' \describe{
#'  \item{target [string]}{Name of target variable.}
#'  \item{excluded [character]}{Names of excluded covariates.}
#'  \item{size [integer]}{Number of cases.}
#'  \item{dim [integer]}{Number of covariates.}
#'  \item{excluded [character]}{Names of excluded variables.}
#'  \item{n.num [integer]}{Number of numerical covariates.}
#'  \item{n.int [integer]}{Number of integer covariates.}
#'  \item{n.fact [integer]}{Number of factor covariates.}
#'  \item{n.char [integer]}{Number of character covariates.}
#' 	\item{has.missing [boolean]}{Are missing values present?}
#'  \item{is.classif [logical]}{Factor target variable?}
#' 	\item{is.regr [boolean]}{Numerical target variable?}
#'  \item{class.levels [character]}{Possible classes. NA if not classification.}
#'  \item{class.nr [integer]}{Number of classes. NA if not classification.}
#'  \item{class.dist [integer]}{Class distribution. Named vector. NA if not classification.}
#'	\item{is.binary [logical]}{Binary classification?. NA if not classification.}
#' }
#' @exportClass data.desc
#' @title Description object for data. 
#' 

setClass(
		"data.desc",
		contains = c("object"),
		representation = representation(
				props = "list"
		)
)


setMethod(
		f = "initialize",
		signature = signature("data.desc"),
		def = function(.Object, data, target, excluded) {
			i = which(colnames(data) %in% c(target, excluded))
			df2 = data[, -i, drop=F]
			.Object@props$target = target 
			.Object@props$excluded = excluded 
			.Object@props$obs = nrow(data)
			inputs = c()
			inputs = c(
					n.num = sum(sapply(df2, is.numeric)), 
					n.int  = sum(sapply(df2, is.integer)),
					n.fact = sum(sapply(df2, is.factor)),
					n.char = sum(sapply(df2, is.character))
			)
			.Object@props$inputs = inputs
			.Object@props$has.missing = any(is.na(iris))
			y = data[, target]
			if(is.factor(y))
				.Object@props$classes =	{tab=table(y);cl=as.integer(tab); names(cl)=names(tab);cl}
			else
				.Object@props$classes =	as.integer(NA)
			return(.Object)
		}
)


#' @rdname data.desc-class
setMethod(
		f = "[",
		signature = signature("data.desc"),
		def = function(x,i,j,...,drop) {
			if (i == "target") 
				return(x@props$target)
			if (i == "excluded") 
				return(x@props$excluded)
			if (i == "size") 
				return(x@props$obs)
			if (i == "dim") 
				return(sum(x@props$inputs))
			if (i == "n.num") 
				return(as.integer(x@props$inputs["n.num"]))
			if (i == "n.int") 
				return(as.integer(x@props$inputs["n.int"]))
			if (i == "n.fact") 
				return(as.integer(x@props$inputs["n.fact"]))
			if (i == "n.char") 
				return(as.integer(x@props$inputs["n.char"]))
			if (i == "has.missing") 
				return(length(x["rows.with.missings"]) > 0)
			if (i == "is.classif") 
				return(all(!is.na(x@props$classes)))
			if (i == "is.regr") 
				return(!x["is.classif"])
			if (i == "class.levels") 
				if(x["is.classif"]) return(names(x@props$classes)) else return(as.character(NA))
			if (i == "class.nr") 
				if(x["is.classif"]) return(length(x@props$classes)) else return(as.integer(NA))
			if (i == "class.dist") 
				if(x["is.classif"]) return(x@props$classes) else return(as.integer(NA))
			if (i == "is.binary") 
				if(x["is.classif"]) return(x["class.nr"] == 2) else return(as.logical(NA))
			callNextMethod()
		}
)







