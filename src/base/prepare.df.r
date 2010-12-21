roxygen()
#' @exportClass prepare.control
#' @rdname prepare.control-class
#' @title Control object for data preparation.
#' @seealso \code{\link{make.task}}

setClass(
		"prepare.control",
		representation = representation(
				ints.as = "character",
				chars.as = "character",
				drop.class.levels = "logical",
				impute.inf = "numeric",
				impute.large = "numeric"
		)
)

#---------------- constructor---- -----------------------------------------------------

#' Constructor.

setMethod(
		f = "initialize",
		signature = signature("prepare.control"),
		def = function(.Object, ints.as, chars.as, drop.class.levels, impute.inf, impute.large) {
			.Object@ints.as = ints.as
			.Object@chars.as = chars.as
			.Object@drop.class.levels = drop.class.levels
			.Object@impute.inf = impute.inf
			.Object@impute.large = impute.large
			return(.Object)
		}
)


roxygen()
#' Construct a control object for data preparation.
#'
#' @param ints.as [string]\cr
#'   Should integer input variables be converted to either "numeric" or "factor". Default is "numeric".
#' @param chars.as [string]\cr
#'   Default is "factor".
#' @param drop.class.levels [logical]\cr
#'   Should empty class levels be dropped? Default is TRUE.
#' @param impute.inf [string]\cr
#'   Value infinite values in the data are replaced by. Default ist .Machine$double.xmax. 
#' @return \code{\linkS4class{prepare.control}}.
#' @export
#' @rdname prepare.control
#' @title Construct prepare.control object.


prepare.control = function(ints.as = "numeric", chars.as = "factor", drop.class.levels = TRUE, impute.inf = .Machine$double.xmax, impute.large = NA) {
	new("prepare.control", ints.as, chars.as, drop.class.levels, impute.inf, impute.large)
}


prep.data = function(is.classif, data, target, exclude=c(), control) {
	
	ints.as = control@ints.as
	chars.as = control@chars.as
	drop.class.levels = control@drop.class.levels
	impute.inf = control@impute.inf
	impute.large = control@impute.large
	
	if (is.classif && !is.null(data[[target]])) {
		targets = data[, target]
		
		#convert target to factor
		if (!is.factor(targets)) {
			if(is.integer(data[, target]) || is.character(targets) || is.logical(factor)) {
				if (.mlr.local$errorhandler.setup$on.convert.var == "warn")
					warning("Converting target col. to factor.")
				data[, target] = as.factor(targets)
			} else {
				stop("Unsuitable target col. for classification data!")				
			}
		}	
		
		targets = data[, target]
		
		# drop unused class levels
		if (drop.class.levels) {
			before.drop <- levels(targets)
			data[, target] <- targets[, drop=TRUE]
			after.drop <- levels(data[, target])
			if(!identical(before.drop, after.drop)) {
				warning(paste("Empty levels were dropped from class col.:", 
								setdiff(before.drop, after.drop)))
			}	
		}
	}	
	
	cns = colnames(data)
	exclude = c(exclude, target)
	for (i in 1:ncol(data)) {
		cn = cns[i]
		v = data[, i]
		if (!(cn  %in% exclude)) {
			if (ints.as == "numeric" && is.integer(v)) {
				data[,i] = as.numeric(v)
				if (.mlr.local$errorhandler.setup$on.convert.var == "warn")
					warning("Converting integer variable to numeric: ", cn)
			}
			if (ints.as == "factor" && is.integer(v)) {
				data[,i] = as.factor(v)
				if (.mlr.local$errorhandler.setup$on.convert.var == "warn")
					warning("Converting integer variable to factor: ", cn)
			}
			if (chars.as == "factor" && is.character(v)) {
				data[,i] = as.factor(v)
				if (.mlr.local$errorhandler.setup$on.convert.var == "warn")
					warning("Converting char variable to factor: ", cn)
			}
			if (is.numeric(v) && any(is.infinite(v))) {
				v[is.infinite(v)] = sign(v[is.infinite(v)]) * impute.inf  
				data[,i] = v
				if (.mlr.local$errorhandler.setup$on.convert.var == "warn")
					warning("Converting inf values to +-", impute.inf, ": ", cn)
			}
		}
	}
	
	return(data)    
}



