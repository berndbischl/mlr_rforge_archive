#' Control object for data preparation.
#' 
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
				impute.large = "numeric",
        large = "numeric"
    )
)

#' Constructor.

setMethod(
		f = "initialize",
		signature = signature("prepare.control"),
		def = function(.Object, ints.as, chars.as, drop.class.levels, impute.inf, impute.large, large) {
			.Object@ints.as = ints.as
			.Object@chars.as = chars.as
			.Object@drop.class.levels = drop.class.levels
			.Object@impute.inf = impute.inf
			.Object@impute.large = impute.large
      .Object@large = large
      return(.Object)
		}
)


#' Construct a control object for data preparation.
#'
#' @param ints.as [string]\cr
#'   Should integer input variables be converted to either "numeric" or "factor". Default is "numeric".
#' @param chars.as [string]\cr
#'   Conversion of character input variables. Currently only "factor" is supported.
#' @param drop.class.levels [boolean]\cr
#'   Should empty class levels be dropped? Default is TRUE.
#' @param impute.inf [numeric]\cr
#'   Value infinite values in the data are replaced by. Default is \code{.Machine$double.xmax}. 
#' @param impute.large [numeric]\cr
#'   Value large (absolute) values in the data are replaced by (capping). Default is plus/minus \code{.Machine$double.xmax}. 
#' @param large [numeric]\cr
#'   Threshold for capping in \code{impute.large}. Default is \code{Inf}. 
#' 
#' @return \code{\linkS4class{prepare.control}}.
#' @export
#' @rdname prepare.control
#' @title Construct prepare.control object.


prepare.control = function(ints.as = "numeric", chars.as = "factor", drop.class.levels = TRUE, 
  impute.inf = .Machine$double.xmax, impute.large = .Machine$double.xmax, large = Inf) {
	new("prepare.control", ints.as, chars.as, drop.class.levels, impute.inf, impute.large, large)
}


prep.data = function(is.classif, data, target, exclude=c(), control) {
	
	ints.as = control@ints.as
	chars.as = control@chars.as
	drop.class.levels = control@drop.class.levels
	impute.inf = control@impute.inf
	impute.large = control@impute.large
	large = control@large
  
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
  conv.in = conv.if = conv.cf = character(0) 
	for (i in 1:ncol(data)) {
		cn = cns[i]
		v = data[, i]
		if (!(cn  %in% exclude)) {
			if (ints.as == "numeric" && is.integer(v)) {
        conv.in = c(conv.in, cn)
				data[,i] = as.numeric(v)
			}
			if (ints.as == "factor" && is.integer(v)) {
        conv.if = c(conv.if, cn)
        data[,i] = as.factor(v)
			}
			if (chars.as == "factor" && is.character(v)) {
        conv.cf = c(conv.cf, cn)
        data[,i] = as.factor(v)
			}
      # infs
			if (is.numeric(v)) {
        j = is.infinite(v)
        if (any(j)) {
          conv.inf = c(conv.inf, cn)
          v[j] = sign(v[j]) * impute.inf  
          data[,i] = v
        }
			}
      # large
      if (is.numeric(v)) {
        j = abs(v) > large 
        if (any(j)) {
          conv.large = c(conv.large, cn)
          v[j] = sign(v[j]) * impute.large  
          data[,i] = v
        }
      }
    }
	}
  if (.mlr.local$errorhandler.setup$on.convert.var == "warn") {
    if (length(conv.in) > 0)
      warning("Converting integer variable to numeric: ", paste(conv.in, collapse=","))
    if (length(conv.if) > 0)
      warning("Converting integer variable to factor: ", paste(conv.if, collapse=","))
    if (length(conv.cf) > 0)
      warning("Converting char variable to factor: ", paste(conv.cf, collapse=","))
    if (length(conv.inf) > 0)
      warning("Converting inf values to +-", impute.inf, ": ", paste(conv.inf, collapse=","))
    if (length(conv.large) > 0)
      warning("Converting large values to +-", impute.large, ": ", paste(conv.large, collapse=","))
  }  
  
	return(data)    
}



