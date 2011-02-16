#' @exportClass prepare.control
#' @rdname prepare.control

setClass(
		"prepare.control",
		representation = representation(
				ints.as = "character",
				chars.as = "character",
        logs.as = "character",
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
		def = function(.Object, ints.as, chars.as, logs.as, drop.class.levels, impute.inf, impute.large, large) {
			.Object@ints.as = ints.as
			.Object@chars.as = chars.as
      .Object@logs.as = logs.as
      .Object@drop.class.levels = drop.class.levels
			.Object@impute.inf = impute.inf
			.Object@impute.large = impute.large
      .Object@large = large
      return(.Object)
		}
)


#' Control structure for basic data preparation.
#'
#' @param ints.as [string]\cr
#'   Should integer input variables be converted to either "double" or "factor". Default is "double".
#' @param chars.as [string]\cr
#'   Conversion of character input variables. Currently only "factor" is supported.
#' @param logs.as [string]\cr
#'   Should logical input variables be converted to either "double" or "factor". Default is "factor".
#' @param drop.class.levels [boolean]\cr
#'   Should empty class levels be dropped? Default is TRUE.
#' @param impute.inf [numeric]\cr
#'   Value infinite values in the data are replaced by. Default is \code{.Machine$double.xmax}. 
#' @param impute.large [numeric]\cr
#'   Value large (absolute) values in the data are replaced by (capping). Default is plus/minus \code{.Machine$double.xmax}. 
#' @param large [numeric]\cr
#'   Threshold for capping in \code{impute.large}. Default is \code{Inf}. 
#' 
#' @return Control structure for data preparation.
#' @export
#' @rdname prepare.control
#' @title Control for basic basic data preparation.


prepare.control = function(
  ints.as = c("double", "factor"), 
  chars.as = c("factor"), 
  logs.as = c("factor", "double"), 
  drop.class.levels = TRUE,
  impute.inf = .Machine$double.xmax, impute.large = .Machine$double.xmax, large = Inf) {
  
  match.arg(ints.as)
  match.arg(chars.as)
  match.arg(logs.as)
  new("prepare.control", ints.as, chars.as, logs.as, drop.class.levels, impute.inf, impute.large, large)
}


prep.data = function(is.classif, data, target, control) {
	
	ints.as = control@ints.as
	chars.as = control@chars.as
  logs.as = control@logs.as
  drop.class.levels = control@drop.class.levels
	impute.inf = control@impute.inf
	impute.large = control@impute.large
	large = control@large
  
  cns = colnames(data)
  
  #todo: these are all candidates for bad chars
  #! " # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~
  bad.chars = c("\\[", "]", "\\(", ")", "{", "}", ",", "\\+", "-", "\\*", "/", "=", "\\$", "~")
  bcs.collapsed = paste(sapply(bad.chars, function(x) substr(x, nchar(x), nchar(x))), collapse=" ")
  if (any(sapply(bad.chars, function(bc) length(grep(bc, target)) > 0)))
    stop("Target name contains one of the special characters: ", bcs.collapsed ,". You have to rename it!")
        
  if (any(sapply(bad.chars, function(bc) length(grep(bc, cns)) > 0))) {
    if (.mlr.local$errorhandler.setup$on.convert.varname == "stop")
      stop("Feature name contains one of the special characters: ", bcs.collapsed)
    if (.mlr.local$errorhandler.setup$on.convert.varname == "warn")
      warning("Converting feature names containing special characters: ", bcs.collapsed)
    for (bc in bad.chars) {
      # take last int code when escaping regexp
      cns = gsub(pattern=bc, replacement=rev(utf8ToInt(bc))[1], cns)
    }
    colnames(data) = cns
  }

  if (is.classif && !is.null(data[[target]])) {
		targets = data[, target]
		
		#convert target to factor
		if (!is.factor(targets)) {
			if(is.integer(targets) || is.character(targets) || is.logical(targets)) {
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
  #regression
  if (!is.classif && !is.null(data[[target]])) {
    targets = data[, target]
    #convert target to numeric
    if (!is.numeric(targets)) {
      if(is.integer(targets)) {
        if (.mlr.local$errorhandler.setup$on.convert.var == "warn")
          warning("Converting target col. to numeric.")
        data[, target] = as.numeric(targets)
      } else {
        stop("Unsuitable target col. for classification data!")       
      }
    } 
    targets = data[, target]
  } 
  
  
	cns = colnames(data)
  conv.in = conv.if = conv.ln = conv.lf = conv.cf = conv.inf  = conv.large = character(0) 
	for (i in 1:ncol(data)) {
		cn = cns[i]
		v = data[, i]
		if (cn  != target) {
      if (is.integer(v)) {
        if (ints.as == "double") {
          conv.in = c(conv.in, cn)
          data[,i] = as.numeric(v)
        } else if (ints.as == "factor") {
          conv.if = c(conv.if, cn)
          data[,i] = as.factor(v)
        } else 
          stop("Integer inputs must be converted!")
      }
      if (is.character(v)) {
        if (chars.as == "factor") {
          conv.cf = c(conv.cf, cn)
          data[,i] = as.factor(v)
        } else 
          stop("Character inputs must be converted!")
      }
      if (is.logical(v)) {
        if (logs.as == "double") {
          conv.ln = c(conv.ln, cn)
          data[,i] = as.numeric(v)
        } else if (logs.as == "factor") {
          conv.lf = c(conv.lf, cn)
          data[,i] = as.factor(v)
        } else 
          stop("Logical inputs must be converted!")
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
        j = !is.na(v) & abs(v) > large 
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
      warning("Converting integer variable to double: ", paste(conv.in, collapse=","))
    if (length(conv.if) > 0)
      warning("Converting integer variable to factor: ", paste(conv.if, collapse=","))
    if (length(conv.ln) > 0)
      warning("Converting logical variable to double: ", paste(conv.ln, collapse=","))
    if (length(conv.lf) > 0)
      warning("Converting logical variable to factor: ", paste(conv.lf, collapse=","))
    if (length(conv.cf) > 0)
      warning("Converting char variable to factor: ", paste(conv.cf, collapse=","))
    if (length(conv.inf) > 0)
      warning("Converting inf values to +-", impute.inf, ": ", paste(conv.inf, collapse=","))
    if (length(conv.large) > 0)
      warning("Converting large values to +-", impute.large, ": ", paste(conv.large, collapse=","))
  }  
  
	return(data)    
}



