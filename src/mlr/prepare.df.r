#' @exportClass prepare.control
#' @rdname prepare.control

setClass(
		"prepare.control",
		representation = representation(
				ints.as = "character",
				chars.as = "character",
        logs.as = "character",
        Dates.as = "character",
        Dates.origin = "Date",
        POSIXcts.as = "character",
        POSIXcts.origin = "POSIXct",
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
		def = function(.Object, ints.as, chars.as, logs.as, Dates.as, Dates.origin, POSIXcts.as, POSIXcts.origin,   
      drop.class.levels, impute.inf, impute.large, large) {
			
      .Object@ints.as = ints.as
			.Object@chars.as = chars.as
      .Object@logs.as = logs.as
      .Object@Dates.as = Dates.as
      .Object@Dates.origin = Dates.origin
      .Object@POSIXcts.as = POSIXcts.as
      .Object@POSIXcts.origin = POSIXcts.origin
      .Object@drop.class.levels = drop.class.levels
			.Object@impute.inf = impute.inf
			.Object@impute.large = impute.large
      .Object@large = large
      return(.Object)
		}
)


#' Control structure for basic data preparation.
#'
#' @param ints.as [character(1)]\cr
#'   Should integer features be converted to either "numeric" or "factor". Default is "numeric".
#' @param chars.as [character(1)]\cr
#'   Conversion of character features. Currently only "factor" is supported.
#' @param logs.as [character(1)]\cr
#'   Should logical features be converted to either "numeric" or "factor". Default is "factor".
#' @param Dates.as [character(1)]\cr
#'   Conversion of Date features. Currently only "numeric" is supported, which converts to days since \code{Dates.origin}. 
#' @param Dates.origin [character(1)]\cr
#'   Reference point for \code{Dates.as}. Default is \code{as.Date("1970-01-01")}.
#' @param POSIXcts.as [character(1)]\cr
#'   Should POSIXct features be converted to either "seconds", "minutes", "hours", "days". Default is "seconds" since \code{Dates.origin}.
#' @param POSIXcts.origin [character(1)]\cr
#'   Reference point for \code{POSIXcts.as}. Default is \code{as.POSIXct("1970-01-01 01:00")}.
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
  ints.as = c("numeric", "factor"), 
  chars.as = c("factor"), 
  logs.as = c("factor", "numeric"), 
  Dates.as = c("numeric"),
  Dates.origin = as.Date("1970-01-01"),
  POSIXcts.as = c("seconds", "minutes", "hours", "days"), 
  POSIXcts.origin = as.POSIXct("1970-01-01 01:00"),
  drop.class.levels = TRUE,
  impute.inf = .Machine$double.xmax, impute.large = .Machine$double.xmax, large = Inf) {
  
  ints.as = match.arg(ints.as)
  chars.as = match.arg(chars.as)
  logs.as = match.arg(logs.as)
  Dates.as = match.arg(Dates.as)
  POSIXcts.as = match.arg(POSIXcts.as)
  check.arg(Dates.origin, "Date")
  check.arg(POSIXcts.origin, "POSIXct")
  new("prepare.control", ints.as, chars.as, logs.as, Dates.as, Dates.origin, POSIXcts.as, POSIXcts.origin, 
    drop.class.levels, impute.inf, impute.large, large)
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
  bad.chars = c("\\[", "]", "\\(", ")", "\\{", "}", ",", "\\+", "-", "\\*", "/", "=", "\\$", "~")
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
  conv.in=conv.if=conv.ln=conv.lf=conv.cf=conv.dn=conv.posix=conv.inf=conv.large=character(0) 
  for (i in 1:ncol(data)) {
    cn = cns[i]
    v = data[, i]
    if (cn  != target) {
      if (is.double(v)) {
        # numeric
        # infs 
        j = is.infinite(v)
        if (any(j)) {
          conv.inf = c(conv.inf, cn)
          v[j] = sign(v[j]) * impute.inf  
          data[,i] = v
        }
      } else if (is.integer(v)) {
        # integer
        if (ints.as == "numeric") {
          conv.in = c(conv.in, cn)
          data[,i] = as.numeric(v)
        } else if (ints.as == "factor") {
          conv.if = c(conv.if, cn)
          data[,i] = as.factor(v)
        } 
      } else if (is.character(v)) {
        # character
        if (chars.as == "factor") {
          conv.cf = c(conv.cf, cn)
          data[,i] = as.factor(v)
        } 
      } else if (is.logical(v)) {
        # logical 
        if (logs.as == "numeric") {
          conv.ln = c(conv.ln, cn)
          data[,i] = as.numeric(v)
        } else if (logs.as == "factor") {
          conv.lf = c(conv.lf, cn)
          data[,i] = as.factor(v)
        } 
      } else if (is(v, "Date")) {
        # Date
        if (control@Dates.as == "numeric") {
          conv.dn = c(conv.dn, cn)
          data[,i] = as.numeric(v) - as.numeric(control@Dates.origin)
        } 
      } else if (is(v, "POSIXct")) {
        # POSIXct
        conv.posix = c(conv.posix, cn)
        data[,i] = (as.numeric(v) - as.numeric(control@POSIXcts.origin)) / 
          c(seconds=1, minutes=60, hours=3600, days=3600*24)[control@POSIXcts.as]
      } else if (is(v, "factor")) {
        #factor
        # do nothing, we are ok
      } else {
        stop("Encountered unsupported feature: ", cn, " ", as.character(class(v))[1])        
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
      warning("Converting integer variable to numeric: ", paste(conv.in, collapse=","))
    if (length(conv.if) > 0)
      warning("Converting integer variable to factor: ", paste(conv.if, collapse=","))
    if (length(conv.ln) > 0)
      warning("Converting logical variable to numeric: ", paste(conv.ln, collapse=","))
    if (length(conv.lf) > 0)
      warning("Converting logical variable to factor: ", paste(conv.lf, collapse=","))
    if (length(conv.cf) > 0)
      warning("Converting char variable to factor: ", paste(conv.cf, collapse=","))
    if (length(conv.dn) > 0)
      warning("Converting Date variable to numeric days: ", paste(conv.dn, collapse=","))
    if (length(conv.posix) > 0)
      warning("Converting POSIXct to ", control@POSIXcts.as, ":", paste(conv.posix, collapse=","))
    if (length(conv.inf) > 0)
      warning("Converting inf values to +-", impute.inf, ": ", paste(conv.inf, collapse=","))
    if (length(conv.large) > 0)
      warning("Converting large values to +-", impute.large, ": ", paste(conv.large, collapse=","))
  }  
  
	return(data)    
}



