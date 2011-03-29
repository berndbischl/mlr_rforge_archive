#' @exportClass prepare.control
#' @rdname prepare.control
setClass(
		"prepare.control",
		representation = representation(
				ints.as = "character",
				chars.as = "character",
        logs.as = "character",
        drop.class.levels = "logical"
    )
)

#' Constructor.

setMethod(
		f = "initialize",
		signature = signature("prepare.control"),
		def = function(.Object, ints.as, chars.as, logs.as, drop.class.levels) {
			
      .Object@ints.as = ints.as
			.Object@chars.as = chars.as
      .Object@logs.as = logs.as
      .Object@drop.class.levels = drop.class.levels
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
#' @param drop.class.levels [boolean]\cr
#'   Should empty class levels be dropped? Default is TRUE.
#' @return Control structure for data preparation.
#' @export
#' @usage prepare.control(ints.as, chars.as, logs.as, drop.class.levels = TRUE) 
#' @rdname prepare.control
#' @title Control for basic basic data preparation.


prepare.control = function(ints.as = c("numeric", "factor"),  chars.as = c("factor"),  logs.as = c("factor", "numeric"), 
  drop.class.levels = TRUE) {
  
  ints.as = match.arg(ints.as)
  chars.as = match.arg(chars.as)
  logs.as = match.arg(logs.as)
  new("prepare.control", ints.as, chars.as, logs.as, drop.class.levels)
}


prep.data = function(is.classif, data, target, control) {
	
	ints.as = control@ints.as
	chars.as = control@chars.as
  logs.as = control@logs.as
  drop.class.levels = control@drop.class.levels
  
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
        stop("Unsuitable target col. for regression data!")       
      }
    } 
    targets = data[, target]
  } 
  
  
	cns = colnames(data)
  conv.in=conv.if=conv.ln=conv.lf=conv.cf=character(0) 
  for (i in 1:ncol(data)) {
    cn = cns[i]
    v = data[, i]
    if (cn  != target) {
      if (length(class(v)) == 1 && class(v) == "numeric") {
        # numeric
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
      } else if (is(v, "factor")) {
        #factor
        # do nothing, we are ok
      } else {
        stop("Encountered unsupported feature: ", cn, " ", as.character(class(v))[1])        
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
  }   
	return(data)    
}