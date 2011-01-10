##double    
#if (missing(data.type))
#  data.type = ifelse(is.integer(lower) || is.integer(upper) || is.integer(default), "integer", "numeric")
#.Object@lower = lower					
#.Object@upper = upper
#if (missing(default))
#  default = 
#    .Object@deafult
#if (!(default == "missing" || (lower <= default && upper >= default)))
#  stop("Default value of par. ", name, " has to be in lower/upper limits or 'missing'!")
#callNextMethod(.Object, name, default, when, flags, requires)
#}





#.Object@name = name						
#.Object@default = default						
#.Object@when = when
#if (!(when %in% c("train", "predict", "both")))
#  stop("par.desc ", name, " : Arg 'when' can only be 'train', 'predict' or 'both', not '", when, "'!")
#.Object@flags = flags
#if (length(flags) > 0) { 
#  ns = names(flags)
#  if (!all.els.named(flags) || any(duplicated(ns)))
#    stop("par.desc: ", name, " : All elements of flag list have to be uniquely named!")
#  if (!all(ns %in% c("optimize", "pass.default")))
#    stop("par.desc: ", name, " : Only flags 'optimize' and 'pass.default' are supported!")
#  if (!all(sapply(flags, function(x) is.logical(x) || length(x)==1)))
#    stop("par.desc: ", name, " : Only boolean flags are supported!")
#}
#.Object@requires = requires						
#return(.Object)

#
##' Constructor.
#setMethod(
#  f = "initialize",
#  signature = signature("par.desc.disc"),
#  def = function(.Object, name, default="missing", when="train", vals, flags=list(), requires=expression(TRUE)) {
#    if (default != "missing") {
#      if (is.character(default) && default %in% names(vals))
#        default = vals[[default]]
#      y = sapply(vals, function(x) isTRUE(all.equal(x, default)))
#      if (!(any(y)))
#        stop("Default value of par. ", name,  " has to be among allowed values!")
#    }
#    .Object@vals = vals					
#    callNextMethod(.Object, name, default, when, flags, requires)
#  }
#)

