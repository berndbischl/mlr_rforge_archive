#' Set hyperparameters of learner object. 
#' 
#' @param learner [\code{\linkS4class{learner}}]\cr 
#'        Learner object.   
#' @param ... [any] \cr
#'        Optional named (hyper)parameters. Alternatively, you can pass via the "par.vals" argument.
#' @param par.vals [list] \cr
#'       Optional list of named (hyper)parameters. Alternatively, you can pass via the ... argument.
#' 		    
#' @return \code{\linkS4class{learner}} with changed hyperparameters.
#' @exportMethod set.hyper.pars
#' @title Set hyperparamters of learner object.
#' @rdname set.hyper.pars 

setGeneric(
    name = "set.hyper.pars",
    def = function(learner, ..., par.vals) {
      x = list(...)      
      if (missing(par.vals))
        par.vals = list()
      if(!all.els.named(x))
        stop("All parameter settings have to be named arguments!")
      par.vals = insert(par.vals, x)
      standardGeneric("set.hyper.pars")
    }
)

#' @rdname set.hyper.pars 
setMethod(
    f = "set.hyper.pars",
    
    signature = signature(
        learner="learner", 
        par.vals="list" 
    ),
    
    def = function(learner, ..., par.vals) {
      ns = names(par.vals)
      pars = learner@par.set@pars
      for (i in seq(length=length(par.vals))) {
        n = ns[i]
        p = par.vals[[i]]
        pd = pars[[n]]
        if (is.null(pd)) {
          # no description: stop warn or quiet
          msg = paste(class(learner), ": Setting par ", n, " without description!", sep="")
          if (.mlr.local$errorhandler.setup$on.par.without.desc == "stop")
            stop(msg)
          if (.mlr.local$errorhandler.setup$on.par.without.desc == "warn")
            warning(msg)
          learner@par.vals[[n]] = p
        } else {
          isf = is.feasible(p, pd)
          if (length(isf) != 1 || !isf)
            stop("'", n, "' must be a feasible parameter setting.")
          # if valname of discrete par was used, transform it to real value
          if ((pd@type == "discrete" || pd@type == "ordered") 
            && is.character(p) && length(p) == 1 && p %in% names(pd@constraints$vals))
            p = pd@constraints$vals[[p]]
          learner@par.vals[[n]] = p
        }
      }
      return(learner)
    } 
)


