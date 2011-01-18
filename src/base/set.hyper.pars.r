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
      pds = learner["par.descs"]
      for (i in seq(length=length(par.vals))) {
        n = ns[i]
        p = par.vals[[i]]
        pd = pds[[n]]
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
          learner@par.vals[[n]] = p
        }
      }
      return(learner)
    } 
)


