#' Set the hyperparameters of learner objects.
#' 
#' @param learner [\code{\linkS4class{Learner}}] \cr
#'        Learner object.
#' @param ... [any] \cr
#'        Optional named (hyper)parameters. Alternatively these can be given
#'        using the \code{par.vals} argument.
#' @param par.vals [\code{list}] \cr
#'        Optional list of named (hyper)parameters. The arguments in
#'        \code{...} take precedence over values in this list. We strongly
#'        encourage you to use one or the other to pass (hyper)parameters
#'        to the learner but not both.
#'
#' @seealso See \code{\link{getHyperPars}} for a function to retrieve
#'   the currently set hyper parameters. To get a list of all (hyper)parameters of
#'   a learner, see the \code{par.set} slot of the \code{\linkS4class{Learner}}
#'   object.
#'
#' @return \code{\linkS4class{Learner}} with changed hyperparameters.
#'
#' @example
#' 
#' if (require(kernlab)) {
#'   cl1 <- makeLearner("classif.ksvm", sigma=1)
#'   cl2 <- setHyperPars(cl1, sigma=10, par.vals=list(C=2))
#'   print(cl1)
#'   ## Note the now set and altered hyperparameters:
#'   print(cl2)
#' }
#'
#' @exportMethod setHyperPars
#' @title Set hyperparamters of learner object.
#' @rdname setHyperPars 
setGeneric(
    name = "setHyperPars",
    def = function(learner, ..., par.vals) {
      x = list(...)      
      if (missing(par.vals))
        par.vals = list()
      if(!isProperlyNamed(x))
        stop("All parameter settings have to be named arguments!")
      par.vals = insert(par.vals, x)
      standardGeneric("setHyperPars")
    }
)

#' @rdname setHyperPars 
setMethod(
    f = "setHyperPars",
    
    signature = signature(
        learner="Learner", 
        par.vals="list" 
    ),
    
    def = function(learner, ..., par.vals) {
      ns = names(par.vals)
      pars = learner@par.set$pars
      for (i in seq(length=length(par.vals))) {
        n = ns[i]
        p = par.vals[[i]]
        pd = pars[[n]]
        if (is.null(pd)) {
          # no description: stop warn or quiet
          msg = paste(class(learner), ": Setting par ", n, " without description!", sep="")
          if (.mlr.conf$errorhandler.setup$on.par.without.desc == "stop")
            stop(msg)
          if (.mlr.conf$errorhandler.setup$on.par.without.desc == "warn")
            warning(msg)
          learner@par.set$pars[[n]] = makeUntypedLearnerParam(id=n)
          learner@par.vals[[n]] = p
        } else {
          isf = isFeasible(pd, p)
          if (length(isf) != 1 || !isf)
            stop("'", n, "' must be a feasible parameter setting.")
          # if valname of discrete par was used, transform it to real value
          if ((pd$type == "discrete" || pd$type == "ordered") 
            && is.character(p) && length(p) == 1 && p %in% names(pd$values))
            p = pd$values[[p]]
          learner@par.vals[[n]] = p
        }
      }
      return(learner)
    } 
)


