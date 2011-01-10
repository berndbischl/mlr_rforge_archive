#' @include learnerR.r
roxygen()
#' @include wrapped.model.r
roxygen()
#' @include train.learner.r
roxygen()
#' @include pred.learner.r
roxygen()
#' @include task.regr.r
roxygen()


setClass(
    "regr.nnet", 
    contains = c("rlearner.regr")
)


setMethod(
    f = "initialize",
    signature = signature("regr.nnet"),
    def = function(.Object) {
      
      desc = c(
          missings = FALSE,
          doubles = TRUE,
          factors = TRUE,
          weights = TRUE
      )
      
      par.descs = list(
        integer.learner.parameter(name="size", default=3L, lower=0, flags=list(pass.default=TRUE)),
        integer.learner.parameter(name="maxit", default=100L, lower=1L)
      )
      
      callNextMethod(.Object, pack="nnet", desc=desc, par.descs=par.descs)
    }
)

#' @rdname train.learner

setMethod(
    f = "train.learner",
    signature = signature(
        .learner="regr.nnet", 
        .task="regr.task", .subset="integer" 
    ),
    
    def = function(.learner, .task, .subset,  ...) {
      f = .task["formula"]
      if (.task["has.weights"])
        nnet(f, data=get.data(.task, .subset), linout=T, weights=.task["weights"][.subset], ...)
      else  
        nnet(f, data=get.data(.task, .subset), linout=T, ...)
    }
)

#' @rdname pred.learner

setMethod(
    f = "pred.learner",
    signature = signature(
        .learner = "regr.nnet", 
        .model = "wrapped.model", 
        .newdata = "data.frame", 
        .type = "missing" 
    ),
    
    def = function(.learner, .model, .newdata, .type, ...) {
      predict(.model["learner.model"], newdata=.newdata, ...)[,1]
    }
) 
