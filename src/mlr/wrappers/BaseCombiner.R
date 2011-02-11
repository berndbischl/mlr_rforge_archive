#' @include learner.r
roxygen()
#' @include task.learn.r
roxygen()
#' @include setId.R
roxygen()
#' @include setHyperPars.R
roxygen()
#' @include setPredictType.R
roxygen()
#' @include train.learner.r
roxygen()
#' @include pred.learner.r
roxygen()
#' @include wrapped.model.r
roxygen()


#' Abstract base class to wrap something around a learner.
#' @exportClass BaseCombiner

setClass(
  "BaseCombiner",
  contains = c("learner"),
  representation = representation(
    learners = "list"
  )
)


#' Constructor.

setMethod(
  f = "initialize",
  signature = signature("BaseCombiner"),
  def = function(.Object, learners, par.set, par.vals=list(), pack=Reduce(c, sapply(learners, function(x) x@pack))) {
    if (missing(learners))
      return(make.empty(.Object))
    .Object@learners = learners
    callNextMethod(.Object, par.set=par.set, par.vals=par.vals, pack=pack)
  }
)


#' @rdname train.learner
setMethod(
  f = "train.learner",
  signature = signature(
    .learner="BaseCombiner", 
    .task="LearnTask", .subset="integer"
  ),
  
  def = function(.learner, .task, .subset,  ...) {
    lapply(.learner@learners, function(w) train(w, .task, .subset))
  }
)



##' @rdname to.string
#setMethod(f = "to.string",
#  signature = signature("BaseCombiner"),
#  def = function(x) {
#    s = ""
#    y = x 
#    while (is(y, "BaseCombiner")) {
#      s = paste(s, class(y), "->", sep="")
#      y = y@learner
#    }
#    s = paste(s, class(y))
#    
#    return(paste(
#        s, "\n",
#        "Hyperparameters: ", x["par.vals.string"], "\n\n",
#        sep = ""         
#      ))
#  })



