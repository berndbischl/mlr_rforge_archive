#' @include learnerR.r
roxygen()
#' @include wrapped.model.r
roxygen()
#' @include train.learner.r
roxygen()
#' @include pred.learner.r
roxygen()
#' @include task.classif.r
roxygen()


setClass(
  "classif.mda", 
  contains = c("rlearner.classif")
)


setMethod(
  f = "initialize",
  signature = signature("classif.mda"),
  def = function(.Object) {
    
    desc = c(
      oneclass = FALSE,
      twoclass = TRUE,
      multiclass = TRUE,
      missings = FALSE,
      doubles = TRUE,
      factors = TRUE,
      decision = FALSE,
      prob = TRUE,
      weights = FALSE,
      costs = FALSE
    )
 
    x = callNextMethod(.Object, pack="mda", desc=desc)
    
    par.descs <- list(
      new("par.desc.unknown", par.name="subclasses", default=2L),
      new("par.desc.double", par.name="iter", default=5L, lower=1L),
      new("par.desc.double", par.name="dimension", lower=1L),
      new("par.desc.disc", par.name="method", default="polyreg", 
        vals=list(polyreg=polyreg, mars=mars, bruto=bruto, gen.ridge=gen.ridge)),
      new("par.desc.log", par.name="trace", default=FALSE, flags=list(optimize=FALSE)),
      # change default and pass it to reduce mem
      new("par.desc.log", par.name="keep.fitted", default=FALSE, flags=list(optimize=FALSE, pass.default=TRUE)),
      new("par.desc.double", par.name="tries", default=5L, lower=1L)
    )

    x@par.descs = par.descs
    return(x)
  }
)

#' @rdname train.learner


setMethod(
  f = "train.learner",
  signature = signature(
    .learner="classif.mda", 
    .task="classif.task", .subset="integer" 
  ),
  
  def = function(.learner, .task, .subset,  ...) {
    f = .task["formula"]
    mda(f, data=get.data(.task, .subset), ...)
  }
)

#' @rdname pred.learner

setMethod(
  f = "pred.learner",
  signature = signature(
    .learner = "classif.mda", 
    .model = "wrapped.model", 
    .newdata = "data.frame", 
    .type = "character" 
  ),
  
  def = function(.learner, .model, .newdata, .type, ...) {
    .type <- ifelse(.type=="response", "class", "posterior")
    predict(.model["learner.model"], newdata=.newdata, type=.type, ...)
  }
)	




