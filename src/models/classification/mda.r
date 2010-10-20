#' @include learnerR.r
roxygen()
#' @include wrapped.model.r
roxygen()
#' @include train.learner.r
roxygen()
#' @include pred.learner.r
roxygen()


setClass(
  "classif.mda", 
  contains = c("rlearner.classif")
)


setMethod(
  f = "initialize",
  signature = signature("classif.mda"),
  def = function(.Object) {
    
    desc = new("learner.desc.classif",
      oneclass = FALSE,
      twoclass = TRUE,
      multiclass = TRUE,
      missings = FALSE,
      numerics = TRUE,
      factors = TRUE,
      characters = FALSE,
      decision = FALSE,
      probs = TRUE,
      weights = FALSE,
      costs = FALSE
    )
 
    x = callNextMethod(.Object, pack="mda", desc=desc)
    
    par.descs <- list(
      new("par.desc.unknown", par.name="subclasses", default=2L),
      new("par.desc.num", par.name="iter", default=5L, lower=1L),
      new("par.desc.num", par.name="dimension", lower=1L),
      new("par.desc.disc", par.name="method", default="polyreg", 
        vals=list(polyreg=polyreg, mars=mars, bruto=bruto, gen.ridge=gen.ridge)),
      new("par.desc.log", par.name="trace", default=FALSE, flags=list(optimize=FALSE)),
      # change default and pass it to reduce mem
      new("par.desc.log", par.name="keep.fitted", default=FALSE, flags=list(optimize=FALSE, pass.default=TRUE)),
      new("par.desc.num", par.name="tries", default=5L, lower=1L)
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
    .targetvar="character", 
    .data="data.frame", 
    .data.desc="data.desc", 
    .task.desc="task.desc", 
    .weights="numeric", 
    .costs="matrix" 
  ),
  
  def = function(.learner, .targetvar, .data, .data.desc, .task.desc, .weights, .costs,  ...) {
    f = as.formula(paste(.targetvar, "~."))
    mda(f, data=.data, ...)
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




