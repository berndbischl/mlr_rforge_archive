#' @include learnerR.r
roxygen()
#' @include wrapped.model.r
roxygen()
#' @include train.learner.r
roxygen()
#' @include pred.learner.r
roxygen()

setClass(
  "regr.fnn", 
  contains = c("rlearner.regr")
)

setMethod(
  f = "initialize",
  signature = signature("regr.fnn"),
  def = function(.Object) {
    
    desc <- new("learner.desc.regr",
      missings = FALSE,
      numerics = TRUE,
      factors = FALSE,
      weights = FALSE
    )
    
    # l is for reject option. cannot be done with mlr atm
    par.descs = list(
      new("par.desc.num", par.name="k", default=1L, lower=1L),
      new("par.desc.log", par.name="use.all", default=TRUE, requires=expression(algorithm == "VR")),
      new("par.desc.disc", par.name="algorithm", default="cover_tree", vals=list("cover_tree", "kd_tree", "VR"))
    )
    callNextMethod(.Object, pack="FNN", desc=desc, par.descs=par.descs)
  }
)

#' @rdname train.learner

setMethod(
  f = "train.learner",
  signature = signature(
    .learner="regr.fnn", 
    .task="regr.task", .subset="integer", .vars="character" 
  ),
  
  def = function(.learner, .task, .subset, .vars, ...) {
    i = which(colnames(.data)==.targetvar)
    y = .data[,i]
    train = .data[,-i]
    list(train=train, y=y, parset=list(...))
  }
)

#' @rdname pred.learner

setMethod(
  f = "pred.learner",
  signature = signature(
    .learner = "regr.fnn", 
    .model = "wrapped.model", 
    .newdata = "data.frame",
    .type="missing" 
  ),
  
  def = function(.learner, .model, .newdata, ...) {
    m = .model["learner.model"]
    pars = list(train=m$train, test=.newdata, y=m$y)  
    pars = c(pars, m$parset, list(...))
    do.call(FNN::knn.reg, pars)$pred
  }
)	




