#' @include prediction.r
roxygen()
#' @include measure.r
roxygen()

#' Measures the quality of predictions w.r.t. some performance measures.
#' 
#' @param pred [\code{\linkS4class{prediction}}] \cr
#' 		  Prediction object to evaluate.
#' @param measures [see \code{\link{measures}}]
#'        Performance measures. 
#' @param aggr [see \code{\link{aggregations}}]
#'        Aggregation functions. 
#' 		  Ignored if not a \code{\linkS4class{resample.prediction}}
#' @param task [\code{\linkS4class{learn.task}}]\cr 
#'        Optionally specifies learning task, very rarely needed.
#' 
#' @return A list with with possibly three named components: "measures" is a data.frame of performance values,
#' 		   "aggr" a data.frame of aggregated values 
#' 
#' @exportMethod performance
#' @rdname performance
#'
#' @title Measure performance.



setGeneric(
		name = "performance",
		def = function(pred, measure, task, model, pred.train) {
      if (missing(pred))
        pred = new("prediction")
      if (missing(pred.train))
        pred.train = new("prediction")
      if (missing(task))
        task = new("learn.task")
      if (missing(model))
        model = new("wrapped.model")
      standardGeneric("performance")
		}
)

#' @rdname performance

setMethod(
  f = "performance",
  signature = signature(pred="prediction", measure="measure", task="learn.task", model="wrapped.model", pred.train="prediction"),
  def = function(pred, measure, task, model, pred.train) {
    m = measure
    if (m["req.pred.test"]) {
      if (is.empty(pred))
        stop("You need to pass pred for measure ", m["id"])
      pred2 = pred
      td = pred@task.desc
      dd = pred@data.desc                    
    } else {
      pred2 = NULL          
    }
    if (m["req.pred.train"]) {
      if (is.empty(pred.train))
        stop("You need to pass pred.train for measure ", m["id"])
      pred.train2 = pred.train          
      td = pred.train@task.desc
      dd = pred.train@data.desc
    } else {
      pred.train2 = NULL          
    }
    if (m["req.model"]) {
      if (is.empty(model))
        stop("You need to pass model for measure ", m["id"])
      model2 = model  
      td = model@task.desc
      dd = model@data.desc
    } else {
      model2 = NULL
    }
    if (m["req.task"]) {
      if (is.empty(task))
        stop("You need to pass task for measure ", m["id"])
      task2 = task 
      td = task@task.desc
      dd = task@data.desc
    } else {
      task2 = NULL
    }
    rqt = m["req.task.type"]
    if ((td["is.classif"] && identical(rqt, "regr")) || (td["is.regr"] && !("regr" %in% rqt))) 
      stop("Wrong task type ", td@task.class, " for measure ", m["id"], "!")
    if (m["req.task.type"] == "binary" && !dd["is.binary"])
      stop("Multiclass problems cannot be used for measure ", m["id"], "!")
    if (identical(m["req.pred.type"], "prob")) {
      if (!is.null(pred2) && pred2["type"] != "prob")
        stop("Probabilities in prediction objects are required by measure ", m["id"], "!")
      if (!is.null(pred.train2) && pred.train2["type"] != "prob")
        stop("Probabilities in prediction objects are required by measure ", m["id"], "!")
    }
    measure@fun(task2, model2, pred2, red.train2, m@pars)
  }
)






##' @rdname performance
#
#setMethod(
#  f = "performance",
#  signature = signature(pred="prediction", measure="measure", aggr="missing"),
#  def = function(pred, pred.train, measure, task, extract) {
#    x = pred
#    td = x@task.desc
#    dd = x@data.desc		
#    has.prob = FALSE
#    if (measure["req.pred.test"]) {
#      if (is.empty(pred))
#        stop("You need to pass pred for measure ", measure["id"])
#      pred2 = pred
#      td = pred@task.desc
#      dd = pred@data.desc                    
#    } else {
#      pred2 = NULL          
#    }
#    if (measure["req.pred.train"]) {
#      if (missing(pred.train))
#        stop("You need to pass pred.train for measure ", measure["id"])
#      pred.train2 = pred.train          
#      td = pred.train@task.desc
#      dd = pred.train@data.desc
#    } else {
#      pred.train2 = NULL          
#    }
#    if (measure["req.model.extract"]) {
#      if (missing(model))
#        stop("You need to pass model for measure ", measure["id"])
#      model2 = model  
#      td = model@task.desc
#      dd = model@data.desc
#    } else {
#      model2 = NULL
#    }
#    if (measure["req.task"]) {
#      if (missing(task))
#        stop("You need to pass task for measure ", measure["id"])
#      task2 = task 
#    } else {
#      task2 = NULL
#    }
#    tt = if (td["is.classif"]) "classif" else "regr" 
#    if (!(tt %in% measure["req.task.type"]))
#      stop("Wrong task type ", tt, " for measure ", measure["id"], "!")
#    if (measure["req.binary"] && !dd["is.binary"])
#      stop("Multiclass problems cannot be used for measure ", measure["id"], "!")
#    if (identical(measure["req.pred.type"], "prob")) {
#      if (!is.null(pred2) && pred2["type"] != "prob")
#        stop("Probabilities in prediction objects are required by measure ", measure["id"], "!")
#      if (!is.null(pred.train2) && pred.train2["type"] != "prob")
#        stop("Probabilities in prediction objects are required by measure ", measure["id"], "!")
#    }
#    measure@fun(pred.test=pred2, pred.train=pred.train2, model=model2, task=task2, pars=m@pars)
#  }
#)



