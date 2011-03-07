#' @include Prediction.R
roxygen()
#' @include Measure.R
roxygen()

#' Measures the quality of a prediction w.r.t. some performance measures.
#' 
#' @param pred [\code{\linkS4class{Prediction}}] \cr
#'   Prediction object to evaluate.
#' @param measure [\code{\linkS4class{Measure}}]
#'   Performance measure. 
#' @param task [\code{\linkS4class{LearnTask}}]\cr 
#'   Learning task, might be requested by performance measure, usually not needed.
#' @param model [\code{\linkS4class{WrappedModel}}]\cr 
#'   Model built on training data, might be requested by performance measure, usually not needed.
#' 
#' @return A single numerical performance value. 
#' 
#' @exportMethod performance
#' @rdname performance
#'
#' @title Measure performance.

setGeneric(
		name = "performance",
		def = function(pred, measure, task, model) {
      if (missing(pred))
        pred = new("Prediction")
      if (missing(task))
        task = new("LearnTask")
      if (missing(model))
        model = new("WrappedModel")
      standardGeneric("performance")
		}
)

#' @rdname performance

setMethod(
  f = "performance",
  signature = signature(pred="Prediction", measure="Measure", task="LearnTask", model="WrappedModel"),
  def = function(pred, measure, task, model) {
    m = measure
    if (m["req.pred"]) {
      if (is.empty(pred))
        stop("You need to pass pred for measure ", m@id)
      pred2 = pred
      td = pred@desc
    } else {
      pred2 = NULL          
    }
    if (m["req.model"]) {
      if (is.empty(model))
        stop("You need to pass model for measure ", m@id)
      model2 = model  
      td = model@desc
    } else {
      model2 = NULL
    }
    if (m["req.task"]) {
      if (is.empty(task))
        stop("You need to pass task for measure ", m@id)
      task2 = task 
      td = task@desc
    } else {
      task2 = NULL
    }
    rqt = m["req.task.type"]
    if ((td@type == "classif" && identical(rqt, "regr")) || (td@type == "regr" && !("regr" %in% rqt))) 
      stop("Wrong task type ", td@type, " for measure ", m@id, "!")
    if (m["req.task.type"] == "binary" && length(getClassLevels(x)) > 2)
      stop("Multiclass problems cannot be used for measure ", m@id, "!")
    if (identical(m["req.pred.type"], "prob")) {
      if (!is.null(pred2) && pred2["type"] != "prob")
        stop("Probabilities in prediction objects are required by measure ", m@id, "!")
    }
    measure@fun(task2, model2, pred2, m@extra.pars)
  }
)





