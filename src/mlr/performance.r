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
    td = NULL
    if (m@req.pred) {
      if (is.empty(pred))
        stop("You need to pass pred for measure ", m@id)
      pred2 = pred
      td = pred@task.desc
    } else {
      pred2 = NULL          
    }
    if (m@req.model) {
      if (is.empty(model))
        stop("You need to pass model for measure ", m@id)
      model2 = model  
      td = model@task.desc
    } else {
      model2 = NULL
    }
    if (m@req.task) {
      if (is.empty(task))
        stop("You need to pass task for measure ", m@id)
      task2 = task 
      td = task@desc
    } else {
      task2 = NULL
    }
    # null only happens in custom resampled measure when we do no individual measurements
    if (!is.null(td)) {
      if ((td@type == "classif" && !m@classif) || (td@type == "regr" && !m@regr)) 
        stop("Wrong task type ", td@type, " for measure ", m@id, "!")
      if (m@only.binary && length(td@class.levels) > 2)
        stop("Multiclass problems cannot be used for measure ", m@id, "!")
      if (!is.null(pred2) && !(pred2@predict.type %in% m@allowed.pred.types))
        stop("Measure ", m@id, " is only allowed for predictions of type: ", paste(m@allowed.pred.types, collapse=","))
    }
    measure@fun(task2, model2, pred2, m@extra.args)
  }
)





