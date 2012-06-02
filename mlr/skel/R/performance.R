#' Measure performance of prediction.
#'
#' Measures the quality of a prediction w.r.t. some performance measure.
#' 
#' @param pred [\code{\link{Prediction}}] \cr
#'   Prediction object to evaluate.
#' @param measure [\code{\link{Measure}}]
#'   Performance measure. 
#' @param task [\code{\link{SupervisedTask}}]\cr 
#'   Learning task, might be requested by performance measure, usually not needed.
#' @param model [\code{\link{WrappedModel}}]\cr 
#'   Model built on training data, might be requested by performance measure, usually not needed.
#' @return A single numerical performance value. 
#' @export
performance = function(pred, measure, task, model) {
  m = measure
  td = NULL
  if (m$req.pred) {
    if (missing(pred))
      stopf("You need to pass pred for measure %s!", m$id)
    pred2 = pred
    td = pred$task.desc
  } else {
    pred2 = NULL          
  }
  if (m$req.model) {
    if (missing(model))
      stopf("You need to pass model for measure %s!", m$id)
    model2 = model  
    td = model$task.desc
  } else {
    model2 = NULL
  }
  if (m$req.task) {
    if (missing(task))
      stopf("You need to pass task for measure %s!", m$id)
    task2 = task 
    td = task$desc
  } else {
    task2 = NULL
  }
  # null only happens in custom resampled measure when we do no individual measurements
  if (!is.null(td)) {
    if ((td$type == "classif" && !m$classif) || (td$type == "regr" && !m$regr)) 
      stopf("Wrong task type %s for measure %s!", td$type, m$id)
    if (m$only.binary && length(td$class.levels) > 2)
      stopf("Multiclass problems cannot be used for measure %s!", m$id)
    if (!is.null(pred2) && !(pred2$predict.type %in% m$allowed.pred.types))
      stopf("Measure %s is only allowed for predictions of type: %s!", 
        m$id, collapse(m$allowed.pred.types))
  }
  measure$fun(task2, model2, pred2, m$extra.args)
}






