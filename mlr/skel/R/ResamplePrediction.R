#FIXME where does time exactly come from? only test preds?

#' Prediction from resampling.
#' 
#' Contains predictions from resampling, returned (among other stuff) by function \code{\link{resample}}.
#' Can basically be used as its super class.
#' The main differences are:
#' (a) The internal data.frame (slot \code{df}) contains an additional column \code{id}, specifying the iteration
#' of the resampling strategy. (b) The object can be converted into a list of  \code{\link{Prediction}} objects by using \code{as.list} on it,
#' one object for each resampling iteration.
#' @slot instance Resampling instance that was used to generate the prediction. 
#' @export
#' 
#' @seealso \code{\link{resample}}, \code{\link{predict}} 
NULL

makeResamplePrediction = function(instance, preds.test, preds.train) {
  data = data.frame()
  for (i in 1:instance$desc$iters) {
    data = rbind(data, cbind(preds.test[[i]]$data, iter=i, set="test"))
    if (!is.null(preds.train[[i]]))
      data = rbind(data, cbind(preds.train[[i]]$data, iter=i, set="train"))                 
  }
  p1 = preds.test[[1]]
  time = 
  structure(list( 
    instance = instance,
    predict.type = p1$predict.type,			
    data = data,
    threshold = p1$threshold,
    task.desc = p1$task.desc,	
    time = extractSubList(preds.test, "time")
  ), class=c("ResamplePrediction", "Prediction"))
}


print.ResamplePrediction = function(x, ...) {
  cat("Resampled Prediction for:\n")
  print(x$instance$desc)
}


