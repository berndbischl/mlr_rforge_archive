#' @export
#' @rdname TuneControl
makeTuneControlCMAES = function(same.resampling.instance=TRUE, start, ...) {
  checkArg(start, "numeric")
  makeTuneControl(same.resampling.instance=same.resampling.instance, 
    start=as.list(start), ..., cl="TuneControlCMAES")
}

