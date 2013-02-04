#' @export
#' @rdname TuneControl
makeTuneControlIrace = function(same.resampling.instance=TRUE, start, ...) {
  makeTuneControl(same.resampling.instance=same.resampling.instance, 
                  start=list(), ..., cl="TuneControlIrace")
}
