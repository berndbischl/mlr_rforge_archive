#' @export
#' @rdname TuneControl
makeTuneControlIrace = function(n.instances = 100L, ...) {
  makeTuneControl(same.resampling.instance=FALSE, n.instances = n.instances, 
                  start=list(), ..., cl="TuneControlIrace")
}
