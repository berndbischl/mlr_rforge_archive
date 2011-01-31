#' @export 
makeSPOControl = function(seq.loops=10, propose.points=1, propose.points.method="seq.design", 
  seq.design.points=100, seq.design.fun=randomLHS, seq.design.args=list(),
  resample.desc = make.res.desc("cv", iter=10), resample.at = c(1, seq.loops), resample.measures=list(mse) 
) {
  
  list( 
    seq.loops = seq.loops, 
    propose.points = propose.points,
    propose.points.method = propose.points.method,
    seq.design.points = seq.design.points, 
    seq.design.fun = seq.design.fun, 
    seq.design.points = seq.design.points,
    resample.desc = resample.desc,
    resample.at = resample.at,
    resample.measures = resample.measures
  )
}

