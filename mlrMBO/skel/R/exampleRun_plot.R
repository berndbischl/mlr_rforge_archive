plot.MBOExampleRun = function(obj, ...,  
  pch=19, col.initdes="black", col.seqdes="darkseagreen", col.propdes="tomato") {
  
  if (obj$n.params == 1L && obj$par.types %in% c("numeric", "numericvector"))
    plotMBOExampleRun1DNumeric(obj, ...,
      pch=pch, col.initdes=col.initdes, col.seqdes=col.seqdes, col.propdes=col.propdes)
  else if (obj$n.params == 2L && all(obj$par.types %in% c("numeric", "numericvector")))
    plotMBOExampleRun2DNumeric(obj, ...,
      pch=pch, col.initdes=col.initdes, col.seqdes=col.seqdes, col.propdes=col.propdes)
  else
    stopf("Should not happen!")
}

