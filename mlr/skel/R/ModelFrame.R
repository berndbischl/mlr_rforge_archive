
makeModelFrame = function(data, target) {
  mf = model.frame(as.formula(paste(target, "~.-1")), data)
  attr(mf, ".Environment") = NULL
  return(mf)
}

# FIXME option for intercept
makeModelMatrix = function(data, target, mframe) {
  model.matrix(as.formula(paste(target, "~.-1")), mframe)
}

makeModelFrameDesc = function(mframe, mmatrix) {
  terms = attr(mframe, "terms")
  structure(list(
    terms = terms,
    xlevels = .getXlevels(terms, mframe),
    constrasts = attr(mmatrix, "constrasts")
 ), class="ModelMatrixDesc")
}

convertNewDataToModelMatrix = function(model.frame.desc, newdata) {
  terms = delete.response(model.frame.desc$terms)
  mf = model.frame(terms, newdata, na.action=na.pass, xlev=model.frame.desc$xlevels)
  # FIXME checkMFCClases
  return(model.matrix(terms, mf, model.frame.desc$constrasts))
}
