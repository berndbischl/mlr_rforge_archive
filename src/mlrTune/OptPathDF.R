#' @exportClass OptPath
#' @title Optimazation path
setClass(
  "OptPathDF",
  contains = c("OptPath")
)

#' Constructor.
setMethod(
  f = "initialize",
  signature = signature("OptPathDF"),
  def = function(.Object, par.set, y.names, minimize) {
    .Object = callNextMethod(.Object, par.set, y.names, minimize)
    ns = c(.Object@x.names, y.names)
    df = as.data.frame(matrix(0, nrow=0, ncol=length(ns)))
    colnames(df) = ns 
    .Object@env$path = df 
    return(.Object)
  }
)


#' @rdname addPathElement
setMethod(
  f = "addPathElement",
  signature = signature(op="OptPathDF", x="list", x.trafo="list", y="numeric", dob="integer", eol="integer"), 
  def = function(op, x, x.trafo, y, dob, eol) {
    el = do.call(cbind, lapply(x.trafo, function(z) as.data.frame(t(z), stringsAsFactors=FALSE)))
    el = cbind(el, as.data.frame(as.list(y), stringsAsFactors=FALSE))
    colnames(el) = c(op@x.names, op@y.names)
    op@env$path = rbind(op@env$path, el)
    k = length(op@env$dob) + 1
    op@env$dob[k] = dob  
    op@env$eol[k] = eol  
    return(NULL)
  }
)

#' @rdname ength
setMethod(
  f = "length",
  signature = signature(op="OptPathDF"), 
  def = function(op) {
    nrow(op@env$path)
  }
)



#' Convert to data.frame
#' @export
as.data.frame.OptPathDF = function(x) {
  df = x@env$path
  df = cbind(df, dob=x@env$dob, eol=x@env$eol)
  df
}


#' @rdname getPathElement
setMethod(
  f = "getPathElement",
  signature = signature(op="OptPathDF", index="integer"), 
  def = function(op, index) {
    e = op@env
    path = e$path
    x = dataFrameRowToList(path, op@par.set, index)
    y = unlist(path[index, op@y.names, drop=FALSE])
    list(x=x, y=y, dob=e$dob[index], eol=e$eol[index])
  }
)

