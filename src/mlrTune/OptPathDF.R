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
  def = function(.Object, x.names, y.names, minimize) {
    .Object = callNextMethod(.Object, x.names, y.names, minimize)
    ns = c(x.names, y.names)
    df = as.data.frame(matrix(0, nrow=0, ncol=length(ns)))
    colnames(df) = ns 
    .Object@env$path = df 
    return(.Object)
  }
)


#' @rdname addPathElement
setMethod(
  f = "addPathElement",
  signature = signature(op="OptPathDF", x="list", y="numeric", dob="integer", eol="integer"), 
  def = function(op, x, y, dob, eol) {
    el = do.call(cbind, lapply(x, function(z) as.data.frame(t(z), stringsAsFactors=FALSE)))
    el = cbind(el, as.data.frame(as.list(y), stringsAsFactors=FALSE))
    colnames(el) = c(op@x.names, op@y.names)
    op@env$path = rbind(op@env$path, el)
    k = length(op@env$dob) + 1
    op@env$dob[k] = dob  
    op@env$eol[k] = eol  
    return(NULL)
  }
)

#' @rdname getLength
setMethod(
  f = "getLength",
  signature = signature(op="OptPathDF"), 
  def = function(op) {
    nrow(op@env$path)
  }
)



#' Convert to data.frame
#' @rdname OptPath-class 
#' @export
as.data.frame.OptPathDF = function(x) {
  df = x@env$path
  df = cbind(df, dob=x@env$dob, eol=x@env$eol)
  df
}



#' @rdname getLength
setMethod(
  f = "getPathElement",
  signature = signature(op="OptPathDF", index="integer"), 
  def = function(op, index) {
    e = op@env
    path = e$path
    x = as.list(path[index, op@x.names, drop=FALSE])
    y = unlist(path[index, op@y.names, drop=FALSE])
    list(x=x, y=y, dob=e$dob[index], eol=e$eol[index])
  }
)

