source("D:\\sync\\projekte\\mlr\\src\\mlrTune\\op.R")

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
    .Object@env$path = data.frame()
    return(.Object)
  }
)


#' @rdname addPathElement
setMethod(
  f = "addPathElement",
  signature = signature(op="OptPathDF", x="list", y="numeric", dob="integer", eol="integer"), 
  def = function(op, x, y, dob, eol) {
    names(x) = op@x.names
    names(y) = op@y.names
    el = cbind(as.data.frame(x), as.data.frame(as.list(y)))
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

#' @rdname getLength
setMethod(
  f = "getLength",
  signature = signature(op="OptPathDF"), 
  def = function(op) {
    nrow(op@env$path)
  }
)

#' @rdname paramToPosition
paramToPosition = function(op, x, cand) {
  if (!missing(cand)) {
    r <- eval(eval(substitute(substitute(cand, op@env))), parent.frame())
    idx <- which(r)
    # we do not check names / attribs
    tmp <- Position(function(zz) all.equal(x, zz, check.attributes=FALSE), op@env$path[idx])
    if (!is.na(tmp))
      idx[tmp]
    else
      tmp
  } else {
    Position(function(e) isTRUE(all.equal(x, e$x, check.attributes=FALSE)), op@env$path)
  }
}





#' Convert to data.frame
#' @rdname OptPath-class 
#' @export
as.data.frame.OptPathDF = function(x) {
  x@env$path
}


op = new("OptPathDF", x.names=as.character(1:3), y.names="y", minimize=T)
print(op)
for (i in 1:3)
   addPathElement(op, x=as.list(rep(i, 3)), y=99) 
print(op)
print(as.data.frame(op))

setEoL(op, x=as.list(rep(2, 3)), 88)
print(as.data.frame(op))

