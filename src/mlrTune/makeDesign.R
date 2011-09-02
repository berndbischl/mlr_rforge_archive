#todo: links to lhs functions
#todo: describe and check vector paramters
#' Draws a sample from a set of parameters using a design function from the package lhs.
#'
#' @param n [integer(1)]\cr 
#'   Number of samples in design.   
#' @param par.set [\code{\linkS4class{ParameterSet}}]] \cr
#'   Set of parameters.
#' @param fun [function] \cr
#'   Function from package lhs. Possible are: maximinLHS, randomLHS, geneticLHS, improvedLHS, , optAugmentLHS, optimumLHS 
#' @param fun.args [list] \cr
#'   List of further arguments passed to \code{fun}. 
#' @param trafo [logical(1)] \cr
#'   Transform all parameters by using theirs respective transformation functions. Default is \code{FALSE}. 
#' @param ints.as.num [logical(1)] \cr
#'   Should parameters of type 'integer', 'integervector' have columns of type 'numeric' in result?
#'   Default is \code{FALSE}.  
#' @return The created design as a data.frame. Columns are named by the ids of the parameters.
#'   If the \code{par.set} argument contains a vector parameter, its corresponding columns names  
#'   in the design are the parameter id concatenated with 1 to dimension of vector.   
#'   The data type of a column 
#'   is defined in the following way. Numeric parameters generate numeric columns, integer parameters generate integer columns, 
#'   logical parameters generate logical columns, discrete parameters character columns.
#'   The result will have an \code{logical(1)} attribute 'trafo', 
#'   which is set to the value of argument \code{trafo}.    
#' @export 
makeDesign = function(n, par.set, fun=randomLHS, fun.args=list(), trafo=FALSE, ints.as.num=FALSE) {
  require.packs("lhs", "makeDesign")
  if(any(sapply(par.set@pars, function(x) is(x, "LearnerParameter"))))
    stop("No par.set parameter in 'makeDesign' can be of class 'LearnerParameter'! Use basic parameters instead to describe you region of interest!")        
  
  lower = lower(par.set)
  upper = upper(par.set)
  
  if (any(is.infinite(c(lower, upper))))
    stop("makeDesign requires finite box constraints!")
  
  vals = values(par.set)
  pars = par.set@pars
  
  k = sum(sapply(par.set@pars, function(x) 
        if (x@type %in% c("numericvector", "integervector")) length(lower(x)) else 1))
  des = do.call(fun, c(list(n=n, k=k), fun.args))
  des = as.data.frame(des)

  col = 0
  for (i in 1:length(pars)) {
    p = pars[[i]]
    cc = rev(col)[1]
    if (p@type %in% c("numericvector", "integervector")) 
      col = (cc + 1) : (cc + length(lower(p)))   
    else 
      col = cc + 1    
    trafo.fun = if (trafo) p@trafo else identity
    if (p@type == "numeric")
      des[,col] = trafo.fun((upper(p)-lower(p))*des[,col] + lower(p))
    else if (p@type == "integer") {
      x = trafo.fun(as.integer(floor((upper(p)-lower(p)+1)*des[,col] + lower(p))))
      des[,col] = if (ints.as.num) as.numeric(x) else x  
    } else if (p@type == "numericvector") {
      des[,col] = t((upper(p)-lower(p))*t(des[,col]) + lower(p))
      des[,col] = apply(des[,col], 1, trafo.fun)
    } else if (p@type == "integervector") {
      x = floor((upper(p)-lower(p)+1)*as.matrix(des[,col]) + lower(p))
      if (!ints.as.num)
        mode(x) = "integer"
      des[,col] = x
      des[,col] = apply(des[,col], 1, trafo.fun)
    } else if (p@type == "logical")
      des[,col] = ifelse(des[,col] <= 0.5, FALSE, TRUE)
    else if (p@type == "discrete") {
      v = values(p)
      des[,col] = as.character(factor(names(v[ceiling(des[,col] * length(v))]), levels=v))
    }
  }
  colnames(des) = getRepeatedParameterIDs(par.set, with.nr=TRUE)
  attr(des, "trafo") = trafo
  return(des)
}
