# TODO: Function description. Description of params x1,x2, trafo, errs, size
#' 
#' 
#' @param data [\code{data.frame}]\cr 
#'   Data. 
#' @param  x1
#' @param  x2
#' @param target [\code{character(1)}]\cr 
#'   Target column.
#' @param trafo.x [\code{function}]\cr
#'    Transformation function for x1. Default is identity.
#' @param trafo.y
#'    Transformation function for x2. Default is identity.
#' @param text [\code{character(1)}]\cr
#'   Label for the plot. Default is NULL.    
#' @param errs
#'   Default is NULL  
#' @param size [\code{numeric(1)}]\cr 
#'   Default is 2.
#' @return A ggplot2 object. Print it to plot it.
#' 
#' @export 



plot2d = function(data, x1, x2, target, trafo.x=identity, trafo.y=identity, text=NULL, errs=NULL, size=2) {
  v1 = data[,x1]
  v2 = data[,x2]
  v1 = trafo.x(v1)
  v2 = trafo.y(v2)
  data = data.frame(v1=v1, v2=v2, y=data[, target])
  data$errs = errs
  plt <- ggplot(data, aes(x=v1, y=v2, colour=y, label=label))
  if (is.null(text))
    plt <- plt + geom_point(size=size)
  else {
    data$label = as.factor(text)
    plt <- plt + geom_text(size=size, position="jitter")
  }
  if (!is.null(errs))
    plt <- plt + geom_point(data=subset(data, errs), size=I(10), alpha=I(0.2), colour="black")
  plt <- plt + scale_x_continuous(name=x1)
  plt <- plt + scale_y_continuous(name=x2)
  plt
}