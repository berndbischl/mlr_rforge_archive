plot2d = function(data, x1, x2, target, trafo.x=identity, trafo.y=identity, text=NULL, errs=NULL, size=2) {
  v1 = data[,x1]
  v2 = data[,x2]
  v1 = trafo.x(v1)
  v2 = trafo.y(v2)
  data = data.frame(v1=v1, v2=v2, y=data[, target])
  data$errs = errs
  data$label = as.factor(text)
  plt <- ggplot(, aes(x=v1, y=v2, colour=y, label=label))
  if (is.null(text))
    plt <- plt + geom_point(size=size)
  else
    plt <- plt + geom_text(size=size, position="jitter")
  if (!is.null(errs))
    plt <- plt + geom_point(data=subset(data, errs), size=I(10), alpha=I(0.2), colour="black")
  plt <- plt + scale_x_continuous(name=x1)
  plt <- plt + scale_y_continuous(name=x2)
  plt
}