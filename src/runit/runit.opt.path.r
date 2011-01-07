test.opt.path <- function() {
  
  op = new(opt.path)
  add.path.el(op, list(x1=1, x2="a", y1=1, y2=3))
  add.path.el(op, list(x1=2, x2="a", y1=1, y2=3))
  
  x = as.list(op)
  checkTrue(is.list(x))
  checkEqual(length(x), 2)
  checkTrue(is.list(x[[1]]))
  checkTrue(is.list(x[[2]]))
  checkEqual(length(x[[1]]), 4)
  checkEqual(length(x[[2]]), 4)

  x = as.data.frame(op)
  checkTrue(is.data.frame(x))
  checkEqual(nrow(x), 2)
  checkEqual(ncol(x), 4)

  op2 = subset(op, subset=c(2,2,1), select=c("x2", "y2"))
  checkEqual(nrow(x), 2)
  checkEqual(ncol(x), 4)
  
  
}  