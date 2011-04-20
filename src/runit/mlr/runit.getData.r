test.getData = function() {
  df = getData(multiclass.task)
  checkEquals(df, multiclass.df)
  df = getData(multiclass.task, subset=1:10, vars=colnames(multiclass.df)[1:2])
  checkEquals(df, multiclass.df[1:10, 1:2])
  
  # class.as
  df = getData(binaryclass.task, class.as="01")
  checkEquals(df[, 1:20], binaryclass.df[, 1:20])
  checkTrue(is.numeric(df[, binaryclass.target]))
  checkEquals(sum(df[, binaryclass.target] == 1), 
    sum(binaryclass.df[, binaryclass.target] == binaryclass.task@desc@positive))
  checkEquals(sum(df[, binaryclass.target] == 0), 
    sum(binaryclass.df[, binaryclass.target] == binaryclass.task@desc@negative))
  df = getData(binaryclass.task, class.as="-1+1")
  checkEquals(df[,1:20], binaryclass.df[, 1:20])
  checkTrue(is.numeric(df[, binaryclass.target]))
  checkEquals(sum(df[, binaryclass.target] == 1), 
    sum(binaryclass.df[, binaryclass.target] == binaryclass.task@desc@positive))
  checkEquals(sum(df[, binaryclass.target] == -1), 
    sum(binaryclass.df[, binaryclass.target] == binaryclass.task@desc@negative))
  checkError(getData(binaryclass.task, class.as="foo"), "Argument class.as must be any of")
  
  x = getData(multiclass.task, target.extra=TRUE)
  checkEquals(x$data[,1:4], multiclass.df[,1:4])
  checkEquals(x$target, multiclass.df[, multiclass.target])
 }