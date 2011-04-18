test.check.data <- function() {
	checkError(makeClassifTask(data=binaryclass.df, target= "foo"), "don't contain target var: foo")
  
  # y contains missings
  df = multiclass.df
  df[1, multiclass.target] = NA
  checkError(makeClassifTask(data=df, target=multiclass.target), "Target contains missing values")
  
  # data contains infs
  df = regr.df
  df[1, regr.target] = Inf
  checkError(makeRegrTask(data=df, target=regr.target), "Data contains infinite")
  
  # check conversion of target
  df = multiclass.df
  df[, multiclass.target] = as.character(df[, multiclass.target]) 
  checkWarning(makeClassifTask(data=df, target=multiclass.target), "Converting target to factor") 
  df = regr.df
  df[, regr.target] = as.integer(regr.df[, regr.target]) 
  checkWarning(makeRegrTask(data=df, target=regr.target), "Converting target to numeric") 
  
  # check unsupported columns
  df = multiclass.df
  df[, 1] = as.logical(df[,1])
  colnames(df)[1] = "aaa"
  checkError(makeClassifTask(data=df, target=multiclass.target), "Unsupported feature type in: aaa, logical")
}