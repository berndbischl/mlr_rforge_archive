#' Prediction object.
#' 
#' Result from \code{\link{predict}}. 
#' Use \code{as.data.frame} to access all information in a convenient format.   
#' 
#' Object slots:
#' \describe{
#'  \item{predict.type [\code{character(1)}]}{Type set in \code{\link{setPredictType}}.}
#'  \item{id [numeric]}{Vector of index numbers of predicted cases from the task.}
#'  \item{response [numeric | factor]}{Predicted response values.}
#'  \item{truth [numeric | factor]}{True target values.}
#'  \item{prob [numeric | matrix] Optional parameters: class}{Predicted probabilities. For binary class: Only the probabilities for the positive class are returned.}
#'  \item{threshold [numeric]}{Threshold set in predict function.}
#' }
#Predicted probabilities. If it's a binary problem only the probabilities for the positive class are returned. With "class" you can specifically select which columns of the prob matrix should be returned. Columns names of the returned matrix are always the respective class labels.
makePrediction = function(task.desc, id, truth, predict.type, y, time) {
	data = list()
	# if null no col in data present
	data[["id"]] = id
	data[["truth"]] = truth
  if (predict.type == "response") {
    data[["response"]] = y
  } else if (predict.type == "prob") {
		data[["prob"]] = y
  } else if (predict.type == "se"){
    data[["response"]] = y[,1]
    data[["se"]] = y[,2]
  }
  data = as.data.frame(data)
  # fix columnnames for prob if strage chars are in factor levels
	# FIXME review this!
	i = grep("prob.", colnames(data))
	if (length(i) > 0)
		colnames(data)[i] = paste("prob.", colnames(y), sep="")
  
  p = structure(list(
    predict.type = predict.type,			
    data = data,			
    threshold = as.numeric(NA),			
    task.desc = task.desc,
    time = time			
  ), class="Prediction")
	
  if (predict.type == "prob") {
    th = rep(1/length(task.desc$class.levels), length(task.desc$class.levels))
    names(th) = task.desc$class.levels
    p = setThreshold(p, th)
  } 
  return(p)  
}

# FIXME
print.Prediction = function(x, ...) {
  catf("Prediction %s", printToChar(str(as.data.frame(x)))) 
}

