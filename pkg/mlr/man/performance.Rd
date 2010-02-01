\name{performance}
\alias{performance,ANY,ANY,numeric,loss,function-method}
\alias{performance}
\title{performance}
\usage{performance(true.y, pred.y, weights, loss, aggr)}
\description{\code{performance} measures the quality of predictions w.r.t. some loss function.}
\details{There are some more loss functions which you can use as performance measure, they are as follows:
For classification:
\describe{ 
\item{\code{smce}}{Summed misclassification error}
} 

For regression:
\describe{ 
\item{\code{sae}}{Sum of absolute errors}
\item{\code{mae}}{Median of absolute errors}
\item{\code{sse}}{Squared sum of errors}
\item{\code{mse}}{Mean squared error}
}}
\value{The performance.}
\arguments{\item{true.y}{[ANY] \cr
The data sets true labels.}
\item{pred.y}{[ANY] \cr
The predicted labels.}
\item{weights}{[\code{\link{numeric}}] \cr
An optional vector of weights to be used. Default is a weight of 1 for every case.}
\item{loss}{[\code{\linkS4class{loss}} or \code{\link{character}}] \cr
Loss function. Default is "zero-one" for classifictaion and "squared" for regression.}
\item{aggr}{[\code{\link{function}}] \cr
Function to aggregate individual loss values to a single one. Default is mean.}
}
\examples{data(iris) 
ct <- make.classif.task(data=iris, target="Species")
# specify train and test set indices
train.set <- seq(from=1L, to=150L, by=2L)
test.set <- seq(from=2L, to=150L, by=2L)
# train a model with the train set
m <- train("rpart.classif", ct, subset=train.set) 
# predict the class of the test set measures 
preds <- predict(m, newdata=iris[test.set,])
# evaluate the predictions, first with mean misclassification error as loss function (default)
performance(true.y = iris[test.set, "Species"], pred.y = preds)}
