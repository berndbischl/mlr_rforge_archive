\name{performance}
\alias{performance,ANY,ANY,numeric,list,ANY-method}
\alias{performance}
\title{performance}
\usage{performance(true.y, pred.y, weights, measure, costs)}
\description{\code{performance} measures the quality of predictions w.r.t. some loss function.}
\details{There are some more loss functions which you can use as performance measure, they are as follows:
For classification:
\itemize{ 
\item{\code{smce}}{Summed misclassification error}
} 

For regression:
\itemize{ 
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
\item{measure}{[\code{\link{list}}/ \code{\link{character}}] \cr
A list or a character which defines the loss function. Default is the mean misclassification error 
("mmce") for classification tasks and the mean squared error ("mse") for regression tasks. See details
for other loss functions.}
\item{costs}{[ANY] \cr
An optional possibility to specify costs, default is \code{NULL}.}
}
\examples{data(iris) 
# define a classification task for a decision tree (rpart) for the data set iris
ct <- make.classif.task("rpart.classif", data=iris, formula=Species~.)
# specify train and test set indices
train.set <- seq(from=1L, to=150L, by=2L)
test.set <- seq(from=2L, to=150L, by=2L)
# train a model with the train set
m <- train(ct, subset=train.set) 
# predict the class of the test set measures 
preds <- predict(m, newdata=iris[test.set,])
# evaluate the predictions, first with mean misclassification error as loss function (default)
performance(true.y = iris[test.set, "Species"], pred.y = preds)		
# and with summed misclassification error
performance(true.y = iris[test.set, "Species"], pred.y = preds, measure="smce")}

