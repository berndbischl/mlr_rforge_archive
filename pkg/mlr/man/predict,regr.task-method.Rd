\name{predict,regr.task-method}
\alias{predict,regr.task-method}

\title{predict}
\usage{\S4method{predict}{regr.task}(object, model, newdata)}
\description{Predicts the target values of a new data set based on 
an already fitted wrapped.model of a regression task.}
\details{See documentation super method.}
\value{A vector of numeric values.}
\seealso{\code{\link{predict}}}
\arguments{\item{object}{[\code{\linkS4class{learn.task}}] \cr 
Learning task}
\item{model}{[\code{\linkS4class{wrapped.model}}] \cr 
Wrapped model, trained from learn task}
\item{newdata}{[\code{\link{data.frame}}] \cr 
Contains new observations which should be predicted (by default the train data of the wrapped model).}
}
\examples{data(BostonHousing)
inds <- seq(1, nrow(BostonHousing), 2)
test <- BostonHousing[-inds,]

rt <- make.regr.task("stats.lm", data=BostonHousing, formula=medv~.)
model <- train(rt, subset=inds)
predict(rt, model, newdata = test)}
