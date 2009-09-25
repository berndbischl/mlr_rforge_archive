\name{predict}
\alias{predict,learn.task-method}

\title{predict}
\usage{\S4method{predict}{learn.task}(object, model, newdata)}
\description{Predicts the target values of a new data set based on 
an already fitted \code{\linkS4class{wrapped.model}} of a \code{\linkS4class{learn.task}}.}
\value{Object containing the predicted targets. See inherited methods.}
\seealso{\code{\link{train}}, \code{\link{predict,classif.task-method}}, \code{\link{predict,regr.task-method}}, \code{\link{performance}}}
\alias{predict}
\arguments{\item{object}{[\code{\linkS4class{learn.task}}] \cr 
Learning task}
\item{model}{[\code{\linkS4class{wrapped.model}}] \cr 
Wrapped model, trained from learn task}
\item{newdata}{[\code{\link{data.frame}}] \cr 
Contains new observations which should be predicted (by default the train data of the wrapped model).}
}
\examples{library(MASS)
train.inds <- seq(1,150,2)
test.inds <- seq(2,150,2)

ct <- make.classif.task("lda", data=iris, formula=Species~.)
cm <- train(ct, subset=train.inds)
ps <- predict(ct, cm, newdata=iris[test.inds,])

ct <- make.classif.task("kknn.classif", data=iris, formula=Species~.)
cm <- train(ct, subset=train.inds, parset=list(k=3))
ps <- predict(ct, cm, newdata=iris[test.inds,])}
