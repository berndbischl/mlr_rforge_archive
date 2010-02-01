\name{train}
\alias{train-methods}
\alias{train}
\title{train}
\usage{train(learner, task, subset, parset, vars)}
\description{Given a \code{\linkS4class{learn.task}} \code{train} creates a model for the learning machine 
which can be used for predictions on new data.}
\value{An object of class \code{\linkS4class{wrapped.model}} containing the generated model of the underlying learner and the paramater and index set used for training.}
\seealso{\code{\link{predict}}, \code{\link{make.classif.task}}, \code{\link{make.regr.task}}}
\arguments{\item{learner}{[\code{\linkS4class{wrapped.learner}} or \code{\link{character}}]\cr 
Learning algorithm.}
\item{task}{[\code{\linkS4class{learn.task}}]\cr 
Specifies learning task.}
\item{subset}{[\code{\link{integer}}] \cr 
An index vector specifying the training cases from the data contained in the learning task. By default the complete dataset is used.}
\item{parset}{[\code{\link{list}}] \cr
Named list which contains the hyperparameters of the learner. Default is an empty list, which means no hyperparameters are specifically set and defaults of the underlying learner are used.}
\item{vars}{[\code{\link{character}}] \cr
Vector of variable names to use in training the model. Default is to use all variables.}
}
\examples{library(MASS)
train.inds <- seq(1,150,2)
test.inds <- seq(2,150,2)

ct <- make.classif.task(data=iris, target="Species")
cm <- train("lda", ct, subset=train.inds)
ps <- predict(cm, newdata=iris[test.inds,])

ct <- make.classif.task(data=iris, target="Species")
cm <- train("kknn.classif", ct, subset=train.inds, parset=list(k=3))
ps <- predict(cm, newdata=iris[test.inds,])}
