\name{resample.fit}
\alias{resample.fit-methods}
\alias{resample.fit}
\title{resample.fit}
\usage{resample.fit(learner, task, resampling, parset, vars, type, extract)}
\description{Given a resampling strategy, which defines sets of training and test indices, 
\code{resample.fit} fits the selected learner using the training sets and performs predictions for the test sets. These 
predictions are returned - encapsulated in a \code{\link{resample.result}} object.
Optionally information from the fitted models, e.g. the complete model, can be extracted and returned.}
\value{An object of class \code{\linkS4class{resample.result}}.}
\arguments{\item{learner}{[\code{\linkS4class{wrapped.learner}} or \code{\link{character}}]\cr 
Learning algorithm.}
\item{task}{[\code{\linkS4class{learn.task}}] \cr
Learning task.}
\item{resampling}{[\code{\linkS4class{resample.desc}} or \code{\linkS4class{resample.instance}}] \cr
Resampling strategy.}
\item{parset}{[\code{\link{list}}]\cr 
A list of named elements which specify the hyperparameters of the learner.}
\item{vars}{[\code{\link{character}}] \cr 
Vector of variable names to use in training the model. Default is to use all variables.}
\item{type}{[\code{\link{character}}] \cr 
Only used for classification tasks; specifies the type of predictions -
either probability ("prob") or class ("class").}
\item{extract}{[\code{\link{function}}] \cr 
Function used to extract information from fitted models, e.g. can be used to save the complete list of fitted models. 
Default is to extract nothing.}
}
\examples{ct <- make.classif.task(data=iris, target="Species")
res <- make.res.instance("cv", task=ct, iters=3)
f1 <- resample.fit("rpart.classif", ct, res)	
f2 <- resample.fit("rpart.classif", ct, res, parset=list(minsplit=10, cp=0.03))}
