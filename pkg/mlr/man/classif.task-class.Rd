\name{classif.task-class}
\alias{classif.task-class}
\alias{classif.task}
\title{classif.task}
\description{General description object for a classification experiment.   
Instantiate it by using its factory method.}
\seealso{make.regr.task}
\section{Slots}{\describe{\item{\code{type}:}{(\code{\link{character}}) "class" if you generally want to predict classes or "prob" for probabilities. Default is "class"}\item{\code{costs}:}{(\code{\link{matrix}}) Matrix of misclassification costs. Default is zero-one loss.}}}
\section{Extends}{\code{\linkS4class{learn.task}}}
\section{Methods}{\describe{\item{\code{\link[=default.loss,classif.task-method]{default.loss}}}{}\item{\code{\link[=initialize,classif.task-method]{initialize}}}{\code{signature( = "classif.task")}: Constructor.}\item{\code{\link[=[,classif.task-method]{[}}}{\code{signature( = "classif.task")}: Getter.}
