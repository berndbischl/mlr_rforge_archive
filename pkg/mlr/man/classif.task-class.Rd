\name{classif.task-class}
\alias{classif.task-class}
\alias{classif.task}
\title{Classification task.}
\description{General description object for a classification experiment.   
Instantiate it by using its factory method.}
\details{Getter.\cr

\describe{
\item{class.levels [character]}{Levels of target factor for classification.}
\item{class.nr [integer]}{Number of class labels for classification.}
\item{is.binary [boolean]}{Binary classification task?}
\item{costs [matrix]}{Cost matrix, NULL if not available.}
\item{positive [string]}{Positive class label for binary classification.}
\item{negative [string]}{Negative class label for binary classification.}
}}
\seealso{\code{\link{make.task}}}
\section{Extends}{\code{\linkS4class{learn.task}}}
\section{Methods}{\describe{\item{\code{\link[=to.string,classif.task-method]{to.string}}}{}}}
