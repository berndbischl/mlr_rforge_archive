\name{train.learner,penalized.lasso,formula,data.frame,numeric,list-method}
\alias{train.learner,penalized.lasso,formula,data.frame,numeric,list-method}

\title{Overwritten, to allow "lambda" instead of "lambda1" as parameter name.}
\description{Overwritten, to allow "lambda" instead of "lambda1" as parameter name.
Besides that, simply delegates to super method.}
\arguments{\item{wrapped.learner}{Object of class \code{\linkS4class{wrapped.learner}}.}
\item{formula}{A symbolic description of the model to be fitted.}
\item{data}{Dataframe which includes all the data for the task.}
\item{weights}{An optional vector of weights to be used in the fitting process. Default is a weight of 1 for every case.}
\item{parset}{Named list which contains the hyperparameters of the learner. Default is an empty list, which means no hyperparameters are specifically set and defaults of the underlying learner are used.}
}
