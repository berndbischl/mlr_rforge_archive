\name{wrapped.learner-class}
\alias{wrapped.learner-class}
\alias{wrapped.learner}
\title{wrapped.learner}
\description{Wraps an already implemented learning method from R to make it accesible to mlr.}
\details{Also inlcudes a properties object to describe the features of the learner.}
\section{Slots}{\describe{\item{\code{learner.name}:}{(\code{\link{character}}) Descriptive name of the learning method}\item{\code{learner.pack}:}{(\code{\link{character}}) R package where learner is implemented}\item{\code{train.fct}:}{(\code{\link{function}}) Function used in above package to train a regular model in the package}\item{\code{train.fct.pars}:}{(\code{\link{list}}) Named list of parameters which are fixed in the above train.fct and used at every internal call.}\item{\code{predict.fct}:}{(\code{\link{function}}) Function used in above package to predict new data with a trained model}\item{\code{predict.newdata.arg}:}{(\code{\link{character}}) Name of argument for the new data frame in the underlying predict method.}\item{\code{predict.fct.pars}:}{(\code{\link{list}}) Named list of parameters which are fixed in the above predict.fct and used at every internal call.}\item{\code{learner.props}:}{(\code{\link{learner.props}}) Properties of the learner}}}
\section{Methods}{\describe{\item{\code{\link[=train.learner,wrapped.learner,formula,data.frame,numeric,list-method]{train.learner}}}{\code{signature(wrapped.learner = "wrapped.learner", formula = "formula", data = "data.frame", weights = "numeric", parset = "list")}: Mainly for internal use. Trains a wrapped learner on a giving training set, 
possibly w.r.t. some hyperparamters and case weights.}\item{\code{\link[=initialize,wrapped.learner-method]{initialize}}}{\code{signature( = "wrapped.learner")}: Constructor.}\item{\code{\link[=set.train.par,wrapped.learner-method]{set.train.par}}}{\code{signature( = "wrapped.learner")}: Set a parameter for the underlying train function of a 
[\code{\linkS4class{wrapped.learner}}].
This is not meant for hyperparameters, pass these through the usual parset argument, but rather to
fix (somewhat technical) arguments which stay the same for the whole experiment. You should not have to use this too often.}\item{\code{\link[=set.predict.par,wrapped.learner-method]{set.predict.par}}}{\code{signature( = "wrapped.learner")}: Set a parameter for the underlying predict function of a wrapped learner. 
Used to fix (somewhat techical) arguments which stay the same for the whole experiment.
You should not have to use this too often.}}}
