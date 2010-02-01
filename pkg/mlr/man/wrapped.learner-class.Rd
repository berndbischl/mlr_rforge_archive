\name{wrapped.learner-class}
\alias{wrapped.learner-class}
\alias{wrapped.learner}
\title{wrapped.learner}
\description{Wraps an already implemented learning method from R to make it accesible to mlr.}
\details{Also inlcudes a properties object to describe the features of the learner.}
\section{Slots}{\describe{\item{\code{learner.name}:}{(\code{\link{character}}) Descriptive name of the learning method}\item{\code{learner.pack}:}{(\code{\link{character}}) R package where learner is implemented}\item{\code{train.fct.pars}:}{(\code{\link{list}}) Named list of parameters which are fixed in an internal call to the underlying train function of the learner.}\item{\code{predict.fct.pars}:}{(\code{\link{list}}) Named list of parameters which are fixed in an internal call to the underlying predict function of the learner.}\item{\code{learner.props}:}{(\code{\link{learner.props}}) Properties of the learner}}}
\section{Methods}{\describe{\item{\code{\link[=benchmark,wrapped.learner,learn.task,resample.instance-method]{benchmark}}}{}\item{\code{\link[=resample.fit,wrapped.learner,learn.task,resample.instance,list,character,character,function-method]{resample.fit}}}{}\item{\code{\link[=resample.fit,wrapped.learner,learn.task,resample.desc,list,character,character,function-method]{resample.fit}}}{}\item{\code{\link[=initialize,wrapped.learner-method]{initialize}}}{\code{signature( = "wrapped.learner")}: Constructor.}\item{\code{\link[=[,wrapped.learner-method]{[}}}{}
