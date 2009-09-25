\name{resample.result-class}
\alias{resample.result-class}
\alias{resample.result}
\title{resample.result}
\description{\code{resample.result} contains the results of a resampling process, mainly the list of predicitions and - 
if saved - the list of fitted models.}
\seealso{\code{\linkS4class{resample.desc}}, \code{\linkS4class{resample.instance}}, 
\code{\link{make.cv.instance}}, \code{\link{make.bs.instance}}, 
\code{\link{make.subsample.instance}}, \code{\link{resample.fit}}}
\section{Slots}{\describe{\item{\code{ri.class}:}{(\code{\link{character}}) Class of resample.instance}\item{\code{ri.name}:}{(\code{\link{character}}) Name of resample.instance}\item{\code{preds}:}{(\code{\link{list}}) List of predictions, i.e. list of factors or list of matrices for classification respectivly regression.}\item{\code{models}:}{(\code{\link{list}}) List of the fitted models.}}}
\section{Methods}{\describe{\item{\code{\link[=resample.performance,learn.task,resample.instance,resample.result,list-method]{resample.performance}}}{\code{signature(learn.task = "learn.task", resample.instance = "resample.instance", resample.result = "resample.result", measure = "list")}: Measures the quality of predictions w.r.t. some loss function for a resampled fit.}\item{\code{\link[=[,resample.result-method]{[}}}{\code{signature( = "resample.result")}: Getter.}}}
