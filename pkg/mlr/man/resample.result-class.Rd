\name{resample.result-class}
\alias{resample.result-class}
\alias{resample.result}
\title{resample.result}
\description{\code{resample.result} contains the results of a resampling process, mainly the list of predicitions and - 
if saved - the list of fitted models.}
\seealso{\code{\linkS4class{resample.desc}}, \code{\linkS4class{resample.instance}}, 
\code{\link{make.res.instance}},  
\code{\link{resample.fit}}}
\section{Slots}{\describe{\item{\code{instance}:}{(\code{\link{resample.instance}}) Resample instance used to produce the result}\item{\code{preds}:}{(\code{\link{list}}) List of predictions, i.e. list of factors or list of matrices for classification respectivly regression.}\item{\code{extracted}:}{(\code{\link{list}}) List of extracted informations from the models. Could also be the complete fitted models.}}}
\section{Methods}{\describe{\item{\code{\link[=resample.performance,learn.task,resample.result,loss,function,function,function-method]{resample.performance}}}{\code{signature(task = "learn.task", result = "resample.result", loss = "loss", aggr1 = "function", aggr2 = "function", spread = "function")}: Measures the quality of predictions w.r.t. some loss function for a resampled fit.}
