\name{opt.result-class}
\alias{opt.result-class}
\alias{opt.result}
\title{Optimization result.}
\description{Container for results of hyperparameter tuning or variable selection.
Contains the obtained optimal parameter vector, its performance values
and the optimization path which lead there. It might also optionally
contain a wrapped.model, which was fitted by using the optimal
parameters.}
\details{\code{opt.result-class}: Getter.\cr

\describe{
\item{par [list | character]}{Named list of hyperparameter values
or character vector of variables, id#entified as optimal.}
\item{perf [numeric]}{Performance values of 'par'.}
\item{path [list | data.frame]. Optional parameters: as.data.frame}{
Optimization path. Can be converte#d to a data.frame if as.data.frame is TRUE.}
\item{model [wrapped.model]}{Model fitted with settings in 'par'. Will
be NULL, if fitting was not req#uested.}
}

}
\seealso{\code{\link{tune}}, \code{\link{varsel}}}
\section{Extends}{\code{\linkS4class{object}}}
\section{Methods}{\describe{\item{\code{\link[=[,opt.result-method]{[}}}{}\item{\code{\link[=to.string,opt.result-method]{to.string}}}{}}}
\alias{[,opt.result-method}

