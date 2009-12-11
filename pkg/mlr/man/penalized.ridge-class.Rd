\name{penalized.ridge-class}
\alias{penalized.ridge-class}
\alias{penalized.ridge}
\title{penalized.ridge}
\description{Wrapped learner for Ridge Regression from package \code{penalized} for regression problems.}
\details{\emph{Common hyperparameters:}
\describe{
\item{\code{lambda2}}{Tuning parameter for L2 penalization.}			
\item{\code{epsilon}}{The convergence criterion.}
}}
\seealso{\code{\link[penalized]{penalized}}}
\section{Extends}{\code{\linkS4class{wrapped.learner.regr}}}
\section{Methods}{\describe{{\code{signature(.wrapped.learner = "penalized.ridge", .targetvar = "character", .data = "data.frame", .weights = "numeric", .costs = "missing", .type = "missing")}: Overwritten, to allow "lambda" instead of "lambda2" as parameter name.}{}}}
