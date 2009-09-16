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
\section{Methods}{\describe{\item{\code{\link[=initialize,penalized.ridge-method]{initialize}}}{\code{signature( = "penalized.ridge")}: Constructor.}\item{\code{\link[=train.learner,penalized.ridge,formula,data.frame,numeric,list-method]{train.learner}}}{\code{signature(wrapped.learner = "penalized.ridge", formula = "formula", data = "data.frame", weights = "numeric", parset = "list")}: Overwritten, to allow "lambda" instead of "lambda2" as parameter name.
Besides that, simply delegates to super method.}}}

