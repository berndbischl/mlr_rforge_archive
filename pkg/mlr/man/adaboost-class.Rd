\name{adaboost-class}
\alias{adaboost-class}
\alias{adaboost}
\title{adaboost}
\description{Wrapped learner for Adaboost.M1 from package \code{adabag} for classification problems.}
\details{\emph{Common hyperparameters:}
\describe{
\item{\code{minsplit}}{Minimum number of observations that must exist in a node in order for a split to be attempted.}			
\item{\code{cp}}{Complexity parameter. Any split that does not decrease the overall lack of fit by a factor of cp is not attempted.}
\item{\code{maxdepth}}{Maximum depth of any node of the final tree, with the root node counted as depth 0. Defaults to the number of classes.} 
}}
\seealso{\code{\link[adabag]{adaboost.M1}}}
\section{Extends}{\code{\linkS4class{wrapped.learner.classif}}}
\section{Methods}{\describe{\item{\code{\link[=initialize,adaboost-method]{initialize}}}{\code{signature( = "adaboost")}: Constructor.}}}

