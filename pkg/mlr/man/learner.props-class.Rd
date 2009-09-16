\name{learner.props-class}
\alias{learner.props-class}
\alias{learner.props}
\title{Since not all classifiers can deal with all kind of data properties of the data are compared...}
\description{Since not all classifiers can deal with all kind of data properties of the data are compared 
with the possibilities of the classifier when a \code{\linkS4class{learn.task}} is generated. 
A classif.props object describes such classifier possibilities by logical values.}
\note{When a \code{\linkS4class{learn.task}} gets printed its classifier description is shown on the console.}
\section{Slots}{\describe{\item{\code{supports.missing}:}{(\code{\link{logical}}) [logical] Can the classifier deal with missing values?}\item{\code{supports.numerics}:}{(\code{\link{logical}}) [logical] Can the classifier deal with numeric variables?}\item{\code{supports.factors}:}{(\code{\link{logical}}) [logical] Can the classifier deal with factor variables?}\item{\code{supports.characters}:}{(\code{\link{logical}}) [logical] Can the classifier deal with character variables?}\item{\code{supports.weights}:}{(\code{\link{logical}}) [logical] Can the classifier return class probabilities when predicting?}}}
\section{Methods}{\describe{\item{\code{\link[=show,learner.props-method]{show}}}{\code{signature( = "learner.props")}: Displays the object}\item{\code{\link[=print,learner.props-method]{print}}}{\code{signature( = "learner.props")}: Prints the object}}}

