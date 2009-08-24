\name{data.desc-class}
\alias{data.desc-class}
\alias{data.desc}
\title{data.desc}
\description{Since not all classifiers can deal with all kind of data, the properties of the data are compared 
with the supported features of the learner when a \code{\linkS4class{learn.task}} is generated.
A \code{data.desc} object contains a description of these data properties.}
\section{Slots}{\describe{\item{\code{target.col}:}{(\code{\link{integer}}) Column index of the response}\item{\code{is.classification}:}{(\code{\link{logical}}) Is the target variable categorical?}\item{\code{class.nr}:}{(\code{\link{integer}}) Does the dataset have missing values?}\item{\code{has.missing}:}{(\code{\link{logical}}) Does the dataset have missing values?}\item{\code{numerics}:}{(\code{\link{integer}}) Does the dataset have numeric variables?}\item{\code{integers}:}{(\code{\link{integer}}) Does the dataset have integer variables?}\item{\code{factors}:}{(\code{\link{integer}}) Does the dataset have factor variables?}\item{\code{characters}:}{(\code{\link{integer}}) Does the dataset have character variables?}\item{\code{obs}:}{(\code{\link{integer}}) Number of examples in dataset.}}}
\section{Methods}{\describe{\item{\code{\link[=initialize,data.desc-method]{initialize}}}{}\item{\code{\link[=print,data.desc-method]{print}}}{}\item{\code{\link[=as.character,data.desc-method]{as.character}}}{}\item{\code{\link[=print,data.desc-method]{print}}}{}\item{\code{\link[=show,data.desc-method]{show}}}{}}}

