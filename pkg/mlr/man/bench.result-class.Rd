\name{bench.result-class}
\alias{bench.result-class}
\alias{bench.result}
\title{bench-result}
\description{Container for the results of a benchmark experiment.}
\details{\code{bench.result-class}: Getter. \cr
The following getters all return list of lists of objects: prediction, conf.mat
The first list iterates the tasks, the second one the learners, both are named by respective IDs.
You can reduce these list by using the optional arguments 'task' and 'learner'. 
'drop' is by default TRUE, which means that the list structures are simplified as much as possible, if you don't want this set 'drop' to FALSE. 

The following getters all return list of lists of lists: opt.result, opt.par, opt.perf, opt.path, tuned.par, sel.var
The first list iterates the tasks, the second one the learners, both are named by respective IDs, the third list iterates the
resampling iterations. You can reduce these list by using the optional arguments 'task' and 'learner'. 
'drop' is by default TRUE, which means that the list structures are simplified as much as possible, if you don't want this set 'drop' to FALSE. 

\describe{
\item{learners [character]}{IDs of learners used in experiment.}
\item{tasks [character]}{IDs of tasks used in experiment.}
\item{measures [character]}{Names of measures recorded in experiment.}
\item{iters [numeric]}{Named numerical vector which lists the number of iterations for every task. Names are IDs of task.}
\item{prediction [see above] }{List of list of predictions for every task/learner. }
\item{conf.mat [see above] }{List of list of confusion matrices for every task/learner. }
\item{opt.result [see above] }{List of list of list of \code{\linkS4class{opt.result}} for every task/learner/iteration. Entry is NULL if no optimization was done.}
\item{opt.perf [see above] }{List of list of list of performance vectors of optimal settings for every task/learner/iteration. Note that this performance refers to the inner resampling! Entry is NULL if no optimization was done.}
\item{opt.par [see above] }{List of list of list of optimal settings for every task/learner/iteration. Entry is NULL if no optimization was done.}
\item{opt.path [see above] }{List of list of list of optimization paths for every task/learner/iteration. Entry is NULL if no optimization was done.}
\item{tuned.par [see above] }{List of list of list of optimal hyperparameters for every task/learner/iteration. Entry is NULL if no tuning was done. Basically a different name for "opt.par".}
\item{sel.var [see above] }{List of list of list of optimal features for every task/learner/iteration. Entry is NULL if no feature selection was done.. Basically a different name for "opt.par".}
\item{perf [list]}{Lists of 3 dimensional arrays of performance values for every data set.}
}

}
\seealso{\code{\link{bench.exp}}}
\section{Extends}{\code{\linkS4class{object}}}
\section{Methods}{\describe{\item{\code{\link[=[,bench.result-method]{[}}}{}\item{\code{\link[=to.string,bench.result-method]{to.string}}}{}}}
\alias{[,bench.result-method}

