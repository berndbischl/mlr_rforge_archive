#' Base class for supervised learning tasks.
#' 
#' It encapsulates the data and specifies - through its subclasses - the type of the task (either classification or regression), 
#' the target variable and other details of the problem. 
#'  
#' \describe{
#' \item{env [\code{environment}]}{Environment where data for the task are stored. Use \code{\link{getTaskData}} in order to access it.}
#' \item{blocking [\code{factor}]}{Observations with the same blocking level belong together. Specifically, they are either put all in the training or the test set during a resampling iteration. \code{factor(0)} if no blocking was set.}
#' \item{desc [\code{\link{TaskDesc}}]}{Encapsulates further information about the task.}
#' }
#' $seealso \code{\link{makeClassifTask}}, \code{\link{makeRegrTask}}
#' $name SupervisedTask
#' $rdname SupervisedTask
#' $export 
NULL

makeSupervisedTask = function(data, blocking, desc) {
  env = new.env()
  env$data = data
  structure(list(
    env = env,
    blocking = blocking,
    desc = desc
  ), class="SupervisedTask")
}

print.SupervisedTask = function(x, ...) {
  td = x$desc
  feat = printToChar(td$n.feat)
  cat(
    "Supervised problem: ", td$type, "\n",
    "Features:\n", feat, "\n", 
    "Observations: ", td$size , "\n",
    "Missings: ", td$has.missing, "\n", 
    "Target: ", td$target, "\n", 
    "Has blocking: ", td$has.blocking, "\n",
    sep=""
  )
}

