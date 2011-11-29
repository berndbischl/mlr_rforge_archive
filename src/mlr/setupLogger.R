#' Sets up the logging system of mlr, which control which output is shown where.
#' 
#' @param level [\code{character(1)}] \cr 
#'   Which logging levels should be printed: \dQuote{error}, \dQuote{warn}, \dQuote{info}, \dQuote{debug}. 
#'   Default is \dQuote{info}.    
#' @param show.learner.output [\code{logical(1)}] \cr 
#'   Should the output of the learning algorithm during training and prediction be shown or captured and
#'   suppressed?
#'   Default is \code{TRUE}.      
#' @return NULL.
#' @export
#' @title Logger setup.

setupLogger = function(level="info", show.learner.output=TRUE) {
  if (level=="error") {
    options(warn=-1)
  } else {
    options(warn=1)
  }
  logger.setup = list()
  logger.setup$global.level = level
  logger.setup$show.learner.output = show.learner.output
  .mlr.local$logger.setup <<- logger.setup
  return(NULL)
}
