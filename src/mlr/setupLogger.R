#' Sets up the logging system of mlr, which controls which output is shown.
#' 
#' @title Logger setup.
#' @param level [\code{character(1)}] \cr 
#'   Which logging levels should be printed: \dQuote{error}, \dQuote{warn}, \dQuote{info}, \dQuote{debug}. 
#'   Default is \dQuote{info}.    
#' @param show.learner.output [\code{logical(1)}] \cr 
#'   Should the output of the learning algorithm during training and prediction be shown or captured and
#'   suppressed?
#'   Default is \code{TRUE}.      
#' @return Nothing.
#' @export

setupLogger = function(level="info", show.learner.output=TRUE) {
  checkArg(level, choices=c("error", "warn", "info", "debug"))
  checkArg(show.learner.output, "logical", len=1, na.ok=FALSE)
  
  if (level=="error") {
    options(warn=-1)
  } else {
    options(warn=1)
  }
  .mlr.local$logger.setup = list(
    global.level = level,
    show.learner.output = show.learner.output
  )
  invisible(NULL)
}
