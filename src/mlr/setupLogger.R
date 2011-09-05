# todo: can we log where the current log was generated, like in which method (automatically)?

#' Sets up the logging system of mlr. 
#' 
#' @param console [\code{logical(1)}] \cr
#'   Should output be printed to R console?
#' @param file [\code{character(1)}] \cr 
#'   Path to file to redirect output into.  
#' @param level [character] \cr 
#'   Which logging levels should be printed: 'error', 'warn', 'info', 'debug'. Default is 'info'.     
#' @param sublevel [character] \cr 
#'   Which logging sublevels should be printed. Default is NA which means all logging of the selected main level is printed.
#'   Currently for 'debug' are available: 'train', 'predict' and 'parallel'.      
#'   Currently for 'info' are available: 'tune'.      
#' @return NULL.
#' @export
#' @title Logger setup.

setupLogger <- function(console=TRUE, file=NA, level, sublevel=NA) {
  if (level=="error") {
    options(warn=-1)
  } else {
    options(warn=1)
  }
  
  logger.setup <- list()
  logger.setup$console <- console
  logger.setup$file <- file
  logger.setup$global.level <- level
  logger.setup$sublevel <- sublevel
  
  .mlr.local$logger.setup <- logger.setup
  
  if (!(is.na(file))) 
    unlink(file)
  return(NULL)
}
