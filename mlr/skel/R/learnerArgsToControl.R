#' Split arguments into 'control' and 'other' arguments.
#'
#' Find all elements in list \code{args} whose name is contained in
#' \code{arg.names} and call function \code{control} on these. The
#' result of this is returned as the \code{control} element of the
#' list returned. All remaining elements in \code{args} are returned
#' as the \code{args} element of the return list.
#'
#' @param control [function] \cr Function to apply to the elements of
#'   \code{args} named in \code{arg.names}.
#' @param arg.names [character] \cr List of argument names to extract
#'   from \code{args}.
#' @param args [list] \cr List of named arguments to be split into
#'   control and other arguments.
#' @return List with elements \code{control} and \code{args}.
#' @export
learnerArgsToControl = function(control, arg.names, args) {
  checkArg(control, "function")
  checkArg(arg.names,"character")
  checkArg(args, "list")
  # put stuff into special list and remove it from args
  ctrl.args = insert(list(), args, arg.names)
  ctrl = do.call(control, ctrl.args)
  args[arg.names] = NULL
  return(list(control=ctrl, args=args))
}
