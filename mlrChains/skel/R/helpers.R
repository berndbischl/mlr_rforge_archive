removeFromDots = function(ns, ...) {
  args = list(...)
  args[setdiff(names(args), ns)]
}