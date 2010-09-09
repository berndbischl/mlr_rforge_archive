# file:                     filepath or url
# target:                   can be more than one. first one is selected, rest excluded/removed/kept,
#                           default is target = NULL, then the last column is taken as target variable
# ids:                      names of id variables
# handle.ids:               "remove" / "exclude" option
# handle.mutiple.targets:   list(target, handle.2nd.targets)
#   target:                 chosen target variable if there are several possibe targets, if NULL the first one in target is used
#   handle.2nd.targets:     "remove" / "exclude" / "keep"
# segment:                  e.g. train or test, or NULL
# name:                     name of UCI data set
# id, label:                id, label of the task; as default name is used
# excluded:                 further variables to exclude
# ...:                      further arguments to make.task

arff.to.task <- function(file, target, ids, handle.ids, handle.multiple.targets, segment, name, id = name, label = name, excluded = character(0), ...) {
    removed <- character(0)
print(removed)
print(excluded)
    if(handle.ids == "exclude") excluded <- c(excluded, ids) else removed <- c(removed, ids)
    if(length(target > 1)) {                                                # multiple targets
        if(!is.null(handle.multiple.targets$target)) {
            index <- target %in% handle.multiple.targets$target
            if(any(index)) {
                if(handle.multiple.targets$handle.2nd.targets == "exclude") excluded <- c(excluded, target[!index]) 
                if(handle.multiple.targets$handle.2nd.targets == "remove") removed <- c(removed, target[!index])
                target <- target[index]
            } else stop("chosen target not available")
        } else {
            if(handle.multiple.targets$handle.2nd.targets == "exclude") excluded <- c(excluded, target[-1]) 
            if(handle.multiple.targets$handle.2nd.targets == "remove") removed <- c(removed, target[-1])
            target <- target[1]
        }
    }
print(removed)
print(excluded)
    data <- read.arff2(file, remove = removed)
print(str(data))
    if(is.null(target)) target <- names(data)[length(data)]                     # if no target is given take last column
print(target)
    if(!is.null(segment)) {
        data <- data.frame(segment = segment, data)                             # segment as first column
        excluded <- c(excluded, "segment")                                  # exclude segment variable
    }
    ct <- make.task(id = id, label = label, data = data, target = target, excluded = excluded, ...)
    return(ct)
}
