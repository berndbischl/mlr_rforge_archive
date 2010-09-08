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


arff.to.task <- function(file, target, ids, handle.ids, handle.multiple.targets, segment, name) {
    # evtl. weitere exclude variablen in ...
    excluded <- removed <- character(0)
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
    df <- read.arff2(file, remove = removed)
print(str(df))
    if(is.null(target)) target <- names(df)[length(df)]                     # if no target is given take last column
print(target)
    if(!is.null(segment)) {
        df <- data.frame(segment = segment, df)                             # segment as first column
        excluded <- c(excluded, "segment")                                  # exclude segment variable
    }
    ct <- make.task(id = name, label = name, data = df, target = target, excluded = excluded)
    return(ct)
}
