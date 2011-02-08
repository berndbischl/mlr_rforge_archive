# file:                     filepath or url
# target:                   can be more than one. first one is selected, rest excluded/removed/kept,
#                           default is target = NULL, then the last column is taken as target variable
# ids:                      names of id variables
# handle.ids:               "remove" / "exclude" option
# handle.mutiple.targets:   list(target, handle.2nd.targets)
#   target:                 chosen target variable if there are several possibe targets, if NULL the first one in target is used
#   handle.2nd.targets:     "remove" / "exclude" / "keep"
# handle.nas:               
# name:                     name of UCI data set
# id :                      id of the task; as default name is used
# :                 further variables to exclude
# ...:                      further arguments to make.task

arff.to.task <- function(file, target, ids, handle.multiple.targets, handle.ids, handle.train.test, handle.nas, name, 
    id = name, exclude = character(0), ...) {
    removed <- character(0)

    # ids
    if(handle.ids == "exclude") exclude <- c(exclude, ids) else removed <- c(removed, ids)
    # multiple targets
    if(length(target > 1)) {
        if(!is.null(handle.multiple.targets$target)) {
            index <- target %in% handle.multiple.targets$target
            if(any(index)) {
                if(handle.multiple.targets$handle.2nd.targets == "exclude") exclude <- c(exclude, target[!index]) 
                if(handle.multiple.targets$handle.2nd.targets == "remove") removed <- c(removed, target[!index])
                target <- target[index]
            } else stop("chosen target not available")
        } else {
            if(handle.multiple.targets$handle.2nd.targets == "exclude") exclude <- c(exclude, target[-1]) 
            if(handle.multiple.targets$handle.2nd.targets == "remove") removed <- c(removed, target[-1])
            target <- target[1]
        }
    }
    
    # if both train and test data should be read
    if (!is.null(handle.train.test) && handle.train.test == "all") {
        data <- lapply(file, read.arff2, remove = removed)
        names(data) <- c("test", "train")
        nr <- sapply(data, nrow)
        mlr_segment <- factor(rep(c("test", "train"), nr))    # mlr_segment as first column
        data$test <- data.frame(mlr_segment = mlr_segment[1:nr["test"]], data$test)
        data$train <- data.frame(mlr_segment = mlr_segment[(nr["test"]+1):sum(nr)], data$train, row.names = (nr["test"]+1):sum(nr))
        data <- unsplit(data, mlr_segment)
        exclude <- c(exclude, "mlr_segment")  # exclude mlr_segment from task
    } else {
        data <- read.arff2(file, remove = removed)
    }

    # if no target variable is given take last column
    if(is.null(target)) target <- names(data)[length(data)]
    
    # NAs
    if(!is.null(handle.nas)) data = handle.nas(data)

    # Variablen umbenennen, falls Sonder- oder Leerzeichen, 
    # todo: später in check.task einbauen
    #forbidden  = c("[", "]", "(", ")", ",", " ")
    #   ! " # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~
    #bsp <- c("v 51", "v ?", "v !", "v $", "v %", "v &", "v '", "v (", "v )", "v *", "v +", "v ,", "v -", "v #",
    #"v .", "v /", "v :", "v ;", "v <", "v =", "v >", "v ?", "v @", "v [", "v \\", "v ]", "v ^", "v _", "v `", "v {", "v |", "v }", "v ~")
    names(data) <- gsub(pattern = "\\[", replacement = "s91", x = names(data))
    names(data) <- gsub(pattern = "]", replacement = "s93", x = names(data))
    names(data) <- gsub(pattern = "\\(", replacement = "s40", x = names(data))
    names(data) <- gsub(pattern = ")", replacement = "s41", x = names(data))
    names(data) <- gsub(pattern = ",", replacement = "s13", x = names(data))
    # randomForest hat Probleme mit - Zeichen
    names(data) <- gsub(pattern = "-", replacement = "_", x = names(data))   
    #names(data) <- gsub(pattern = "([][(),])", replacement = "93 91 40 41 13", x = names(data))
    names(data) <- gsub(' +', "_", names(data))# ' ' oder ' +'?

    ct <- make.task(id = id, data = data, target = target, exclude = exclude, ...)
    return(ct)
}
