### trivial case, no ids, no multiple targets
ct <- get.uci.task("balance-scale")
ct
str(ct)

ct <- get.uci.task("balance-scale", handle.ids = "remove")
ct
str(ct)


### handle ids
# remove ids
ct <- get.uci.task("kdd_synthetic_control", handle.ids = "exclude")
ct
str(ct)

# exclude ids
ct <- get.uci.task("kdd_synthetic_control", handle.ids = "remove")
ct
str(ct)


### train/test versions of data sets
ct <- get.uci.task("spect_train")
ct
str(ct)

ct <- get.uci.task("spect")
ct
str(ct)

ct <- get.uci.task("spect", handle.train.test = "train")
ct
str(ct)

ct <- get.uci.task("spect", handle.train.test = "test")
ct
str(ct)

ct <- get.uci.task("spect", handle.train.test = "all")
ct
str(ct)


### handle multiple targets
# flag data: targets[["flags.arff"]] = c("landmass", "zone", "language", "religion")
# default: handle.multiple.targets: target = NULL, handle.2nd.targets = "exclude"
ct <- get.uci.task("flags")
ct
str(ct)

# handle.multiple.targets: target = NULL, handle.2nd.targets = "remove"
ct <- get.uci.task("flags", handle.multiple.targets = list(target = NULL, handle.2nd.targets = "remove"))
ct
str(ct)

# handle.multiple.targets: target = NULL, handle.2nd.targets = "keep"
ct <- get.uci.task("flags", handle.multiple.targets = list(target = NULL, handle.2nd.targets = "keep"))
ct
str(ct)

# handle.multiple.targets: target = "zone", handle.2nd.targets = "exclude"
ct <- get.uci.task("flags", handle.multiple.targets = list(target = "zone", handle.2nd.targets = "exclude"))
ct
str(ct)

# handle.multiple.targets: target = "language", handle.2nd.targets = "remove"
ct <- get.uci.task("flags", handle.multiple.targets = list(target = "language", handle.2nd.targets = "remove"))
ct
str(ct)

# handle.multiple.targets: target = "religion", handle.2nd.targets = "keep"
ct <- get.uci.task("flags", handle.multiple.targets = list(target = "religion", handle.2nd.targets = "keep"))
ct
str(ct)


### pass further arguments to make.task
#   id as default name is used
ct <- get.uci.task("balance-scale", id = "b")
ct
str(ct)

ct <- get.uci.task("balance-scale")
ct
str(ct)

ct <- get.uci.task("balance-scale", id = "a")
ct
str(ct)

#   exclude; further variables to exclude
ct <- get.uci.task("balance-scale", exclude = "weight-left")
ct
str(ct)

ct <- get.uci.task("kdd_synthetic_control", handle.ids = "exclude", exclude = "col_1")
ct
str(ct)

ct <- get.uci.task("kdd_synthetic_control", handle.ids = "remove", exclude = "col_1")
ct
str(ct)

ct <- get.uci.task("flags", handle.multiple.targets = list(target = "zone", handle.2nd.targets = "exclude"), exclude = "quarters")
ct
str(ct)


#   weights
ct <- get.uci.task("balance-scale", weights = rep(1, 625))
ct
str(ct)

#   blocking
ct <- get.uci.task("balance-scale", blocking = factor(rep(1:25, 25)))
ct
str(ct)

#   costs
ct <- get.uci.task("breast-cancer", costs = matrix(1,2,2,dimnames = list(c("no-recurrence-events", "recurrence-events"),c("no-recurrence-events", "recurrence-events")))-diag(2))
ct
str(ct)

#   positive
ct <- get.uci.task("breast-cancer", positive = "recurrence-events")
ct
str(ct)


## special characters in colnames
# todo: change check.task
ct <- get.uci.task("spambase")
ct
str(ct)

ct <- get.uci.task("vehicle")
ct
str(ct)


## NAs in target variable
# todo: change prepare.df and runit.prepare
ct <- get.uci.task("bridges_version1")
