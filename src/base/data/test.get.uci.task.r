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
# merging?


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
