# name:                     name of UCI-data set (without url and ".arff"?)
# url:                      both possible urls, "tu-dortmund" is default
# handle.ids:               "remove" / "exclude", default = "exclude"
# handle.mutiple.targets:   list(target, handle.2nd.targets)
#   target:                 chosen target variable if there are several possibe targets, if NULL (default) the first one is used
#   handle.2nd.targets:     "remove" / "exclude" / "keep", default = "exclude"
# handle.train.test:        "train" / "test" / "all", default = NULL
# ...:                      further arguments to make.task, supported are
#                           id, label; as default name is used
#                           excluded; further variables to exclude
#                           weights
#                           blocking
#                           costs
#                           positive

get.uci.task <- function(name, url = c("http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff", 
    "http://repository.seasr.org/Datasets/UCI/arff"), handle.ids = "exclude", handle.multiple.targets = list(target = 
    NULL, handle.2nd.targets = "exclude"), handle.train.test = NULL, ...) {
    
    # Notlösung, solange die Infos über ids und targets nicht woanders abgelegt sind
    
    # ids
    ids = list() 
    ids[["bridges_version1.arff"]] = "IDENTIF" 
    ids[["bridges_version2.arff"]] = "IDENTIF" 
    ids[["flags.arff"]] = "name"
    ids[["kdd_synthetic_control.arff"]] = "index"
    ids[["molecular-biology_promoters.arff"]] = "instance"
    ids[["spectrometer.arff"]] = "LRS-name"     # evtl. sind weitere Variablen überflüssig, Beschreibung nicht verstanden
    ids[["splice.arff"]] = "Instance_name"

    # targets
    targets = list() 
    targets[["bridges_version1.arff"]] = c("MATERIAL", "REL-L", "SPAN", "T-OR-D", "TYPE")
    targets[["bridges_version2.arff"]] = c("MATERIAL", "REL-L", "SPAN", "T-OR-D", "TYPE")
    targets[["flags.arff"]] = c("landmass", "zone", "language", "religion")   # keep option makes sense here
    targets[["lung-cancer.arff"]] = "class" 
    targets[["molecular-biology_promoters.arff"]] = "class" 
    targets[["shuttle-landing-control.arff"]] = "Class" 
    targets[["spect_train.arff"]] = "OVERALL_DIAGNOSIS" 
    targets[["spect_test.arff"]] = "OVERALL_DIAGNOSIS" 
    targets[["spectf_train.arff"]] = "OVERALL_DIAGNOSIS" 
    targets[["spectf_test.arff"]] = "OVERALL_DIAGNOSIS"
    targets[["solar-flare_1.arff"]] = c("C-class_flares_production_by_this_region", "M-class_flares_production_by_this_region", "X-class_flares_production_by_this_region") # exclude or remove
    targets[["solar-flare_2.arff"]] = c("C-class_flares_production_by_this_region", "M-class_flares_production_by_this_region", "X-class_flares_production_by_this_region") # exclude or remove
    targets[["spectrometer.arff"]] = "LRS-class" # enthält auch Subklassen, 10er KLassen, 1er Subklassen
    targets[["wine.arff"]] = "class"    
            
    # file name(s)
    file <- name
    if (!is.null(handle.train.test)) {
        file <- switch(handle.train.test,
            "train" = paste(name, "train", sep = "_"),
            "test" = paste(name, "test", sep = "_"),
            "all" = paste(name, c("test", "train"), sep = "_")
        )
    }
    file <- paste(file, ".arff", sep = "")
    url <- match.arg(url)
    
    arff.to.task(file = file.path(url, file), target = targets[[file[1]]], ids = ids[[file[1]]], handle.multiple.targets = handle.multiple.targets, 
        handle.ids = handle.ids, handle.train.test = handle.train.test, name = name, ...)
}
