# name:                     name of UCI-data set (without url and ".arff"?)
# url:                      both possible urls, "tu-dortmund" is default
# handle.ids:               "remove" or "exclude"
# handle.mutiple.targets:   list(target, handle.2nd.targets)
#   target:                 chosen target variable if there are several possibe targets, if NULL the first one is used
#   handle.2nd.targets:     "remove" / "exclude" / "keep"
# ...:                      further arguments to make.task, supported are
#                           id, label; as dafault name is used
#                           excluded; further variables to exclude
#                           weights
#                           blocking
#                           costs
#                           positive

get.uci.task <- function(name, url = c("http://www.statistik.tu-dortmund.de/download/Datasets/UCI/arff", 
"http://repository.seasr.org/Datasets/UCI/arff"), handle.ids = "exclude", handle.multiple.targets = list(target = 
NULL, handle.2nd.targets = "exclude"), ...) {
    
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
    targets[["spectf_test.arff"]] = "OVERALL_DIAGNOSIS" 
    targets[["spectf_test.arff"]] = "OVERALL_DIAGNOSIS"
    targets[["solar-flare_1.arff"]] = c("C-class_flares_production_by_this_region", "M-class_flares_production_by_this_region", "X-class_flares_production_by_this_region") # exclude or remove
    targets[["solar-flare_2.arff"]] = c("C-class_flares_production_by_this_region", "M-class_flares_production_by_this_region", "X-class_flares_production_by_this_region") # exclude or remove
    targets[["spectrometer.arff"]] = "LRS-class" # enthält auch Subklassen, 10er KLassen, 1er Subklassen
    targets[["wine.arff"]] = "class"    
            
    # wenn name "train" oder "test" enthält, Spalte an den Datensatz anhängen zu Identifizierung, die aber in der task excluden
    # wie mergen? automatisch den zweiten Datensatz dazuladen?
    segment <- NULL
    if(grepl("train", name)) segment <- "train"
    if(grepl("test", name)) segment <- "test"
    
    file <- paste(name, ".arff", sep = "")
    url <- match.arg(url)
    
    arff.to.task(file = file.path(url, file), target = targets[[file]], ids = ids[[file]], handle.ids = handle.ids, 
        handle.multiple.targets = handle.multiple.targets, segment = segment, name = name, ...)
}
