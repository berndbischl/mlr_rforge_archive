pkgs <- c("abind", "ROCR", "RUnit", "MASS", "rpart", "e1071", "boot",
          "roxygen", "kernlab", "adabag", "kknn", "randomForest",
          "ada", "mboost", "mda", "gbm", "penalized", "mlbench",
          "reshape", "klaR", "snowfall", "nnet", "RWeka", "party",
          "earth", "cmaes")

ipkgs <- rownames(installed.packages())

npkgs <- setdiff(pkgs, ipkgs)

install.packages(npkgs)
