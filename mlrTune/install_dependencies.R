pkgs <- c()

# Determine missing packages 
installed.pkgs <- rownames(installed.packages())
missing.pkgs <- setdiff(pkgs, installed.pkgs)

if(length(missing.pkgs) > 0) {
  cat("Installing the following packages:\n")
  print(missing.pkgs)
  install.packages(missing.pkgs, dep = TRUE, repos="http://cran.at.r-project.org")
} else {
  cat("All packages installed!\n")
}

