get.rev.nr <- function() {
  if (.Platform$OS.type == "unix") {
    rev.nr <- as.numeric(gsub("M", "", system("svnversion", intern=TRUE)))
  } else {
        working.copy  <- project.dir
        rev.in  <- file.path(src.dir, "make", "rev_nr")
        rev.out <- file.path(src.dir, "make", "rev_nr.out")
        svn.path <- paste("\"", svn.path, "\"", sep="")
        cmd <- paste(svn.path, working.copy, rev.in, rev.out, "-f")
        print(cmd)
        system(cmd)     
        rev.nr <- scan(file=rev.out)
  }
  ## add 1 because we need to commit to r-forge before its build there
  return(rev.nr+1)
}
