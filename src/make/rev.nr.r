get.rev.nr = function(pack.name) {
  if (.Platform$OS.type == "unix") {
    wd = getwd()
    # only use rev nr of correct src dir
    setwd(file.path(project.dir, "src", pack.name))
    # remove M for modified
    rev.nr = gsub("M", "", system("svnversion -c", intern=TRUE))
    # use max nr in mixed revision
    rev.nr = strsplit(rev.nr, ":")[[1]]
    rev.nr = as.integer(rev(rev.nr)[1])
    setwd(wd)
  } else {
        working.copy = file.path(project.dir, "src", pack.name)
        rev.in  = file.path(src.dir, "make", "rev_nr")
        rev.out = file.path(src.dir, "make", "rev_nr.out")
        svn.path = paste("\"", svn.path, "\"", sep="")
        cmd = paste(svn.path, working.copy, rev.in, rev.out, "-f")
        print(cmd)
        system(cmd)     
        rev.nr = scan(file=rev.out)
  }
  ## add 1 because we need to commit to r-forge before its build there
  return(rev.nr+1)
}
