cA.dir <- "~/R/RcppExports/compileAttributes"
dir.create(cA.dir, showWarnings=FALSE)
setwd(cA.dir)
tgz.vec <- Sys.glob("../packages/*.tar.gz")
for(pkg.i in seq_along(tgz.vec)){
  pkg.tar.gz <- tgz.vec[[pkg.i]]
  cat(sprintf("%4d / %4d %s\n", pkg.i, length(tgz.vec), pkg.tar.gz))
  pkg.name <- sub("_.*", "", basename(pkg.tar.gz))
  RcppExports.cpp <- file.path(pkg.name, "src/RcppExports.cpp")
  ## untar(pkg.tar.gz)
  ## unlink(RcppExports.cpp)
  ## Rcpp::compileAttributes(pkg.name)
  generated <- if(file.exists(RcppExports.cpp)){
    readLines(RcppExports.cpp)
  }else{
    character()
  }
  unlink(pkg.name, recursive=TRUE)
  if(length(generated)){
    dir.create(dirname(RcppExports.cpp), recursive=TRUE)
    writeLines(generated, RcppExports.cpp)
  }
}

