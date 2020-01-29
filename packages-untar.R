setwd("packages")
tgz.vec <- Sys.glob("*.tar.gz")
for(pkg.tar.gz in tgz.vec){
  pkg.name <- sub("_.*", "", basename(pkg.tar.gz))
  RcppExports.cpp <- file.path(pkg.name, "src/RcppExports.cpp")
  system(paste("tar -xvf", pkg.tar.gz, RcppExports.cpp))
}
