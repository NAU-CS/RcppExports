pkg.name.vec <- dir("packages")
dir.create("checks", showWarnings=FALSE)
pkg.check.vec <- file.path("checks", pkg.name.vec)
new.i.vec <- which(!file.exists(pkg.check.vec))
for(pkg.i in new.i.vec){
  pkg.name <- pkg.name.vec[[pkg.i]]
  cat(sprintf("%4d / %4d %s\n", pkg.i, length(new.i.vec), pkg.name))
  u <- paste0(
    "https://cloud.r-project.org/web/checks/check_results_",
    pkg.name,
    ".html")
  pkg.check <- pkg.check.vec[[pkg.i]]
  tryCatch({
    download.file(u, pkg.check)
  }, error=function(e){
    print(e)
  })
}

