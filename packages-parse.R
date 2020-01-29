old.pattern <- list(type=list(
  before="[^<,]+",
  nc::quantifier(
    "<\\s*",
    inside="(?1)|[^<>]+",
    ">",
    "\\s*",
    "(?:const)?",
    "\\s*",
    "&?",
    "\\s*",
    "?")),
  "\\s+",
  name="[^\\s,)>]+",
  nc::quantifier(
    "\\s*",
    "=",
    "\\s*",
    default="[^\\s,)>]+",
    "?"),
  "[,)]")
nc::capture_all_str(
  "const Eigen::Matrix<double, 1, Eigen::Dynamic>& inv, const int& nerrs, foo< bar< sars > > var = 1, boost::ecuyer1988& base_rng__, std::ostream* pstream__)",
  old.pattern)

cleanType <- function(type.vec){
  if(length(type.vec)==0)return(type.vec)
  trans.list <- list(
    comments=function(x)gsub("/[*].*?[*]/", "", x),
    beginning=function(x)sub("^\\s+", "", x),
    end=function(x)sub("\\s+$", "", x),
    after.temp=function(x)gsub("([<>])\\s+", "\\1", x),
    before.temp=function(x)gsub("\\s+([<>])", "\\1", x),
    and=function(x)gsub("&", "", x),
    const=function(x)gsub("\\s*const\\s*", "", x),
    reps=function(x)gsub(" +", " ", x))
  for(fun in trans.list){
    type.vec <- fun(type.vec)
  }
  rep.dt <- unique(nc::capture_all_str(
    type.vec,
    namespace="\\b[^\\s:<]+?",
    "::",
    fun="[^<\\s]+?\\b")[!is.na(fun)])
  for(i in 1:nrow(rep.dt)){
    rep.row <- rep.dt[i]
    pattern <- paste0("(?<!::)\\b", rep.row$fun, "\\b")
    replace <- rep.row[, paste0(namespace, "::", fun)]
    type.vec <- gsub(pattern, replace, type.vec, perl=TRUE)
  }
  type.vec
}

type.pattern <- list(
  type=list(
    before="[^<]+",
    "<\\s*",
    inside="(?1)|[^<>]+",
    ">",
    "\\s*",
    "(?:const)?",
    "\\s*",
    "&?",
    "\\s*"),
  "::type")

parseRcppExports <- function(pkg.path){
  RcppExports.cpp <- normalizePath(file.path(
    pkg.path, "src", "RcppExports.cpp"),
    mustWork=TRUE)
  cpp.lines <- readLines(RcppExports.cpp)
  subject.vec <- gsub("/[*].*?[*]/", "", cpp.lines)
  ns.dt <- nc::capture_all_str(
    subject.vec,
    "using namespace ",
    namespace="[^ ;]+")
  fun.dt <- nc::capture_all_str(
    subject.vec,
    "\n// ",
    commentName=".*",
    "\n",
    prototype=list(
      returnType=".*",
      " ",
      funName=".*?",
      "\\(",
      arguments=".*",
      "\\);\n"),
    SEXP=".*\n",
    "BEGIN_RCPP\n",
    code="(?:.*\n)*?",
    "END_RCPP")
  arg.dt <- if(nrow(fun.dt)==0){
    NULL
  }else{
    fun.dt[, {
      code.vec <- strsplit(code, "\n")[[1]]
      no.comments <- sub("//.*", "", code.vec)
      input.vec <- grep("Rcpp::traits::input_parameter", no.comments, value=TRUE)
      if(length(input.vec)==0){
        NULL
      }else{
        nc::capture_first_vec(input.vec, type.pattern)
      }
    }, by=funName]
  }
  list(
    namespaces=ns.dt,
    prototypes=fun.dt[, .(funName, commentName, prototype)],
    arguments=arg.dt)
}

RcppExports.cpp.vec <- Sys.glob(file.path(
  "packages", "*", "src", "RcppExports.cpp"))
pkg.dir.vec <- dirname(dirname(RcppExports.cpp.vec))

arg.dt.list <- list()
ns.dt.list <- list()
for(pkg.dir in pkg.dir.vec){
  result.list <- parseRcppExports(pkg.dir)
  if(nrow(result.list$prototypes)==0){
    print(pkg.dir)
  }
  if(!is.null(result.list$arguments) && nrow(result.list$arguments)){
    arg.dt.list[[pkg.dir]] <- data.table::data.table(
      pkg.dir, result.list$arguments)
    ns.dt.list[[pkg.dir]] <- data.table::data.table(
      pkg.dir, result.list$namespaces)
  }
}
arg.dt <- do.call(rbind, arg.dt.list)
ns.dt <- do.call(rbind, ns.dt.list)

arg.dt[, clean.type := cleanType(inside)]
print(names(table(arg.dt$clean.type)))
print(names(table(ns.dt$namespace)))
arg.dt[clean.type=="longint"]
arg.dt[grepl(" ", funName)]

arg.counts <- arg.dt[, .(
  args=.N
), by=.(pkg.dir, funName)][arg.dt, on=.(pkg.dir, funName)]
(top10 <- arg.counts[args==1, .(
  funs=.N,
  pkgs=length(unique(pkg.dir))
), by=clean.type][order(-funs)][1:10])

(covered <- arg.counts[top10$clean.type, on="clean.type"][, .(
  top10args=.N
), by=.(pkg.dir, funName, args)][args==top10args][order(-args)])
##covered[funName=="repel_boxes"]
covered[, .(
  funs=.N,
  pkgs=length(unique(pkg.dir))
)]

(some.types <- grep("SEXP|List", top10$clean.type, invert=TRUE, value=TRUE))
some.covered <- arg.counts[some.types, on="clean.type"][, .(
  top10args=.N
), by=.(pkg.dir, funName, args)][args==top10args][order(-args)]
some.covered[, .(
  funs=.N,
  pkgs=length(unique(pkg.dir))
)]
