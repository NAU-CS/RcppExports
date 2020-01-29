if(FALSE){
  system("grep input_parameter packages/*/src/RcppExports.cpp > input_parameter.txt")
}

input.vec <- readLines("input_parameter.txt")
empty.comments <- sub("//.*", "", input.vec)
removed.comments <- grep("input_parameter", empty.comments, value=TRUE)

file.pattern <- list(
  "packages/",
  package=".*?",
  "/src/RcppExports.cpp:\\s*",
  type=list(
    before="[^<]+",
    "<\\s*",
    inside="(?2)|[^<>]+",
    ">",
    "\\s*",
    "(?:const)?",
    "\\s*",
    "&?",
    "\\s*"),
  "::type")
nc::capture_first_vec(
  "packages/HDclust/src/RcppExports.cpp:    Rcpp::traits::input_parameter< Nullable<List> >::type rfsClust_(rfsClust_SEXP);",
  file.pattern,
  nomatch.error=FALSE,
  engine="PCRE")

match.dt <- nc::capture_first_vec(
  removed.comments,
  file.pattern,
  nomatch.error=FALSE,
  engine="PCRE")
match.dt[, input.vec[which(is.na(inside))] ]
match.dt[before != "Rcpp::traits::input_parameter"]

clean <- function(type.vec){
  trans.list <- list(
    comments=function(x)gsub("/[*].*?[*]/", "", x),
    beginning=function(x)sub("^\\s+", "", x),
    end=function(x)sub("\\s+$", "", x),
    after.temp=function(x)gsub("([<>])\\s+", "\\1", x),
    before.temp=function(x)gsub("\\s+([<>])", "\\1", x),
    and=function(x)sub("&$", "", x),
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

match.dt[, cleaned.type := clean(inside)]

(count.dt <- match.dt[, .(
  count=.N,
  packages=length(unique(package))
), by=cleaned.type])
count.dt[order(-count)][1:20]
count.dt[order(-packages)][1:20]

grep("&", count.dt$cleaned.type, value=TRUE)
grep("const", count.dt$cleaned.type, value=TRUE)
grep("Numeric", count.dt$cleaned.type, value=TRUE)

count.dt[grepl("^Rcpp::", cleaned.type) & !grepl("<", cleaned.type)][order(-count)]
