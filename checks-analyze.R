library(data.table)
example.data <- '
<h3><a href="check_issue_kinds.html">Additional issues</a></h3>
<p>
<a href="https://www.stats.ox.ac.uk/pub/bdr/memtests/clang-ASAN/BuyseTest"><span class="check_ko">clang-ASAN</span></a>
<a href="https://www.stats.ox.ac.uk/pub/bdr/memtests/clang-UBSAN/BuyseTest"><span class="check_ko">clang-UBSAN</span></a>
<a href="https://www.stats.ox.ac.uk/pub/bdr/memtests/gcc-ASAN/BuyseTest"><span class="check_ko">gcc-ASAN</span></a>
<a href="https://www.stats.ox.ac.uk/pub/bdr/memtests/gcc-UBSAN/BuyseTest"><span class="check_ko">gcc-UBSAN</span></a>
<a href="https://www.stats.ox.ac.uk/pub/bdr/memtests/valgrind/BuyseTest"><span class="check_ko">valgrind</span></a>
</p>
'
memtests.pattern <- list(
  '<a href="https://www.stats.ox.ac.uk/pub/bdr/memtests/',
  type='.*?',
  '/')
nc::capture_all_str(example.data, memtests.pattern)

issues.pattern <- list(
  '<h3><a href="check_issue_kinds.html">Additional issues</a></h3>\n<p>',
  issues="(?:.*\n)*?",
  "</p>")
issue.row <- nc::capture_all_str(example.data, issues.pattern)
link.pattern <- list(
  "<a",
  attrs=".*?",
  ">",
  content=".*?",
  "</a>")
nc::capture_all_str(issue.row$issues, link.pattern)

link.type.pattern <- list(
  "<a ",
  nc::field("href", '="', ".*?"),
  '"><span class="check_ko">',
  type=".*?",
  "</span></a>")
nc::capture_all_str(issue.row$issues, link.type.pattern)

check.file.vec <- Sys.glob("checks/*")
type.dt.list <- list()#only memtests
issue.dt.list <- list()# all Additional issues
for(pkg.i in seq_along(check.file.vec)){
  check.file <- check.file.vec[[pkg.i]]
  pkg <- basename(check.file)
  cat(sprintf("%4d / %4d pkg=%s\n", pkg.i, length(check.file.vec), pkg))
  pkg.types <- nc::capture_all_str(check.file, memtests.pattern)
  if(nrow(pkg.types)){
    type.dt.list[[pkg]] <- data.table(
      pkg, pkg.types)
  }
  issue.row <- nc::capture_all_str(check.file, issues.pattern)
  if(nrow(issue.row)){
    pkg.issues <- nc::capture_all_str(issue.row$issues, link.type.pattern)
    issue.dt.list[[pkg]] <- data.table(
      pkg, pkg.issues)
  }
}
type.dt <- do.call(rbind, type.dt.list)
issue.dt <- do.call(rbind, issue.dt.list)
unique(type.dt$pkg)

issue.dt[, .(pkgs=.N), by=type][order(pkgs)]
type.dt[, .(pkgs=.N), by=type][order(pkgs)]
