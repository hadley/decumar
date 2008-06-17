l(plyr)
library(digest)
l(eval.with.details)

FILE <- (function() {
  attr(body(sys.function()), "srcfile")
})()$filename
PATH <- dirname(FILE)

source.with.err <- function(path) {
  tryCatch(source(path), error = function(x) {print(path); print(x)})
}
lapply(dir(file.path(PATH, "R"), full.name=T), source.with.err)
