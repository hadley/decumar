.defaults <- list(
  outdir = "_include",
  inline = FALSE,
  cache = FALSE
)

set_defaults <- function(code, ...) {
  .defaults <<- list(...)
}


