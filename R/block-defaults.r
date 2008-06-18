.defaults <- list(
  outdir = "_include",
  inline = TRUE,
  cache = FALSE,
  gg_width = 4, gg_height = 4
)

set_defaults <- function(code, ...) {
  .defaults <<- list(...)
}


