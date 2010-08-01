new_defaults <- function() {
  defaults <- list(
    outdir = "_include",
    inline = TRUE,
    cache = FALSE,
    plot_width = 4, plot_height = 4
  )
  
  get <- function() defaults
  set <- function(values) defaults <<- merge(values)
  merge <- function(values) modifyList(defaults, values)
  
  list(get = get, set = set, merge = merge)
}
defaults <- new_defaults()
