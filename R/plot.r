#' @returns complete path to saved plot (invisible)
save_plot <- function(plot, name = NULL, dir = NULL, 
  filetype = "pdf", width = NULL, height = NULL, dpi = 300) {
  
  if (is.null(name)) name <- digest(plot[[1]])
  if (is.null(dir)) dir <- getwd()
  
  path <- file.path(dir, str_c(name, ".", filetype))

  open_dev(path, width = width, height = height, dpi = dpi)
  on.exit(dev.off())
  print(plot)
  
  invisible(path)
}

#' Create graphics device from filename
#' @param ... other arguments passed on to graphics device
open_dev <- function(path, width = 5, height = 5, dpi = 300, ...) {
  
  devices <- list(
    bmp  = function(...) bmp(...,  res = dpi, units = "in"),
    eps  = function(...) postscript(..., onefile = FALSE, horizontal = FALSE,
      paper = "special"),
    jpeg = function(...) jpeg(..., res = dpi, units = "in"),
    jpg  = function(...) jpeg(..., res = dpi, units = "in"),
    pdf  = function(...) pdf(...),
    png  = function(...) png(..., res = dpi, units = "in"),
    svg  = function(...) svg(...),
    tex  = function(...) pictex(...),
    tiff = function(...) tiff(..., res = dpi, units = "in"),
    wmf  = function(...) win.metafile(...)
  )

  pieces <- str_split(basename(path), "\\.")[[1]]
  ext <- tolower(pieces[length(pieces)])
  dev <- devices[[ext]]
  
  if (is.null(dev)) {
    stop("Unknown graphics extension", call. = FALSE)
  }
  
  dev(path, width = width, height = height, ...)
}