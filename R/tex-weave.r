#' Weave evaluate output into tex.
#' 
#' @param x evaluate output
#' @param options list of options used to control output
#' @export
#' @S3method texweave list
#' @S3method texweave character
#' @S3method texweave source
#' @S3method texweave warning
#' @S3method texweave message
#' @S3method texweave error
#' @S3method texweave recordedplot
texweave <- function(x, options = list()) {
  options <- modifyList(list(escape = TRUE), options)
  UseMethod("texweave", x)
}

r_input <- function(x) structure(x, type = "input")
r_output <- function(x) structure(x, type = "output")

texweave.list <- function(x, options = list()) {
  lapply(x, texweave, options)
}

texweave.character <- function(x, options) {
  if (options$escape) x <- escape_tex(x)
  r_output(x)
}
texweave.source <- function(x, options) {
  src <- line_prompt(x$src)
  if (options$escape) src <- escape_tex(src)
  
  r_input(src)
}

texweave.warning <- function(x, options) {
  msg <- if (options$escape) escape_tex(x$message) else x$message
  
  r_output(str_c("Warning message: ", msg, "\n"))
}

texweave.message <- function(x, options) {
  message <- str_replace(x$message, "\n$", "")
  if (options$escape) message <- escape_tex(message)

  r_output(str_c(message, "\n"))
}

texweave.error <- function(x, options) {
  msg <- if (options$escape) escape_tex(x$message) else x$message
  r_output(str_c("Error: ", msg, "\n"))
}

texweave.recordedplot <- function(x, options) {
  name <- digest(x[[1]])
  save_plot(x, name, options$outdir, width = options$plot_width, 
    height = options$plot_height, dpi = options$dpi)
  r_output(image_tex(name, options$tex_height, options$tex_width))
}