texweave <- function(x, options = list()) UseMethod("texweave", x)

texweave.list <- function(x, options = list()) {
  pieces <- str_c(unlist(lapply(x, texweave, options)), collapse = "")
  str_c("\\begin{alltt}\n", pieces, "\n\\end{alltt}\n", collapse = "")
}

texweave.character <- function(x, options) escape_tex(x)
texweave.source <- function(x, options) escape_tex(line_prompt(x$src))

texweave.warning <- function(x, options) {
  str_c("Warning message: ", escape_tex(x$message), "\n")
}

texweave.message <- function(x, options) {
  message <- str_replace(x$message, "\n$", "")
  str_c("", escape_tex(message), "\n")
}

texweave.error <- function(x, options) {
  str_c("Error: ", escape_tex(x$message), "\n")
}

texweave.recordedplot <- function(x, options) {
  name <- digest(x[[1]])
  save_plot(x, name, options$outdir, width = options$plot_width, 
    height = options$plot_height, dpi = options$dpi)
  image_tex(name, options$tex_height, options$tex_width)
}
