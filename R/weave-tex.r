interweave_tex <- function(code, ..., envir = globalenv()) {
  woven <- weave(code, envir)  
  
  strings <- weave_out(woven, weave_tex, ...)
  paste(strings, collapse="")
}

weave_tex <- list(
  start = function(...) "\\begin{alltt}\n",
  message = function(x, ...) ps("{\\bf", escape_tex(x), "\\}") ,
  warning = function(x, ...) ps("WARNING: ", escape_tex(x), "\n") ,
  error = function(x, ...) ps("ERROR: ", escape_tex(x), "\n") ,
  out = function(x, ...) escape_tex(x),
  src = function(x, ...) escape_tex(line_prompt(x)),
  value = function(x, ...) {
    if (inherits(x, "ggplot")) {
      return(ps(save_plot_tex(x, ...), "\n"))
    }
    out <- capture.output(print(x))
    out <- paste(out, collapse="\n")
    out <- paste(out, "\n", sep="")
    escape_tex(out)
  },
  stop = function(...) "\\end{alltt}\n"
)

# Escape latex special characters
# This function escapes special latex characters so they don't cause problems in your latex file
# 
# @arguments character string to escape
# @arguments if true, force latex newlines
# @keyword misc
# @seealso \url{http://ebooks.du.ac.in/latex/ltx-164.html}
escape_tex <- function(x, newlines = FALSE) {
  x <- gsub("\\\\", "$\\backslash$", x)
  x <- gsub("([#$%&_{}])", "\\\\\\1", x)
  if (newlines) x <- gsub("\n", " \\\\\\\\ \n", x)
  x
}

save_plot_tex <- function(
  x, outdir, comment = FALSE,
  cache = FALSE, filetype = "pdf",
  gg_width = NULL, gg_height = NULL, 
  tex_width = NULL, tex_height = NULL, ...
) {
  path <- file.path(outdir, ps(digest.ggplot(x), ".", filetype))
  
  if (!cache || !file.exists(path)) {
    try(ggsave(x, filename = path, width = gg_width, height = gg_height))
  }
  
  out <- indent(image_tex(path, width=tex_width, height=tex_height))
  if (comment) out <- ps(out, "%")
  out
}

highlight_tex <- function(x) {
  ps(system("highlight -L --quiet --syntax r -f", intern=TRUE, input=x), collapse="\n")
}


# Include graphics in a latex file
# Given a list of files, this function prints the latex code necessary (ie. includegraphics) to include in the file.
# 
# Note: this function needs to be made generic so that
# it automatically uses the appropriate input text for the
# file type being written to.
# 
# @arguments path to graphics files
# @arguments latex scale option
# @arguments latex height option
# @arguments latex width option
# @keyword documentation 
image_tex <- function(path, width=NULL, height=NULL, ...) {
  options <- compact(list(width = width, height = height))
  options_str <- paste(names(options), options, sep="=", collapse=", ")
  options_str <- paste("[", options_str, "]", sep="")
  
  paste("\\includegraphics", options_str, "{", strip_extension(path), "}", sep="")
}

strip_extension <- function(x) {
  gsub("\\..*$", "", x)
}