interweave_html <- function(code, ..., envir = globalenv()) {
  woven <- weave(code, envir)  
  
  strings <- weave_out(woven, weave_html, ...)
  paste(strings, collapse="")
}

weave_html <- list(
  start = function(...) "<pre>\n",
  message = function(x, ...) ps("<strong>", x, "</strong>\n"),
  warning = function(x, ...) ps("<strong>Warning: ", gsub("\n$", "", x), "</strong>\n"),
  error = function(x, ...) ps("<strong>Error: ", x, "</strong>\n"),
  out = function(x, ...) escape_html(x),
  src = function(x, ...) line_prompt(highlight_html(x), "&gt; "),
  value = function(x, ...) {
    if (inherits(x, "ggplot")) {
      details <- eval.with.details(expression(save_plot_html(x, ...)))
      
      out <- weave_out_output(details$output, weave_html)
      return(paste(out, details$value, "\n", collapse=""))
    }
    out <- capture.output(print(x))
    out <- paste(out, collapse="\n")
    out <- paste(out, "\n", sep=" ")
    escape_html(out)
  },
  stop = function(...) "</pre>\n"
)


# Escape html special characters
# This function escapes < and > so that they don't cause problems in your html file
# 
# @arguments character string to escape
# @arguments if true, make html newlines
# @keyword misc
escape_html <- function(x, newlines = FALSE) {
  x <- gsub(">", "&gt;", gsub("<", "&lt;", x))
  if (newlines) x <- gsub("\n", " <br />\n", x)
  x
}

highlight_html <- function(x) {
  ps(system("highlight --syntax r --quiet --wrap-simple -f", intern=TRUE, input=x), collapse="\n")
}


# Include graphics for html
# Uses <img src="" /> to include an image into a html file
#
# @keyword internal
image_html <- function(path, width=NULL, height=NULL, ...) {
  paste(
    "<img src='", path, "' ", 
          "width = '", width, "' ", 
          "height = '", height, "' ", 
          "alt = '' />", 
    sep="", collapse="\n")
}


save_plot_html <- function(x, outdir = NULL, width = 7, height = 5, cache = FALSE, ...) {
  path <- file.path(outdir, ps(digest.ggplot(x), ".png"))
  
  # if (!is.null(outdir) && (!cache || !file.exists(path))) {
  ggsave(x, filename = path, width = width, height = height, dpi = 72)
  # }
  
  image_html(path, width = width * 72, height = height * 72)
}
