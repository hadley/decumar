

weave_html <- list(
  start = function(...) "<pre>\n",
  message = function(x, ...) ps("<strong>", x, "</strong>"),
  warning = function(x, ...) ps("<strong>Warning: ", x, "</strong>"),
  error = function(x, ...) ps("<strong>Error: ", x, "</strong>"),
  out = function(x, ...) escape_html(x),
  src = function(x, ...) escape_html(line_prompt(x)),
  value = function(x, ...) {
    if (inherits(x, "ggplot")) {
      return(ps(save_plot_html(x, ...), "\n"))
    }
    out <- capture.output(print(x))
    out <- paste(out, collapse="\n")
    out <- paste(out, "\n", sep="")
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
	paste("<img src='", path, "' />", sep="", collapse="\n")
}


save_plot_html <- function(x, outdir, width = 6, height = 4, ...) {
  path <- file.path(outdir, ps(digest.ggplot(x), ".png"))
  try(ggsave(x, filename = path, width = width, height = height))
  
  image_html(path, width=width, height = height)
}
