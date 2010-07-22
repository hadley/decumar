#' @param name file name sans extension: assumption is that preamble provides
#'   enough information so tex can discover them itself
image_tex <- function(name, width = NULL, height = NULL, ...) {
  options <- unlist(compact(list(width = width, height = height)))
  options_str <- str_c(names(options), options, sep = " = ", collapse = ", ")
  options_str <- str_c("[", options_str, "]")
  
  str_c("\\includegraphics", options_str, "{", name, "}")
}

#' Escape latex special characters
#' @arguments if true, force latex newlines
#' @seealso \url{http://ebooks.du.ac.in/latex/ltx-164.html}
escape_tex <- function(x, newlines = FALSE) {
  x <- gsub("([\\{}])", "\\\\verb|\\1|", x)
  x <- gsub("([#$%&])", "\\\\\\1", x)
  if (newlines) x <- gsub("\n", " \\\\\\\\ \n", x)
  x
}

# \begin{figure}[htbp]
#   \centering
#     \includegraphics[scale=1]{file}
#   \caption{caption}
#   \label{fig:label}
# \end{figure}
start_figure <- function(position = "htbp", ...) {
  ps(
    "\\begin{figure}[", position, "]\n",
    indent("\\centering")
  )
}
end_figure <- function(caption, label, ...) {
  ps(indent(ps(
    "\\caption{", caption, "}\n",
    "\\label{fig:", label, "}"
  )), "\n\\end{figure}")
}
