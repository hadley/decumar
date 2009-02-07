figure <- function(code, ..., col = 2, envir = globalenv()) {
  woven <- weave(code, envir)  

  i <- 0
  weave_figure <- weave_tex
  weave_figure$value <- function(x, ...) {
    i <<- i + 1
    if (!inherits(x, "ggplot")) return()
    # Figures ignore warnings and errors

    indent(save_plot_tex(x, ..., comment = i %% col))
  }
  weave_figure$out <- nul
  weave_figure$src <- nul
  weave_figure$start <- start_figure
  weave_figure$stop <- end_figure
  
  pieces <- weave_out(woven, weave_figure, ...)
  pieces <- pieces[pieces != ""]
  paste(pieces, collapse="\n")
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
  )), "\n\\end{figure}\n")
}