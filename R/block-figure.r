figure <- function(code, ..., envir = globalenv()) {
  woven <- weave(code, envir)  
  
  weave_figure <- weave_nul
  weave_figure$value <- function(x, ...) {
    if (!inherits(x, "ggplot")) return()
    save_plot_tex(x, comment = TRUE, ...)
  }
  weave_figure$start <- start_figure
  weave_figure$stop <- end_figure
  
  paste(weave_out(woven, weave_figure, ...), collapse="\n")
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
    "\\centering\n"
  )
}
end_figure <- function(caption, label, ...) {
  ps("\n", indent(ps(
    "\\label{fig:", label, "}\n",
    "\\caption{", caption, "}"
  )), "\n\\end{figure}\n")
}