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
