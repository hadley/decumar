ps <- function(..., sep="", collapse="") do.call(paste, compact(list(..., sep=sep, collapse=collapse)))

line_prompt <- function(x, prompt = options("prompt"), continue = options("continue"), ...) {
  lines <- strsplit(x, "\n")[[1]]
  n <- length(lines)

  lines[1] <- paste(prompt, lines[1], sep="")
  if (n > 1)
    lines[2:n] <- paste(continue, lines[2:n], sep="")    
  
  paste(lines, "\n", collapse="")
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
