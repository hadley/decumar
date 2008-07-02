listing <- function(code, ...) {
  ps(
    "\\begin{lstlisting}[language = R]\n",
    gsub("\\$", "\\\\$", code),
    "\\end{lstlisting}"
  )
  # highlight_tex(code)
}
