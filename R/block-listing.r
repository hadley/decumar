listing <- function(code, ...) {
  ps(
    "\\begin{lstlisting}[language = R]\n",
    code,
    "\\end{lstlisting}"
  )
  # highlight_tex(code)
}
