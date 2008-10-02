listing <- function(code, ...) {
  ps(
    "\\begin{alltt}\n",
    code,
    "\\end{alltt}\n"
  )
  # highlight_tex(code)
}
# "\\begin{lstlisting}[language = R]\n",
# gsub("\\$", "\\\\$", code),
# "\\end{lstlisting}"
