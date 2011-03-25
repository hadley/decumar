# Useful functions for JSS output

code <- function(x) {
  str_c("\\begin{Code}\n", x, "\\end{Code}\n")
}

code_chunk <- function(x) {
  str_c("\\begin{CodeChunk}\n", x, "\\end{CodeChunk}\n")
}
code_input <- function(x) {
  str_c("\\begin{CodeInput}\n", x, "\\end{CodeInput}\n")
}
code_output <- function(x) {
  str_c("\\begin{CodeOutput}\n", x, "\\end{CodeOutput}\n")
}