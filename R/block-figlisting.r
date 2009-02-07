figlisting <- function(...) {
  paste(
    listing(...),
    figure(...),
    sep = "\n"
  )
}