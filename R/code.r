code <- function(block) {
  if (block$code == "NA\n") return("")
  caption <- block$params$caption

  output <- "# ----------------------------\n"
  if (!is.null(caption)) {
    output <- ps(output, ps(strwrap(caption, prefix = "# "), collapse = "\n"))
  }

  output <- ps(output, block$code, "\n")

  output
}