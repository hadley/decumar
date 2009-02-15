extract_code <- function(block) {
  if (block$code == "NA\n") return("")
  caption <- block$params$caption

  # output <- "# ----------------------------\n"
  output <- ""
  if (!is.null(caption)) {
    output <- ps(output, ps(strwrap(caption, prefix = "# "), collapse = "\n"))
    output <- paste(output, "\n", sep="")
  }

  output <- ps(output, block$code, "\n")

  output
}