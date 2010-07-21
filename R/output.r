process_file <- function(path) {
  groups <- parse_file(path)

  blocks <- llply(groups[is.block(groups)], parse_block)

  ps(ps(laply(groups, process_group, .progress="text"), collapse="\n"), "\n")
}

overwrite_file <- function(path, complete = FALSE) {
  if (complete) cache$reset()
  output <- process_file(path)
  cat(output, file = path)
}

output_code <- function(input, output_path = "") {
  groups <- parse_file(input)
  blocks <- llply(groups[is.block(groups)], parse_block)
  
  code <- llply(blocks, extract_code)
  output <- paste(code, collapse = "\n")

  cat(output, file = output_path)
}

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