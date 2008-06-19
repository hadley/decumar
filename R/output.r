

process_file <- function(path) {
  groups <- parse_file(path)

  blocks <- lapply(groups[is.block(groups)], parse_block)

  ps(ps(laply(groups, group_output, .progress="text"), collapse="\n"), "\n")
}

overwrite_file <- function(path) {
  output <- process_file(path)
  cat(output, file = path)
}