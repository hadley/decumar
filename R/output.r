

process_file <- function(path) {
  groups <- parse_file(path)

  blocks <- llply(groups[is.block(groups)], parse_block)

  ps(ps(laply(groups, group_output, .progress="text"), collapse="\n"), "\n")
}

overwrite_file <- function(path) {
  output <- process_file(path)
  cat(output, file = path)
}

output_code <- function(input, output_path = "") {
  groups <- parse_file(input)
  blocks <- llply(groups[is.block(groups)], parse_block)
  
  code <- llply(blocks, code)
  output <- paste(code, collapse = "\n")

  cat(output, file = output_path)
}