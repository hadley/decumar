#' Overwrite input with processed output
#' 
#' @param in_path path to input file
#' @param out_path path where output should be saved.  Defaults to overwriting
#'   input
#' @param reset should result cache be emptied before processing?
#' @export
decumar <- function(in_path, out_path = in_path, clean = FALSE) {
  if (clean) cache$reset()
  output <- process_file(in_path)
  cat(output, file = out_path)
}

#' Process decumar file and return result as string
#' 
#' @param path path to file
#' @export
process_file <- function(path) {
  groups <- parse_file(path)
  results <- unlist(llply(groups, process_group, .progress = "text"))

  str_c(c(results, ""), collapse = "\n")
}

#' Extract code from a decumar file.
#'
#' @param path path to file
#' @param output output file, or \code{""} to print to screen
#' @export
output_code <- function(path, output_path = "") {
  groups <- parse_file(path)
  blocks <- llply(groups[is.block(groups)], parse_block)
  
  code <- llply(blocks, extract_code)
  output <- paste(code, collapse = "\n")

  cat(output, file = output_path)
}

extract_code <- function(block) {
  if (block$code == "NA\n") return("")
  caption <- block$params$caption

  output <- "# ----------------------------\n"
  if (!is.null(caption)) {
    comment <- strwrap(caption, prefix = "# ")
    output <- c(output, comment)
  }

  output <- c(output, block$code, "\n")
  str_c(output, collapse = "\n")
}