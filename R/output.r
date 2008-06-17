
process_file <- function(path) {
  lines <- readLines(path, warn=FALSE)
  grp <- c(0,cumsum(diff(is.comment(lines)) != 0))
  groups <- unname(split(lines, grp))

  blks <- which(is.block(groups))
  ends <- which(is.end(groups))


  blk_with_end <- blks[(blks + 2) %in% ends]
  if (length(blk_with_end) > 0)
    groups <- groups[-c(blk_with_end + 1, blk_with_end + 2)]

  blocks <- lapply(groups[is.block(groups)], parse_block)

  ps(ps(laply(groups, group_output, .progress="text"), collapse="\n"), "\n")
}

overwrite_file <- function(path) {
  output <- process_file(path)
  cat(output, file = path)
}