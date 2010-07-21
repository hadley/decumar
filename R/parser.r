is.comment <- function(x) str_detect(x, "^\\s*%")

is.block <-  function(x)  {
  regexp <- str_c("^\\s*%\\s+(", str_c(blocks, collapse = "|"), ")")
  
  first_lines <- unlist(llply(x, "[", 1))
  str_detect(first_lines, regexp)
}
is.end <-  function(x)  {
  first_lines <- unlist(llply(x, "[", 1))
  str_detect(first_lines, "^\\s*% END")
}
trim <- function(x) gsub("^\\s+|\\s+$", "", x)
indent <- function(x, n = 2) {
  spaces <- ps(rep(" ", length = n))
  ps(spaces, gsub("\n", ps("\n", spaces), x))
}

strip_comment <- function(x) {
  indent <- nchar(strsplit(x[1], "%")[[1]][1])
  
  structure(
    laply(x, function(x) gsub("^\\s*% ?", "", x)),
    indent = indent
  )
}

parse_block <- function(input) {
  block <- strip_comment(input)

  blank <- which(block == "")[1]
  if(is.na(blank)) {
    stop("Parse error: No blank line\n\n", paste(input, collapse="\n"),
      call. = FALSE)
  }
  type <- trim(block[1])
  params <- trim(paste(block[seq(2, blank - 1)], collapse=" "))
  code <- paste(block[seq(blank + 1, length(block))], collapse="\n")
  
  structure(list(
    type = tolower(type),
    params = parse_params(params),
    code = code,
    input = input,
    indent = attr(block, "indent")
  ), class = "block")
}

parse_params <- function(params) {
  loc <- gregexpr("[A-Z-]+: ", params)[[1]]

  breaks <- sort(c(0, loc, loc + attr(loc, "match.length"), nchar(params) + 1))
  pieces <- substr(rep(params, length(breaks)), breaks[-length(breaks)], breaks[-1] - 1)
  pieces <- trim(pieces[pieces != ""])

  even <- seq_along(pieces)
  even <- even[even %% 2 == 0]
  labels <- gsub(":$", "", pieces[even - 1])
  labels <- gsub("-", "_", labels)
  values <- pieces[even]
  names(values) <- tolower(labels)

  lapply(values, type.convert, as.is=TRUE)
}


print.block <- function(x, ...) {
  cat("Block (", x$type, ")\n", sep ="")
  if (length(x$params) > 0) cat("  ", clist(x$params), "\n", sep = "")
  cat("\n", indent(x$code), "\n\n", sep="")
}


parse_file <- function(path) {
  lines <- readLines(path, warn=FALSE)
  grp <- c(0,cumsum(diff(is.comment(lines)) != 0))
  groups <- unname(split(lines, grp))

  blks <- which(is.block(groups))
  ends <- which(is.end(groups))


  blk_with_end <- blks[(blks + 2) %in% ends]
  if (length(blk_with_end) > 0)
    groups <- groups[-c(blk_with_end + 1, blk_with_end + 2)]
  
  groups
}

