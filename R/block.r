process_group <- function(group) {
  if (!is.block(group)[1]) {
    return(str_c(group, collapse = "\n"))
  }
  
  block <- parse_block(group)
  input <- str_c(block$input, collapse = "\n")
  output <- call_block(block)
  end <- indent("\n% END", block$indent)
  
  str_c(input, "\n", output, end)
}

call_block <- function(block) {
  params <- c(list(code = block$code), block$params)
  params <- defaults(params, .defaults)

  # Check cache
  hash <- digest(block)
  if (params$cache && block$type != "set_defaults") {
    if (cache$has_key(hash)) {
      return(cache$get(hash))
    }
  }
  
  block_f <- match.fun(str_c("block_", block$type))
  res <- do.call(block_f, params)
  
  # Block evaluated only for its side effects
  if (res == "") {
    cache$set(hash, "")
    return()
  }

  # If inline, indent and return
  if (params$inline) {
    result <- indent(res, block$indent)
    cache$set(hash, result)
    return(result)
  }
  
  # Otherwise save to file and constuct embedding statement
  outdir <- params$outdir
  if (!file.exists(outdir)) dir.create(outdir, recursive = TRUE)
  path <- file.path(outdir, ps(digest(res), ".tex"))
  cat(res, file = path)
  
  result <- indent(ps("\\input{", path, "}"), block$indent)
  cache$set(hash, result)
  result  
}

