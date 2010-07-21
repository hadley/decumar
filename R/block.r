block_call <- function(block) {
  params <- c(list(code = block$code), block$params)
  params <- defaults(params, .defaults)

  # Check cache
  hash <- digest(block)
  if (params$cache && block$type != "set_defaults") {
    hit <- cache_get(hash)
    if (!is.null(hit)) return(hit)
  }
  
  res <- do.call(block$type, params)
  
  # Block evaluated only for its side effects
  if (res == "") {
    cache_set(hash, "")
    return()
  }

  # If inline, indent and return
  if (params$inline) {
    result <- indent(res, block$indent)
    cache_set(hash, result)
    return(result)
  }
  
  # Otherwise save to file and constuct embedding statement
  outdir <- params$outdir
  if (!file.exists(outdir)) dir.create(outdir, recursive = TRUE)
  path <- file.path(outdir, ps(digest(res), ".tex"))
  cat(res, file = path)
  
  result <- indent(ps("\\input{", path, "}"), block$indent)
  cache_set(hash, result)
  result  
}


block_output <- function(block) {
  input <- ps(block$input, collapse = "\n")
  output <- block_call(block)
  end <- indent("\n% END", block$indent)
  
  ps(input, "\n", output, end)
}

group_output <- function(group) {
  if (is.block(group)[1]) {
    block_output(parse_block(group))
  } else {
    ps(group, collapse="\n")
  }
}
