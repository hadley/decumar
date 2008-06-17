block_types <- toupper(c(
  "defaults", # Set up default parameters for the remainder of the file
  "figure",   # Insert a floating figure containing graphics
  "graphic",  # Insert a graphic into the document
  "tabular",  # Insert a table
  "table",    # Insert a floating table containing data
  "output",   # Include output 
  "raw",      # Include output (unescaped) 
  "listing",  # Pretty print code
  "interweave"     # Output and listing interwoven
))


block_call <- function(block) {
  params <- c(list(code = block$code), block$params)
  params <- reshape::defaults(params, .defaults)
  
  res <- do.call(block$type, params)
  if (params$inline) return(indent(res, block$indent))
  
  outdir <- params$outdir
  if (!file.exists(outdir)) dir.create(outdir, recursive = TRUE)
  path <- file.path(outdir, ps(digest(res), ".tex"))
  cat(res, file = path)
  indent(ps("\\input{", path, "}"), block$indent)
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
