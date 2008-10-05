interweave_r <- function(code, ..., envir = globalenv()) {
  woven <- weave(code, envir)  
  
  weave_out(woven, weave_r, ...)
  invisible()
}

weave_r <- list(
  start = sep,
  stop  = sep,
  message = function(x, ...) {
    message(gsub("\n^", "", x))
  },
  warning = function(x, call, ...) {
    message("Warning message:\n", x)
  },
  error = function(x, call, ...) {
    message("Error in ", deparse(call), " : ", x)    
  },
  out = function(x, ...) {
    cat(x)
  },
  value = function(x, path, ...) {
    print(x)
  },
  src = function(x, ...) cat(line_prompt(x), sep="")
)



line_prompt <- function(x, prompt = options("prompt"), continue = options("continue"), ...) {
  lines <- strsplit(x, "\n")[[1]]
  n <- length(lines)

  lines[1] <- paste(prompt, lines[1], sep="")
  if (n > 1)
    lines[2:n] <- paste(continue, lines[2:n], sep="")    
  
  paste(lines, "\n", collapse="")
}

