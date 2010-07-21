sep <- function(...) cat(rep("-", options("width")), sep = "")
nul <- function(...) {}

weave <- function(input, envir = parent.frame(), enclos = NULL) {  
  parsed <- parse_all(input)
  
  evaluate <- function(expr, src) {
    eval.with.details(expr, envir = envir, enclos = enclos, src = src)
  }
  
  structure(
    lapply(1:nrow(parsed), function(i) {
      x <- with(parsed[i,], evaluate(expr[[1]], src[[1]]))
      
      # Ugly hack to save theme state 
      if (inherits(x$value, "ggplot")) {
        x$value$options <- plot_theme(x$value)
      }
      x
    }),
    class = "ewd-list"
  )
}

weave_out <- function(x, format, ...) {
  c(
    format$start(...),
    sapply(x, function(x) weave_out_single(x, format, ...)),
    format$stop(...)
  )
}

weave_out_single <- function(x, f, ...) {
  out <- f$src(x$src)

  out <- paste(out, weave_out_output(x$output, f, ...), sep="")
  
  if(!is.null(x$visible) && x$visible) {
    out <- paste(out, f$value(x$value, ...), sep="")
  }
  out
}

weave_out_output <- function(output, f, ...) {
  paste(lapply(output, function(x) {
    if (inherits(x, "message")) {
      f$message(x$message, ...)
    } else if (inherits(x, "warning")) {
      f$warning(x$message, x$call, ...)
    } else if (inherits(x, "error")) {
      f$error(x$message, x$call, ...)
    } else {
      f$out(x, ...)
    }
  }), collapse = "", sep="")
}

"print.ewd-list" <- function(x, ...) {
  invisible(weave_out(x, weave_r))  
}

weave_nul <- list(
  start = nul, stop  = nul,
  message = nul, warning = nul, error = nul,
  out = nul, value = nul, src = nul
)

