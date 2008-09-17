
interweave <- function(code, ..., envir = globalenv()) {
  woven <- weave(code, envir)  
  
  strings <- weave_out(woven, weave_all, ...)
  paste(strings, collapse="")
}

weave_all <- list(
  start = function(...) "\\begin{alltt}\n",
  message = function(x, ...) ps("{\\bf", x, "\\}") ,
  warning = function(x, ...) ps("WARNING: ", x, "\n") ,
  error = function(x, ...) ps("ERROR: ", x, "\n") ,
  out = function(x, ...) escape_tex(x),
  src = function(x, ...) escape_tex(line_prompt(x)),
  value = function(x, ...) {
    if (inherits(x, "ggplot")) {
      return(ps(save_plot_tex(x, ...), "\n"))
    }
    out <- capture.output(print(x))
    out <- paste(out, collapse="\n")
    out <- paste(out, "\n", sep="")
    escape_tex(out)
  },
  stop = function(...) "\\end{alltt}\n"
)

weave_nul <- list(
  start = nul, stop  = nul,
  message = nul, warning = nul, error = nul,
  out = nul, value = nul, src = nul
)

