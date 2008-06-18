
interweave <- function(code, ...) {
  woven <- weave(code, parent.frame())  
  paste(weave_out(woven, weave_all, ...), collapse="\n")
}

weave_all <- list(
  start = function(...) "\\begin{alltt}\n",
  message = function(x, ...) ps("\\{bf", x, "\\}") ,
  warning = function(x, ...) ps("\\{bf WARNING}: ", x) ,
  error = function(x, ...) ps("\\{bf ERROR}: ", x) ,
  out = function(x, ...) escape_tex(x),
  src = function(x, ...) {
    escape_tex(line_prompt(x))
  },
  value = function(x, ...) {
    if (inherits(x, "ggplot")) {
      
      return(save_plot_tex(x, ...))
    }
    escape_tex(capture.output(print(x)))
  },
  stop = function(...) "\\end{alltt}\n"
)

weave_nul <- list(
  start = nul, stop  = nul,
  message = nul, warning = nul, error = nul,
  out = nul, value = nul, src = nul
)

