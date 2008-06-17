
interweave <- function(code, ...) {
  woven <- weave(code, parent.frame())  
  paste(weave_out(woven, weave_all, ...), collapse="\n")
}

weave_all <- list(
  out = function(x, ...) escape_tex(x),
  src = function(x, ...) escape_tex(line_prompt(x))
)
