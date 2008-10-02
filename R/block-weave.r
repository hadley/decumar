
interweave_tex <- function(code, ..., envir = globalenv()) {
  woven <- weave(code, envir)  
  
  strings <- weave_out(woven, weave_tex, ...)
  paste(strings, collapse="")
}

interweave_html <- function(code, ..., envir = globalenv()) {
  woven <- weave(code, envir)  
  
  strings <- weave_out(woven, weave_html, ...)
  paste(strings, collapse="")
}

interweave <- function(code, ..., envir = globalenv()) {
  woven <- weave(code, envir)  
  
  weave_out(woven, weave_r, ...)
  invisible()
}
