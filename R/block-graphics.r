graphic <- function(code, ..., envir = globalenv()) {
  woven <- weave(code, envir)  
  
  weave_graphics <- weave_nul
  weave_graphics$value <- function(x, ...) {
    if (!inherits(x, "ggplot")) return()
    save_plot_tex(x, ...)
  }
  
  paste(weave_out(woven, weave_graphics, ...), collapse="\n")
}

