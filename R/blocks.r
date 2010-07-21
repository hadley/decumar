.defaults <- list(
  outdir = "_include",
  inline = TRUE,
  cache = FALSE,
  gg_width = 4, gg_height = 4
)

#' Set document defaults
set_defaults <- function(code, ..., envir = globalenv()) {
  .defaults <<- defaults(list(...), .defaults)
  woven <- weave(code, envir)  
  ""
}

#' Evaluate code, but don't show it
code <- function(code, ..., envir = globalenv()) {
  woven <- weave(code, envir)  
  ""
}

#' Show code, but don't evaluate it
listing <- function(code, ...) {
  ps(
    "\\begin{alltt}\n",
    escape_tex(code),
    "\\end{alltt}\n"
  )
}

raw <- function(code, ...) {
}

#' Interweave code and output, as if you had executed at the command line
interweave <- function(...) interweave_tex(...)

#' Embed a graphic
graphic <- function(code, ..., envir = globalenv()) {
  woven <- weave(code, envir)  
  
  weave_graphics <- weave_nul
  weave_graphics$value <- function(x, ...) {
    if (!inherits(x, "ggplot")) return()
    save_plot_tex(x, ...)
  }
  
  paste(weave_out(woven, weave_graphics, ...), collapse="\n")
}

#' Embed a graphic in a floating figure block
#' @param col number of columns
figure <- function(code, ..., col = 2, envir = globalenv()) {
  woven <- weave(code, envir)  

  i <- 0
  weave_figure <- weave_tex
  weave_figure$value <- function(x, ...) {
    i <<- i + 1
    if (!inherits(x, "ggplot")) return()
    # Figures ignore warnings and errors

    indent(save_plot_tex(x, ..., comment = i %% col))
  }
  weave_figure$out <- nul
  weave_figure$src <- nul
  weave_figure$start <- start_figure
  weave_figure$stop <- end_figure
  
  pieces <- weave_out(woven, weave_figure, ...)
  pieces <- pieces[pieces != ""]
  paste(pieces, collapse="\n")
}

#' Show figure along with the code that produced it
figlisting <- function(...) {
  paste(
    listing(...),
    figure(...),
    sep = "\n"
  )
}

#' Evaluate code and display listing separately
codelisting <- function(...) {
  code(...)
  listing(...)
}
