.defaults <- list(
  outdir = "_include",
  inline = TRUE,
  cache = FALSE,
  gg_width = 4, gg_height = 4
)

set_defaults <- function(code, ..., envir = globalenv()) {
  .defaults <<- defaults(list(...), .defaults)
  woven <- weave(code, envir)  
  ""
}


code <- function(code, ..., envir = globalenv()) {
  woven <- weave(code, envir)  
  ""
}

listing <- function(code, ...) {
  ps(
    "\\begin{alltt}\n",
    escape_tex(code),
    "\\end{alltt}\n"
  )
}

raw <- function(code, ...) {
}

interweave <- function(...) interweave_tex(...)

graphic <- function(code, ..., envir = globalenv()) {
  woven <- weave(code, envir)  
  
  weave_graphics <- weave_nul
  weave_graphics$value <- function(x, ...) {
    if (!inherits(x, "ggplot")) return()
    save_plot_tex(x, ...)
  }
  
  paste(weave_out(woven, weave_graphics, ...), collapse="\n")
}

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

figlisting <- function(...) {
  paste(
    listing(...),
    figure(...),
    sep = "\n"
  )
}

codelisting <- function(...) {
  code(...)
  listing(...)
}
