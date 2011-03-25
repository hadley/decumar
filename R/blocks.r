blocks <- c("CODE", "CODELISTING", "DEFAULTS", "FIGLISTING", "FIGURE", 
"GRAPHIC", "INTERWEAVE", "LISTING")
# toupper(str_replace(apropos("block_"), "block_", ""))

#' Set document defaults.
block_defaults <- function(code, ..., envir = globalenv()) {
  defaults$set(list(...))
  evaluate(code, envir)
  NULL
}

#' Evaluate code, but don't show it
block_code <- function(code, ..., envir = globalenv()) {
  evaluate(code, envir)
  NULL
}

#' Show code, but don't evaluate it
block_listing <- function(code, prompt = jss, jss = FALSE, ...) {
  if (prompt) {
    src <- parse_all(code)$src
    code <- str_c(unlist(lapply(src, line_prompt)), collapse = "")
  }
  
  if (jss) {
    code(code)
  } else {
    code <- escape_tex(code)
    str_c("\\begin{alltt}\n", code, "\\end{alltt}\n")
  }
}


#' Interweave code and output, as if you had executed at the command line
block_interweave <- function(code, plot_width = 4, plot_height = 4, tex_width = "0.5\\linewidth", tex_height = NULL, dpi = 300, jss = FALSE, ..., envir = globalenv()) {  
  options <- list(dpi = dpi, plot_height = plot_height, 
    plot_width = plot_width, tex_height = tex_height, tex_width = tex_width)  
  
  if (jss) options$escape <- FALSE
  
  pieces <- texweave(evaluate(code, envir), options)
  
  if (jss) {
    type <- vapply(pieces, function(x) attr(x, "type"), character(1))
    
    group <- cumsum(c(T, type[-1] != type[-length(type)]))
    groups <- split(pieces, group)
    groups <- vapply(groups, function(x) {
      text <- str_c(unlist(x), collapse = "")
      switch(attr(x[[1]], "type"), 
        input = code_input(text), output = code_output(text))
    }, character(1))
    
    code_chunk(str_c(groups, collapse = ""))
  } else {
    pieces <- str_c(unlist(pieces), collapse = "")
    str_c("\\begin{alltt}\n", pieces, "\n\\end{alltt}\n", collapse = "")
  }
}



#' Embed a plot.
#' Displays the last plot produced by the code.
block_graphic <- function(code, plot_width = 4, plot_height = 4, tex_width = "0.5\\linewidth", tex_height = NULL, dpi = 300, ..., envir = globalenv()) {
  
  plots <- Filter(is.recordedplot, evaluate(code, envir))
  if (length(plots) == 0) return(NULL)
  
  last_plot <- plots[[length(plots)]]
  texweave(last_plot, list(dpi = dpi, 
    plot_height = plot_height, plot_width = plot_width,
    tex_height = tex_height, tex_width = tex_width))
}

#' Display text output as is (no escaping of special characters)
block_raw <- function(code, ..., envir = globalenv()) {  
  text <- unlist(Filter(is.character, evaluate(code, envir)))
  str_c(text, collapse = "")
}

#' Embed a plot in a floating figure block
#' Display all plots produced by the code.
#' @param col number of columns
block_figure <- function(code, outdir = ".", plot_width = 4, plot_height = 4, tex_width = "0.5\\linewidth", tex_height = NULL, dpi = 300, position = "htbp", caption = NULL, label = NULL, ..., col = 2, envir = globalenv()) {
  
  results <- evaluate(code, envir)
  plots <- Filter(is.recordedplot, evaluate(code, envir))

  if (length(plots) == 0) return()

  names <- unlist(lapply(plots, function(x) digest(x[[1]])))
  l_ply(plots, save_plot, dir = outdir, width = plot_width, 
    height = plot_height, dpi = dpi)

  tex <- unlist(lapply(names, image_tex, height = tex_height, 
    width = tex_width))
  comments <- ifelse(seq_along(tex) %% col != 0, "%", "")
  
  str_c(
    start_figure(position), "\n",
    indent(str_c(tex, comments, collapse = "\n")), "\n",
    end_figure(caption, label)
  )
}

#' Show figure along with the code that produced it
block_figlisting <- function(...) {
  paste(
    listing(...),
    figure(...),
    sep = "\n"
  )
}

#' Evaluate code and display listing separately
block_codelisting <- function(...) {
  code(...)
  listing(...)
}
