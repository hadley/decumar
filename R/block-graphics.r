graphic <- function(code, ...) {
  woven <- weave(code, parent.frame())  
  
  weave_graphics <- weave_nul
  weave_graphics$value <- function(x, ...) {
    if (!inherits(x, "ggplot")) return()
    save_plot_tex(x, ...)
  }
  
  paste(weave_out(woven, weave_graphics, ...), collapse="\n")
}

save_plot_tex <- function(
  x, outdir, 
  cache = FALSE,
  gg_width = NULL, gg_height = NULL, 
  tex_width = NULL, tex_height = NULL, ...
) {
  path <- file.path(outdir, ps(digest.ggplot(x), ".pdf"))
  
  if (!cache || !file.exists(path)) {
    ggsave(x, filename = path, width = gg_width, height = gg_height)      
  }
  
  ps(indent(image_tex(path, width=tex_width, height=tex_height)), "%")
}

# Include graphics in a latex file
# Given a list of files, this function prints the latex code necessary (ie. includegraphics) to include in the file.
# 
# Note: this function needs to be made generic so that
# it automatically uses the appropriate input text for the
# file type being written to.
# 
# @arguments path to graphics files
# @arguments latex scale option
# @arguments latex height option
# @arguments latex width option
# @keyword documentation 
image_tex <- function(path, width=NULL, height=NULL, ...) {
  options <- compact(list(width = width, height = height))
  options_str <- paste(names(options), options, sep="=", collapse=", ")
	options_str <- paste("[", options_str, "]", sep="")
	
	paste("\\includegraphics", options_str, "{", strip_extension(path), "}", sep="")
}
