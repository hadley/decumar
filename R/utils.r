
line_prompt <- function(x, prompt = options("prompt"), continue = options("continue"), ...) {
  lines <- strsplit(x, "\n")[[1]]
  n <- length(lines)

  lines[1] <- paste(prompt, lines[1], sep="")
  if (n > 1)
    lines[2:n] <- paste(continue, lines[2:n], sep="")    
  
  paste(lines, "\n", collapse="")
}
