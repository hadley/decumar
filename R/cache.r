.create_cache <- function() {
  cache <- list()
  
  get <- function(x) cache[[x]]
  set <- function(x, value) cache[[x]] <<- value
  ls <- function(x) names(cache)
  clear <- function(x) cache <<- list()
  
  list(get=get, set=set, ls=ls, clear=clear)
}

if (!exists("cache_set") || is.null(cache_set)) {
  cache <- .create_cache()
  cache_get <- cache$get
  cache_set <- cache$set
  cache_ls <- cache$ls
  cache_clear <- cache$clear
}