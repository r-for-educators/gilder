#' Flairs the gildies
#' 
#' @export

flair_gilded <- function(x, name, ...){
  
  make_rx <- paste0("\\{\\{[^\\}]*", name, "\\}\\}")
  
  flair::flair_rx(x, make_rx, ...)
  
}

#' Flairs all the gildies
#' 
#' @export

flair_gilded_all <- function(x, ...){
  
  flair::flair_rx(x, "\\{\\{[^\\}]*\\}\\}", ...)
  
}