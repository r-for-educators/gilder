
#' Builds a \code{\link{decorated}} object from a code chunk 
#'
#' (Aliased from \code{flair}; see that package for further use.)
#'
#' @param x A string, containing a chunk label.
#' @param ... Chunk options to pass along
#'
#' @return A \code{decorated} object.
#'
#' @export
decorate_shiny <- function(x, ...) {
  
  flair::decorate(x,  eval = FALSE, ...)
  
}