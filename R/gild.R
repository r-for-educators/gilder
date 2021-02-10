#' Replaces parts of code with interactive Shiny inputs
#' 
#' @param x An object of class \code{something}
#' @param ... Named strings telling which parts of the source code to replace 
#' with shiny variables.
#' 
#' @return An object of class \code{something}
#' 
#' @import stringr
#' 
#' @export

gild <- function(x, ...) {
  
  replacements = unlist(rlang::list2(...))
  
  code_text <- attr(x, "orig_chunk_text")
  
  # Set aside chunk boundaries for later
  chunk_header <- code_text %>% str_extract("\\{r.*\\}")
  
  # Edit code text to prep for shinyness
  code_text <- code_text %>%
    str_extract("(?<=\\})(.|\\s)*(?=\\`\\`\\`)") %>%   # pull out chunk text
    str_replace_all(replacements) %>%                 # gild things
    str_replace_all("\\{", "input$") %>%              # add input$ to variables
    str_remove_all("\\}")
  
  
  # Rebuild chunk
  code_text <- paste0("```", chunk_header, code_text, "```")
  
  attr(x, "orig_chunk_text") <- code_text
  
  return(x)
  
}
