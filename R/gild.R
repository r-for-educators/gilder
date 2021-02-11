#' Replaces parts of code with interactive Shiny inputs
#' 
#' @param x An object of class \code{something}
#' @param ... Named strings telling which parts of the source code to replace 
#' with shiny variables.
#' 
#' @return An object of class \code{something}
#' 
#' @importFrom stringr str_extract
#' 
#' @export

gild <- function(x, ...) {
  
  replacements <- unlist(rlang::list2(...))
  
  repls <- paste0("{", names(replacements), "}")
  
  replacements <- setNames(repls, replacements)

  
  code_text <- attr(x, "orig_chunk_text")
  
  # Set aside chunk boundaries for later
  chunk_header <- code_text %>% str_extract("\\{r.*\\}")
  
  # Edit code text to prep for shinyness
  code_text <- code_text %>%
    str_extract("(?<=\\})(.|\\s)*(?=\\`\\`\\`)") %>%   # pull out chunk text
    gild_code(replacements)
  
  
  # Rebuild chunk
  code_text <- paste0("```", chunk_header, code_text, "```")
  
  attr(x, "orig_chunk_text") <- code_text
  
  ## Now do the flaired code
  where_sources <- purrr::map(x, ~attr(.x, "class")) == "source"
  
  x[where_sources] <- purrr::map(x[where_sources], ~gild_code(.x, replacements))
  
  x[where_sources] <- purrr::map(x[where_sources],
                                 function(x) structure(list(src = x), class = "source"))
  
  return(x)
  
}

#' Helper for gild
#' 
#' @importFrom stringr str_replace_all
#' 
#' 
gild_code <- function(code_text, replacements) {
  
  code_text <- code_text %>%
    str_replace_all(replacements) %>%                 # gild things
    str_replace_all("\\{", "{{input$") %>%              # add input$ to variables
    str_replace_all("\\}", "}}")     # fences for glue later
  
}

