#' Writes server-side code and runs it in server context
#' 
#' @param x An object of type \code{decorated}
#' @param render_type A string containing the name of a Shiny render function.
#' 
#' @return An object of class \code{decorated}

shindig <- function(x, render_type) {
  
  code_text <- attr(x, "orig_chunk_text")
  chunk_name <- attr(x, "chunk_name")
  
  code_text <- str_remove_all(code_text, fixed("```")) %>%
    str_remove("\\{[^\\}]*\\}") %>%
    str_remove_all(fixed("\n"))
  
  new_code <- paste0("output$", chunk_name, " <- ", render_type, "({", code_text, "})")

  shiny_prerendered_chunk("server", new_code)
  
}