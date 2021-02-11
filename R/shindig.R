#' Writes server-side code and runs it in server context
#' 
#' @param x An object of type \code{decorated}
#' @param render_type A string containing the name of a Shiny render function.
#' 
#' @return An object of class \code{decorated}
#' 
#' @importFrom stringr str_c
#' @importFrom purrr map
#' 
#' @export

shindig <- function(x, render_type) {
  
  code_text <- attr(x, "orig_chunk_text")
  chunk_name <- attr(x, "chunk_name")
 
  
  ## Make glue and render code lines for actual code
  
  source_code <- prep_code_string(code_text)
  server_code_s1 <- paste0("codestring_", chunk_name, 
                           "<- ",
                           "reactive({",
                           "glue::glue(",
                           "\"", source_code, "\"",
                           ", .open = '{{', .close = '}}')",
                           "})")
  
  server_code_s2 <- paste0("output$", chunk_name, 
                           "<- ",
                           render_type,
                           "({ ", 
                           "eval(parse(text = ",
                           "codestring_", chunk_name, "()",
                           "))",
                           "})")
  
  
  ## Make glue and render code lines for flaired source code
  
  where_sources <- map(x, ~attr(.x, "class")) == "source"
  flaired_code <- x[where_sources] %>% unlist() %>% str_c(collapse = "<br>")
  
  
  flaired_code <- prep_code_string(flaired_code)
  server_code_f1 <- paste0("flaired_codestring_", chunk_name, 
                           "<- ",
                           "reactive({",
                           "glue::glue(", 
                           "\"", flaired_code, "\"",
                           ", .open = '{{', .close = '}}')",
                           "})")
  
  server_code_f2 <- paste0("output$", chunk_name, "_flaired ", 
                           "<- ",
                           "renderText({ ", 
                           "paste0(",
                           "\"<pre class='r'><code>\", ",
                           "flaired_codestring_", chunk_name, "(), ",
                           "\"</code></pre>\")",
                           "})")
  
  # put everything together
  
  server_code <- paste(server_code_s1, server_code_s2,
                       server_code_f1, server_code_f2,
                       sep = "\n")
  
  
  #cat(server_code)
  shiny_prerendered_chunk("server", server_code)
  
}

#' Helper for shindig
#' 
#' @importFrom stringr str_remove str_remove_all str_replace_all
prep_code_string <- function(code_text){
  
  ## Drop chunk fences
  code_text <- code_text %>%
    str_remove("\\`\\`\\`\\{[^\\}]*\\}") %>%
    str_remove_all(fixed("```")) %>%
    str_remove_all(fixed("\n"))
  
  ## Escape existing quote marks
  code_text <- code_text %>%
    str_replace_all(fixed('"'), '\\\"')

  
  return(code_text)
}