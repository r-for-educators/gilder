shindig <- function(wf, render_type, chunk_name) {
  
  code_text <- attr(wf, "orig_chunk_text")
  
  #chunk_name <- str_extract(code_text, "(?<=r )[^,]*")
  
  code_text <- str_remove_all(code_text, fixed("```")) %>%
    str_remove("\\{[^\\}]*\\}") %>%
    str_remove_all(fixed("\n"))
  
  new_code <- paste0("output$", chunk_name, " <- ", render_type, "({", code_text, "})")
  print(new_code)
  shiny_prerendered_chunk("server", new_code)
  
}

gild <- function(wf, stuff) {
  
  code_text <- attr(wf, "orig_chunk_text")
  
  chunk_header <- code_text %>% str_extract("\\{r.*\\}")
  
  code_text <- code_text %>%
    str_extract("(?<=\\})(.|\\s)*(?=\\`\\`\\`)")
  
  code_text <- code_text %>% str_replace_all(stuff)
  
  code_text <- code_text %>%
    str_replace_all("\\{", "input$") %>%
    str_remove_all("\\}")
  
  code_text <- paste0("```", chunk_header, code_text, "```")
  
  attr(wf, "orig_chunk_text") <- code_text
  
  return(wf)
  
}
