gilderOutput <- function(wf_obj, id, outputType){
  
  ns <- NS(id)
  
  input$code <- 
  
  tagList(
    
    htmlOutput(ns("source_code")),
    
    do.call(outputType,
            list(ns(wf_obj$chunk_name)))
    
  )
  
  
}




moduleServer <- function(id, module) {
  callModule(module, id)
}



gilderServer <- function(id, chunk_name, render_function){
  # do stuff here
  
  moduleServer(
    id,
    function(input, output, session) {
      
     wf_obj <- reactive(input[[chunk_name]])
     
     code_plain <- reactive(wf_obj)$code_text
     code_flaired <- reactive(wf_obj)$flaired_code
     
     
     output[[chunk_name]] <- do.call(render_function,
                                     list({eval(parse(text = code_plain()))})
                                     )
      
      output$source_code <- renderText({
        
        code_flaired()
        
      })
  )
}
