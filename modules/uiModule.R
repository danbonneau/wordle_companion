
uiModule <- function(id) {
  
  ns <- NS(id)
  
  div(id = id,
      

      fluidRow(
        dataTableOutput(ns("table")),
        textOutput(ns("text"))
      )
      
      
      
  )
  
}