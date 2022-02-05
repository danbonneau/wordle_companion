
serverModule <- function(id, data = df) {
  
  moduleServer(id, function(input, output, session) {
    
    word_select <- reactive({
      
      data %>%
        mutate(joint_prob = prob1*prob2*prob3*prob4*prob5) %>%
        select(words, starts_with("l"), n, joint_prob) %>%
        arrange(desc(n), desc(joint_prob)) %>%
        slice(1)
      
    })
  
    
    onclick("letter1_guess", updateTextInput(session, "letter1_guess", value = "."))
    # updateTextInput(session, inputId = "letter2", value = word_select()$l2)
    # updateTextInput(session, inputId = "letter3", value = word_select()$l3)
    # updateTextInput(session, inputId = "letter4", value = word_select()$l4)
    # updateTextInput(session, inputId = "letter5", value = word_select()$l5)
    
    output$table <- renderDataTable({
      head(word_select())
    })
    
    output$text <- renderText({
      paste0("Input: ", input$letter1_guess)
    })
    
  })
  
}