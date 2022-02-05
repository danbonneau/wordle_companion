

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  changeLetterColor(session, input, "l1_w1")
  changeLetterColor(session, input, "l2_w1")
  changeLetterColor(session, input, "l3_w1")
  changeLetterColor(session, input, "l4_w1")
  changeLetterColor(session, input, "l5_w1")
  
  changeLetterColor(session, input, "l1_w2")
  changeLetterColor(session, input, "l2_w2")
  changeLetterColor(session, input, "l3_w2")
  changeLetterColor(session, input, "l4_w2")
  changeLetterColor(session, input, "l5_w2")
  
  changeLetterColor(session, input, "l1_w3")
  changeLetterColor(session, input, "l2_w3")
  changeLetterColor(session, input, "l3_w3")
  changeLetterColor(session, input, "l4_w3")
  changeLetterColor(session, input, "l5_w3")
  
  changeLetterColor(session, input, "l1_w4")
  changeLetterColor(session, input, "l2_w4")
  changeLetterColor(session, input, "l3_w4")
  changeLetterColor(session, input, "l4_w4")
  changeLetterColor(session, input, "l5_w4")
  
  changeLetterColor(session, input, "l1_w5")
  changeLetterColor(session, input, "l2_w5")
  changeLetterColor(session, input, "l3_w5")
  changeLetterColor(session, input, "l4_w5")
  changeLetterColor(session, input, "l5_w5")
  
  changeLetterColor(session, input, "l1_w6")
  changeLetterColor(session, input, "l2_w6")
  changeLetterColor(session, input, "l3_w6")
  changeLetterColor(session, input, "l4_w6")
  changeLetterColor(session, input, "l5_w6")
  
  word_list1 <- reactive({
    
    df %>%
      mutate(joint_prob = prob1*prob2*prob3*prob4*prob5) %>%
      select(words, starts_with("l"), n, joint_prob) %>%
      arrange(desc(n), desc(joint_prob))
    
  })
  
  nextWord1 <- reactive({
    word <- word_list1() %>%
      slice(1)
  })
  
  observeEvent(input$word1, {
    
    if(nchar(input$word1) != 5) {
      disable(id = "go1")
    }
    else {
      enable(id = "go1")
    }
    
  })
  
  observeEvent(nextWord1(), {
    
    updateTextInput(session, "word1", value = nextWord1()$words) 
    
    
  })
  
  observeEvent(input$go1, {
    
    updateButtonLabels(session, input, "word1")
    disable(id = "word1")
    
  })
  
  wrong_letters1 <- reactive({
    
    wrongLetterStore(input, input$word1, 1)

  })
  
  wrong_pos1 <- reactive({
    
    wrongPositionStore(input, input$word1, 1)
    
  })
  
  word_list2 <- reactive({
  
    getWordList(input, input$word1, word_list1(), wrong_pos1(), wrong_letters1(), nextWord1(), 1)
  
  })
  
  # ----------------------- #
  # Word 2 Server-Side Code #
  # ----------------------- #
  
  nextWord2 <- reactive({
    
    if(input$go1 > 0) {
      word_list2() %>%
        slice(1) 
    }
    
  })
  
  observeEvent(nextWord2(), {
    
    updateTextInput(session, "word2", value = nextWord2()$words) 

    
  })
  
  observeEvent(input$word2, {
    
    if(nchar(input$word2) != 5) {
      disable(id = "go2")
    }
    else {
      enable(id = "go2")
    }
    
  })
  
  observeEvent(input$go2, {
    
    updateButtonLabels(session, input, "word2")
    disable(id = "word2")
    
  })
  
  wrong_letters2 <- reactive({
    
    wrongLetterStore(input, input$word2, 2)

  })
  
  wrong_pos2 <- reactive({
    
    wrongPositionStore(input, input$word2, 2)
    
  })
  
  # ----------------------- #
  # Word 3 Server-Side Code #
  # ----------------------- #
  
  word_list3 <- reactive({
    
    getWordList(input, input$word2, word_list2(), wrong_pos2(), wrong_letters2(), nextWord2(), 2)
    
  })
  
  nextWord3 <- reactive({
    
    if(input$go2 > 0) {
      word_list3() %>%
        slice(1) 
    }
    
  })
  
  observeEvent(input$word3, {
    
    if(nchar(input$word3) != 5) {
      disable(id = "go3")
    }
    else {
      enable(id = "go3")
    }
    
  })
  
  observeEvent(nextWord3(), {
    
    updateTextInput(session, "word3", value = nextWord3()$words)
    
  })
  
  observeEvent(input$go3, {

    updateButtonLabels(session, input, "word3")
    disable(id = "word3")
    
  })
  
  wrong_letters3 <- reactive({
    
    wrongLetterStore(input, input$word3, 3)
    
  })
  
  wrong_pos3 <- reactive({
    
    wrongPositionStore(input, input$word3, 3)
    
  })
  
  # ----------------------- #
  # Word 4 Server-Side Code #
  # ----------------------- #
  
  word_list4 <- reactive({
    
    getWordList(input, input$word3, word_list3(), wrong_pos3(), wrong_letters3(), nextWord3(), 3)

  })
  
  nextWord4 <- reactive({
    
    if(input$go3 > 0) {
      word_list4() %>%
        slice(1) 
    }
    
  })
  
  observeEvent(input$word4, {
    
    if(nchar(input$word4) != 5) {
      disable(id = "go4")
    }
    else {
      enable(id = "go4")
    }
    
  })
  
  observeEvent(nextWord4(), {
    
    updateTextInput(session, "word4", value = nextWord4()$words)
    
  })
  
  observeEvent(input$go4, {
    
    updateButtonLabels(session, input, "word4")
    disable(id = "word4")
    
  })
  
  wrong_letters4 <- reactive({
    
    wrongLetterStore(input, input$word4, 4)
    
  })
  
  wrong_pos4 <- reactive({
    
    wrongPositionStore(input, input$word4, 4)
    
  })
  
  # ----------------------- #
  # Word 5 Server-Side Code #
  # ----------------------- #
  
  word_list5 <- reactive({
    
    getWordList(input, input$word4, word_list4(), wrong_pos4(), wrong_letters4(), nextWord4(), 4)
    
  })
  
  nextWord5 <- reactive({
    
    if(input$go4 > 0) {
      word_list5() %>%
        slice(1)
    }

  })
  
  observeEvent(input$word5, {
    
    if(nchar(input$word5) != 5) {
      disable(id = "go5")
    }
    else {
      enable(id = "go5")
    }
    
  })
  
  observeEvent(nextWord5(), {
    
    updateTextInput(session, "word5", value = nextWord5()$words)
    
  })
  
  observeEvent(input$go5, {
    
    updateButtonLabels(session, input, "word5")
    disable(id = "word5")
    
  })
  
  wrong_letters5 <- reactive({
    
    wrongLetterStore(input, input$word5, 5)
    
  })
  
  wrong_pos5 <- reactive({
    
    wrongPositionStore(input, input$word5, 5)
    
  })

  # ----------------------- #
  # Word 6 Server-Side Code #
  # ----------------------- #
  
  word_list6 <- reactive({
    
    getWordList(input, input$word5, word_list5(), wrong_pos5(), wrong_letters5(), nextWord5(), 5)
    
  })
  
  nextWord6 <- reactive({
    
    if(input$go5 > 0) {
      word_list6() %>%
        slice(1)
    }

  })
  
  observeEvent(input$word6, {
    
    if(nchar(input$word6) != 5) {
      disable(id = "go6")
    }
    else {
      enable(id = "go6")
    }
    
  })
  
  observeEvent(nextWord6(), {
    
    updateTextInput(session, "word6", value = nextWord6()$words)
    
  })
  
  observeEvent(input$go6, {
    
    updateButtonLabels(session, input, "word6")
    disable(id = "word6")
    
  })
  
  wrong_letters6 <- reactive({
    
    wrongLetterStore(input, input$word6, 6)
    
  })
  
  wrong_pos6 <- reactive({
    
    wrongPositionStore(input, input$word6, 6)
    
  })
  
  # ---------------------------------- #
  # Table for Word List Options Output #
  # ---------------------------------- #
  
  
  output$table <- renderDataTable({
    
    if(input$go1 == 0) {
      list_of_words <- word_list1() %>%
        select(-joint_prob)
    }
    else if(input$go2 == 0) {
      list_of_words <- word_list2() %>%
        select(-joint_prob)
    }
    else if(input$go3 == 0) {
      list_of_words <- word_list3() %>%
        select(-joint_prob)
    }
    else if(input$go4 == 0) {
      list_of_words <- word_list4() %>%
        select(-joint_prob)
    }
    else if(input$go5 == 0) {
      list_of_words <- word_list5() %>%
        select(-joint_prob)
    }
    else {
      list_of_words <- word_list6() %>%
        select(-joint_prob)
    }
    
    datatable(
      list_of_words,
      colnames = list(
        "Word",
        "Letter 1",
        "Letter 2",
        "Letter 3",
        "Letter 4",
        "Letter 5",
        "# Unique Letters"
      ),
      rownames = F,
      options = list(
        dom = "ft",
        columnDefs = list(list(className = 'dt-center', targets ="_all"))
      )
    )
    
  })
  
  output$words_left1 <- renderText({
    
    paste0("Possible Words: ", scales::comma(nrow(word_list1())))
    
  })
  
  output$words_left2 <- renderText({
    
    paste0("Possible Words: ", scales::comma(nrow(word_list2())))
    
  })
  
  output$words_left3 <- renderText({
    
    paste0("Possible Words: ", scales::comma(nrow(word_list3())))
    
  })
  
  output$words_left4 <- renderText({
    
    paste0("Possible Words: ", scales::comma(nrow(word_list4())))
    
  })
  
  output$words_left5 <- renderText({
    
    paste0("Possible Words: ", scales::comma(nrow(word_list5())))
    
  })
  
  output$words_left6 <- renderText({
    
    paste0("Possible Words: ", scales::comma(nrow(word_list6())))
    
  })

  
})
