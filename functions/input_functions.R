


wordInputUI <- function(id, word_id) {
  
  ns <- NS(id)
  
  tagList(
    div(class = "word-button",
        textOutput(paste0("words_left", word_id)) %>%
          tagAppendAttributes(class = "words-left-white"),
                 textInput(inputId = id, label = paste0("Word ", word_id), value = "")  %>%
                    tagAppendAttributes(class = "inline-element"),
                 bsButton(inputId = paste0("go", word_id), "Lock Guess", class = "lockButton")
    ),
    fluidRow(
      column(width = 6, offset = 3,
             splitLayout(
               bsButton(inputId = paste0("l1_w", word_id), label = "", class = "tileButton"),
               bsButton(inputId = paste0("l2_w", word_id), label = "", class = "tileButton"),
               bsButton(inputId = paste0("l3_w", word_id), label = "", class = "tileButton"),
               bsButton(inputId = paste0("l4_w", word_id), label = "", class = "tileButton"),
               bsButton(inputId = paste0("l5_w", word_id), label = "", class = "tileButton")
             )
      )
    ) 
  )
  
}

changeLetterColor <- function(session, input, id) {
  
  observeEvent(input[[id]], {
    
    value <- input[[id]]
    
    if(value > 2) {
      value <- value %% 3
    }
    
    if(value == 1) {
      removeClass(class = "wrongLetter", selector = paste0("#", id))
      addClass(class = "wrongSpot", selector = paste0("#", id))
    }
    else if(value == 2) {
      removeClass(class = "wrongSpot", selector = paste0("#", id))
      addClass(class = "rightSpot", selector = paste0("#", id))
    }
    else {
      removeClass(class = "rightSpot", selector = paste0("#", id))
      addClass(class = "wrongLetter", selector = paste0("#", id))
    }
    
  })
  
}

# -------------------------------- #
# Functions to Evaluate Next Words #
# -------------------------------- #


wrongLetterStore <- function(input, word_data, word_num) {
  
  wrong_vec <- c()
  
  for(i in 1:5) {
    if(input[[paste0("l", i, "_w", word_num)]] %% 3 == 0) {
      wrong_vec <- c(wrong_vec, toupper(str_sub(word_data, i, i)))
    }
  }
  
  wrong_vec <- paste(wrong_vec, collapse = "|")
  
}

wrongPositionStore <- function(input, word_data, word_num) {
  
  wrong_pos <- c()
  
  for(i in 1:5) {
    if(input[[paste0("l", i, "_w", word_num)]] %% 3 == 1) {
      wrong_pos <- c(wrong_pos, toupper(str_sub(word_data, i, i)))
    }
    else {
      wrong_pos <- c(wrong_pos, 1)
    }
  }
  return(wrong_pos)
  
}

# ex. getWordList(input, input$word2, word_list2(), wrong_pos2(), wrong_letters2(), nextWord2(), 2)
getWordList <- function(input, word_input, word_data, wrong_pos, wrong_letters, next_word, word_num) {
  
  words_list <- word_data
  
  keep_wrong_pos <- wrong_pos[!wrong_pos %in% 1]
  
  if(length(keep_wrong_pos) > 0) {
    for(i in 1:length(keep_wrong_pos)) {
      
      words_list <- words_list %>%
        filter(grepl(keep_wrong_pos[i], words))
      
    }
  }
  
  words_list <- words_list %>%
    filter(!grepl(wrong_letters, words)) %>%
    filter(!grepl(wrong_pos[1], l1),
           !grepl(wrong_pos[2], l2),
           !grepl(wrong_pos[3], l3),
           !grepl(wrong_pos[4], l4),
           !grepl(wrong_pos[5], l5)
    )
  
  if(input[[paste0("l", 1, "_w", word_num)]] %% 3 == 2) {
    words_list <- words_list %>%
      filter(l1 == toupper(str_sub(word_input, 1, 1)))
  }
  if(input[[paste0("l", 2, "_w", word_num)]] %% 3 == 2) {
    words_list <- words_list %>%
      filter(l2 == toupper(str_sub(word_input, 2, 2)))
  }
  if(input[[paste0("l", 3, "_w", word_num)]] %% 3 == 2) {
    words_list <- words_list %>%
      filter(l3 == toupper(str_sub(word_input, 3, 3)))
  }
  if(input[[paste0("l", 4, "_w", word_num)]] %% 3 == 2) {
    words_list <- words_list %>%
      filter(l4 == toupper(str_sub(word_input, 4, 4)))
  }
  if(input[[paste0("l", 5, "_w", word_num)]] %% 3 == 2) {
    words_list <- words_list %>%
      filter(l5 == toupper(str_sub(word_input, 5, 5)))
  }

  
  return(words_list)
  
}

# -------------------- #
# Update Button Labels #
# -------------------- #

updateButtonLabels <- function(session, input, input_id) {
  
  updateButton(session, paste0("l1_w", str_sub(input_id, nchar(input_id), nchar(input_id))), 
               label = toupper(str_sub(input[[input_id]], 1, 1)))
  updateButton(session, paste0("l2_w", str_sub(input_id, nchar(input_id), nchar(input_id))), 
               label = toupper(str_sub(input[[input_id]], 2, 2)))
  updateButton(session, paste0("l3_w", str_sub(input_id, nchar(input_id), nchar(input_id))), 
               label = toupper(str_sub(input[[input_id]], 3, 3)))
  updateButton(session, paste0("l4_w", str_sub(input_id, nchar(input_id), nchar(input_id))), 
               label = toupper(str_sub(input[[input_id]], 4, 4)))
  updateButton(session, paste0("l5_w", str_sub(input_id, nchar(input_id), nchar(input_id))), 
               label = toupper(str_sub(input[[input_id]], 5, 5)))
  
}









