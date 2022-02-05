
library(shinydashboard)
library(shiny)
library(shinyWidgets)
library(dplyr)
library(DT)
library(shinyBS)
library(sjmisc)
library(shinyjs)
library(shinythemes)
source("modules/uiModule.R")
source("modules/serverModule.R")
source("functions/input_functions.R")

header <- dashboardHeader(title = "Wordle Solver")

sidebar <- dashboardSidebar(disable = T)

body <- dashboardBody(
  useShinyjs(),
  wordInputUI("word1", 1),
  br(),
  wordInputUI("word2", 2),
  br(),
  wordInputUI("word3", 3),
  br(),
  wordInputUI("word4", 4),
  br(),
  wordInputUI("word5", 5),
  br(),
  wordInputUI("word6", 6),
  
  dataTableOutput("table")      
  
  
)

ui <- dashboardPage(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    skin = "black",
    header = header,
    sidebar = sidebar,
    body = body
)

