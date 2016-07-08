#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# server is at:   https://goldin.shinyapps.io/Search_Pepys/

library(shiny)
#library(readr)
library(lubridate)
library(plyr)
library(dplyr)
library(purrr)
library(stringi)
library(stringr)
library(scales)
library(ggplot2)
library(DT)
#library(tidytext)
#library(tokenizers)
#library(devtools)

load("entry_df.RData")
source("for_app.R")

starting_search <- "Duke of York"
sample_search <- c("dancing", "Greenwich", "by water", "sat late", "Valentine",
                   "fire", "Brampton", "Cambridge", "horse", "church", "sermon",
                   "the king", "took coach", "morning dra", "and so to bed",
                   "betime", "Castlem", "my father", "my brother", "Hewer",
                   "dead", "plague", "play", "my coach", "play-*house","coffee",
                   'there saw "', "W\\. Pen", "God", "with great pleasure",
                   "talking with my wife", "parliament, wine")
sample_search <- sample_search[order(runif(length(sample_search)))]
sample_index <- 1
results <- data_frame(date = c(NA), text = "No search has happened")
# Define UI for application that draws a histogram
ui <- shinyUI(
  fluidPage(theme = "bootstrap.css",
            
            fluidRow(column(width = 3, 
                            tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/2/21/Samuel_Pepys.jpg/205px-Samuel_Pepys.jpg", 
                                     width = "205px", height = "240px")),
                     # Application title
                     column(width = 9,
                            titlePanel("Search the Diary of Samuel Pepys"),
                            tags$br(), "The Diary of Samuel Pepys covered ten years from 1660 to 1669. Use this page to search the text of the diary and to plot",
                            "the frequency of words and phrases over the years.",
                            
                            tags$br(), tags$br(), "See the full text of the diary at ",
                            a("pepysdiary.com.", href="http://www.pepysdiary.com", target="_blank"), 
                            "This text is downloaded from ",
                            a("Project Guttenberg", href="http://www.gutenberg.org/ebooks/4200?msg=welcome_stranger", target="_blank"), 
                            "(1893 edition of the diary edited by Henry B. Wheatley).",
                            tags$br(), tags$br(), "Enter text into the search box below. Each place where the search text is found is displayed along with a few words before and after the text.",
                            "Click on Examples to see some sample searches.", "Click on the Date of Entry to see the full entry.",
                            tags$br(), tags$br())),
            fluidRow(column(width = 3),
                     column(width = 9, fluidRow(column(width = 8, textInput("search_text", "Search for", starting_search, width = "95%")),
                                                #column(width = 2, actionButton("do_search", "Search")),
                                                column(width = 2, actionButton("example", "Examples")))
                            
                     )
            ),
            tabsetPanel(
              tabPanel("Term in Context", DT::dataTableOutput("found_text")),
              tabPanel("Histogram", plotOutput("the_histogram")),
              tabPanel("Help", 
                       fluidRow( 
                         column(width = 1), 
                         column(width = 10,
                                fluidRow(tags$br(), "Type something into the search box and you will see where it appears throughout the diary.",
                                         "Click on the", tags$b("Examples"),  "button to cycle through a list of searches.",
                                         tags$p(), 
                                         "The search box does not work like a Google search.",
                                         "It looks for an exact match character by character.",
                                         "You can use that to cast a wide net for variations in spelling.",
                                         "For example, the word fragment", 
                                         tags$code("Castlem"),
                                         "will find both 'Castlemaine' and 'Castlemayne'.",
                                         tags$p(),
                                         "The search uses something called",
                                         a("regular expressions", href="http://www.regular-expressions.info/quickstart.html", target="_blank"),
                                         "which can provide for more more advanced searches.",
                                         "It is unlikely that you will want to use any of the features of regular expressioins,",
                                         "but the capability is there.",
                                         
                                         "A backslash character has a special meaning. For example, \\d will",
                                         "match any digit. Thus, the search string",
                                         tags$code("\\d\\d\\d"), "would find any three-digit number.",
                                         "Several characters have special meanings as",
                                         "part of a regular expression and won't be taken literally: [ \\ ^$ . | ? * + ( ) { } .",
                                         "To look for a question in the search, you must use",
                                         "a backslash to", tags$em("escape"), "the special meaning of the",
                                         "question mark:", tags$code("\\?"), ".", "The special mark \\b matches the beginning or end of a word.",
                                         "So ", tags$code("ill"), "will match either", tags$em("ill"), "or", tags$em("illness"),
                                         "or", tags$em("till"),
                                          "but", tags$code("\\bill\\b"), "will match only", tags$em("ill"), ".",
                                         tags$p(), "Search provided by John Goldin  john.r.goldin gmail", tags$br(),
                                         "This page was created using Shiny by RStudio."
                                )))
              ))
            
  ))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {
  
    # escape is TRUE by default for security reasons, but because
    # I am controlling data from inside app, should be no security issue.
    output$found_text <- DT::renderDataTable(word_in_context(entry_df, input$search_text, max_entries = 40000), 
                                             colnames = c("Date of Entry", "Context"),
                                             escape = FALSE,
                                             selection = c("none"),
                                             #extensions = 'Buttons', 
                                             options = list(
                                               pageLength = 20,
                                               lengthMenu = c(10, 20, 50, 100),
                                               dom = 'iptl'
                                               #buttons = c('copy', 'print')
                                             ))
    output$the_histogram <- renderPlot({word_histogram(entry_df, search_for = input$search_text)})
    observeEvent(input$example, {
      updateTextInput(session, "search_text",  value = sample_search[sample_index])
      sample_index <<- sample_index + 1
      if (sample_index > length(sample_search)) sample_index <<- 1
    })
})

# Run the application 
shinyApp(ui = ui, server = server)

