# Next-Word Predictor — Shiny app
# A stupid back-off n-gram model: try trigram (last 2 words) -> bigram (last word)
# -> most common words. Author: Khaleel Khan
library(shiny)
source("predict.R")   # loads model + clean_input() + predict_next()

ui <- fluidPage(
  titlePanel("Next-Word Predictor"),
  sidebarLayout(
    sidebarPanel(
      textInput("phrase", "Type a phrase:", value = "i would like to"),
      actionButton("go", "Predict next word", class = "btn-primary"),
      br(), br(),
      helpText("Enter one or more words and press the button. The app predicts the",
               "most likely next word from an n-gram model trained on blogs, news",
               "and Twitter text.")
    ),
    mainPanel(
      h3("Predicted next word"),
      div(style = "font-size:28px; font-weight:bold; color:#2c7fb8;",
          textOutput("top1")),
      br(),
      h4("Other likely words"),
      textOutput("alts"),
      br(), hr(),
      em("Stupid back-off model: trigram → bigram → unigram.")
    )
  )
)

server <- function(input, output) {
  preds <- eventReactive(input$go, predict_next(input$phrase, 3),
                         ignoreNULL = FALSE)   # also fire once on load
  output$top1 <- renderText({ p <- preds(); if (length(p)) p[1] else "—" })
  output$alts <- renderText({
    p <- preds()
    if (length(p) > 1) paste(p[-1], collapse = ", ") else "—"
  })
}

shinyApp(ui, server)
