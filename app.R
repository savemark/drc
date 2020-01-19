source("drk.R")

interestRateSwapUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6,
             h4("1. Oviktad diskonteringskurva"),
             textInput(ns("swapvector"), "Skriv in marknadsnoteringar för swapräntor", "-0.00095, -0.001, -0.00103, -0.00083, -0.00045, 0.00005, 0.00065, 0.00125, 0.00185, 0.00243, NA, 0.00348, NA, NA, 0.00465, NA, NA, NA, NA, 0.00578", width = "100%"),
             fluidRow(
               column(6,
                      numericInput(ns("credit_adjustment"), "Kreditjustering", -0.0035)
               ),
               column(6,
                      checkboxInput(ns("allow_negative_rates"), "Tillåt negativa räntor", value = TRUE)
               )
             ),
             offset = 3
      )
    ),
    fluidRow(
      column(6,
             "Kontroll:",
             verbatimTextOutput(ns("checkInput")),
             hr(),
             offset = 3
      )
    )
  )
}

interestRateSwapSR <- function(input, output, session) {
  output$checkInput <- renderPrint({
    suppressWarnings(par_t <- as.numeric(trimws(unlist(strsplit(input$swapvector, ",")))))
    irs <- interestRateSwap(par_t, credit.adj = input$credit_adjustment, allow.negative.rates = input$allow_negative_rates)
    print(irs)
  })
}

ui <- fluidPage(
  interestRateSwapUI("name"),
  interestRateSwapUI("name2")
)

server <- function(input, output, session) {
  callModule(interestRateSwapSR, "name")
  callModule(interestRateSwapSR, "name2")
}

shinyApp(ui, server)