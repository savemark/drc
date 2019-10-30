library(shiny)
library(DT)
library(shinythemes)

source("drk.R")

shinyUI(
  fluidPage(
    theme = shinytheme("simplex"),
    titlePanel("Diskonteringsräntekurvor"),
    p("Detta program beräknar diskonteringsräntekurvor både från idag och framåt i tiden,
                    för exempelvis användning till att beräkna värdet av framtida balansräkningar. Kurvorna kan enkelt skrivas till 
                    Excel i tabellformat.\n
                    Indata är marknadsnoterade swapräntor."),
    p("Se beräkningsmetodik i PROMEMORIA 2013-12-01 [FI Dnr 13-11409]."),
    hr(),
    fluidRow(
      column(6,
             h4("1. Oviktad diskonteringskurva"),
             textInput("swapvector", "Skriv in marknadsnoteringar för swapräntor", "-0.00095, -0.001, -0.00103, -0.00083, -0.00045, 0.00005, 0.00065, 0.00125, 0.00185, 0.00243, NA, 0.00348, NA, NA, 0.00465, NA, NA, NA, NA, 0.00578", width = "100%"),
             fluidRow(
               column(6,
                      numericInput("credit_adjustment", "Kreditjustering", -0.0035)
               ),
               column(6,
                      checkboxInput("allow_negative_rates", "Tillåt negativa räntor", value = TRUE)
               )
             ),
             offset = 3
      )
    ),
    fluidRow(
      column(6,
             "Kontroll:",
             verbatimTextOutput("checkInput"),
             hr(),
             offset = 3
      )
    ),
    fluidRow(
      column(6,
             h4("2. Viktad diskonteringskurva"),
             fluidRow(
               column(3,
                      numericInput("credit_adjustment_2", "Kreditjustering", -0.0035)
               ),
               column(3,
                      numericInput("ufr", "UFR", 0.042)
               ),
               column(3,
                      sliderInput("maximum_duration", "Längsta löptid", min = 1, max = 150, value = 50)
                      ),
               column(3,
                      checkboxInput("allow_negative_rates_2", "Tillåt negativa räntor", value = TRUE)
               )
             ),
             offset = 3
      )
    ),
    fluidRow(
      column(6,
             verbatimTextOutput("wirs"),
             offset = 3
      ),
      column(3,
             plotOutput("drkPlot")
      )
    )
  )
)