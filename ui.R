library(shiny)
library(DT)
library(shinythemes)

source("drk.R")

shinyUI(
  fluidPage(
    tags$head(
      tags$style(HTML(
      "
      #checkInput table > thead > tr > th,
      #checkInput table > tbody > tr > th,
      #checkInput table > tfoot > tr > th,
      #checkInput table > thead > tr > td,
      #checkInput table > tbody > tr > td,
      #checkInput table > tfoot > tr > td {padding:0px;}"
      )
      )
    ),
    tags$head(
      tags$style(HTML(
      "
      #wirs table > thead > tr > th,
      #wirs table > tbody > tr > th,
      #wirs table > tfoot > tr > th,
      #wirs table > thead > tr > td,
      #wirs table > tbody > tr > td,
      #wirs table > tfoot > tr > td {padding:0px;}"
      )
      )
    ),
    theme = shinytheme("simplex"),
    fluidRow(
      column(8,
             titlePanel("Diskonteringsräntekurvor"),
             p("Detta program beräknar diskonteringsräntekurvor både från idag och framåt i tiden,
                    för exempelvis användning till att beräkna värdet av framtida balansräkningar. Kurvorna kan enkelt skrivas till 
                    Excel i tabellformat.\n
                    Indata är marknadsnoterade swapräntor."),
             p("Se beräkningsmetodik i PROMEMORIA 2013-12-01 [FI Dnr 13-11409]."),
             offset = 1)
    ),
    br(),
    fluidRow(
      column(8,
             h4("MARKNADSNOTERINGAR FÖR SWAPRÄNTOR"),
             textInput("swapvector", "Marknadsnoteringar", "-0.00095, -0.001, -0.00103, -0.00083, -0.00045, 0.00005, 0.00065, 0.00125, 0.00185, 0.00243, NA, 0.00348, NA, NA, 0.00465, NA, NA, NA, NA, 0.00578", 
                       width = "100%"),
             p("Skriv in marknadsnoteringar för swapräntor. NA står för Not Available. NA behöver skrivas in för noteringar som saknas om man vill ha exempelvis 20 eller 30 nollkupongräntor. Den
               första och den sista koordinaten i n-tupeln ovan får inte vara NA eftersom diskonteringsfaktorer beräknas rekursivt mellan kända noteringar."),
             hr(),
             offset = 1
      )
    ),
    fluidRow(
      column(8,
             h4("1. DISKONTERINGSRÄNTEKURVA FRÅN MARKNADSNOTERADE SWAPRÄNTOR"),
             fluidRow(
               column(2,
                      numericInput("credit_adjustment", "Kreditriskjustering", -0.0035)
               ),
               column(2,
                      checkboxInput("allow_negative_rates", "Tillåt negativa räntor", value = TRUE)
               )
             ),
             offset = 1
      )
    ),
    fluidRow(
      column(8,
             DTOutput("checkInput"),
             hr(),
             offset = 1
      ),
      column(2,
             downloadButton("downloadDatasetInterestRateSwap", "Ladda ned")
      )
    ),
    fluidRow(
      column(8,
             h4("2. LINJÄRT VIKTADE TERMINSRÄNTOR"),
             fluidRow(
               column(8,
                      plotOutput("drkPlot")
               )
             ),
             fluidRow(
               column(2,
                      numericInput("credit_adjustment_2", "Kreditriskjustering", -0.0035)
               ),
               column(2,
                      numericInput("ufr", "UFR", 0.042)
               ),
               column(2,
                      checkboxInput("allow_negative_rates_2", "Tillåt negativa räntor", value = TRUE)
               ),
               column(2,
                      sliderInput("maximum_duration", "Längsta löptid", min = 1, max = 150, value = 120)
               )
             ),
             fluidRow(
               column(8,
                      DTOutput("wirs")
               ),
               column(2,
                      downloadButton("downloadDatasetWeightedInterestRateSwap", "Ladda ned")
               )
             ),
             offset = 1
      )
    )
  )
)