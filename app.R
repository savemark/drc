# Load packages
library(shiny)
library(shinythemes)
library(DT)

# Load separate module and function scripts
source("functions_drc.R")
source("module_drc.R")

# User interface
ui <- fluidPage(
  tags$head(
    tags$style(HTML(
      "
      #name-wirs table > thead > tr > th,
      #name-wirs table > tbody > tr > th,
      #name-wirs table > tfoot > tr > th,
      #name-wirs table > thead > tr > td,
      #name-wirs table > tbody > tr > td,
      #name-wirs table > tfoot > tr > td {padding:0px;}")
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
           p("Se beräkningsmetodik i PROMEMORIA 2013-12-01 [FI Dnr 13-11409].")
    )
  ),
  br(),
  fluidRow(
    column(8,
           h4("MARKNADSNOTERINGAR FÖR SWAPRÄNTOR"),
           textInput("swaprate", "Marknadsnoteringar", "0.013200, 0.015275, 0.0177, 0.02008, 0.02208, 0.023630, 0.0249, 0.025930, 0.02678, 0.02745, NA, 0.028450, NA, NA, 0.029400, NA, NA, NA, NA, 0.0304", 
                     width = "100%"),
           # -0.00095, -0.001, -0.00103, -0.00083, -0.00045, 0.00005, 0.00065, 0.00125, 0.00185, 0.00243, NA, 0.00348, NA, NA, 0.00465, NA, NA, NA, NA, 0.00578
           p("Skriv in marknadsnoteringar för swapräntor. NA står för Not Available. NA behöver skrivas in för noteringar som saknas om man vill ha exempelvis 20 eller 30 nollkupongräntor. Den
               första och den sista koordinaten i n-tupeln ovan får inte vara NA eftersom diskonteringsfaktorer beräknas rekursivt mellan kända noteringar."),
           hr()
    )
  ),
  fluidRow(
    column(8,
           mainPanel(
             tabsetPanel(
               type = "tabs",
               tabPanel("Ostressad", weightedInterestRateSwapUI("name")),
               tabPanel("Stressad", weightedInterestRateSwapUI("name2"))
             ),
             width = 12
           )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  callModule(weightedInterestRateSwapServer, "name", swaprate = input$swaprate)
  callModule(weightedInterestRateSwapServer, "name2", swaprate = input$swaprate)
}

shinyApp(ui, server)
