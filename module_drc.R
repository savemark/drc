# Module user interface
weightedInterestRateSwapUI <- function(id, selected = list()) {
  ns <- NS(id)
  stress_type <- match.arg(selected$stress_type, c("none", "absolute_up", "absolute_down", "relative_up", "relative_down"))
  tagList(
    fluidRow(
      column(12,
             plotOutput(ns("wdrcplot"))
      )
    ),
    fluidRow(
      column(3,
             numericInput(ns("credit_adjustment"), "Kreditriskjustering", -0.0035, step = 1/10000)
      ),
      column(3,
             numericInput(ns("ufr"), "Ultimate forward rate", 0.042, step = 1/10000)
      ),
      column(3,
             checkboxInput(ns("allow_negative_rates"), "Till\u00E5t negativa kreditriskjusterade swapr\u00E4ntor", value = TRUE)
      ),
      column(3,
             sliderInput(ns("maximum_maturity"), "L\u00E4ngsta l\u00F6ptid", min = 1, max = 150, value = 120)
      )
    ),
    fluidRow(
      column(9,
             radioButtons(ns("stress_type"), 
                          label = "Stresstyp:",
                          choiceNames = c("Ingen",
                                          "Absolut r\u00E4nteh\u00F6jningschock",
                                          "Absolut r\u00E4ntes\u00E4nkningschock",
                                          "Relativ r\u00E4nteh\u00F6jningschock",
                                          "Relativ r\u00E4ntes\u00E4nkningschock"),
                          choiceValues = c("none",
                                           "absolute_up",
                                           "absolute_down",
                                           "relative_up",
                                           "relative_down"),
                          selected = stress_type,
                          inline = TRUE
             )
      ),
      column(3,
             downloadButton(ns("downloadDatasetWeightedInterestRateSwap"), "Ladda ned tabell")
      )
    ),
    fluidRow(
      column(12,
             DTOutput(ns("wirs"))
      )
    )
  )
}

# Module server logic
weightedInterestRateSwapServer <- function(input, output, session, swaprate) {
  
  # Datasets
  datasetWeightedInterestRateSwap <- reactive({
    suppressWarnings(par_t <- as.numeric(trimws(unlist(strsplit(swaprate(), ",")))))
    wirs <- weightedInterestSwap(par_t, 
                                 T = input$maximum_maturity, 
                                 credit.adj = input$credit_adjustment, 
                                 allow.negative.rates = input$allow_negative_rates, 
                                 UFR = input$ufr,
                                 stress = stress,
                                 args.stress = list(type = input$stress_type))
  })
  
  # Tables
  output$wirs <- DT::renderDT({
    datatable(datasetWeightedInterestRateSwap(), 
              rownames = FALSE,
              options = list(paging = FALSE, info = FALSE, searching = FALSE)) %>% formatRound(columns = 2:8, digits = 8) %>% formatStyle(columns = 1:8, fontSize = "8pt")
  })
  
  # Plots
  output$wdrcplot <- renderPlot({
    par(bg = NA)
    suppressWarnings(par_t <- as.numeric(trimws(unlist(strsplit(swaprate(), ",")))))
    irs <- interestRateSwap(par_t)
    wirs <- weightedInterestSwap(par_t, 
                                 T = input$maximum_maturity, 
                                 credit.adj = input$credit_adjustment, 
                                 allow.negative.rates = input$allow_negative_rates, 
                                 UFR = input$ufr,
                                 stress = stress,
                                 args.stress = list(type = input$stress_type))
    plot(wirs[, 8], xlab = "L\u00F6ptid", ylab = "Nollkupongr\u00E4nta", type = "p", main = "Diskonteringsr\u00E4ntekurva")
    lines(wirs[, 8])
    grid(lty = "dotted")
  })
  
  # Downloads
  output$downloadDatasetWeightedInterestRateSwap <- downloadHandler(
    filename = function() {
      paste("wirs", ".xlsx", sep = "")
    },
    content = function(file) {
      xlsx::write.xlsx(datasetWeightedInterestRateSwap(), file, sheetName = "Nollkupongränta", 
                       col.names = TRUE, row.names = FALSE, append = FALSE)
    }
  )
}