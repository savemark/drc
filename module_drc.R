# Module user interface
weightedInterestRateSwapUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12,
             plotOutput(ns("wdrcplot"))
      )
    ),
    fluidRow(      
      column(2,
             downloadButton(ns("downloadDatasetWeightedInterestRateSwap"), "Ladda ned")
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
             checkboxInput(ns("allow_negative_rates"), "Tillåt negativa kreditriskjusterade swapräntor", value = TRUE)
      ),
      column(3,
             sliderInput(ns("maximum_maturity"), "Längsta löptid", min = 1, max = 150, value = 120)
      )
    ),
    fluidRow(
      column(12,
             radioButtons(ns("stress_type"), 
                          label = "Stresstyp:",
                          choiceNames = c("Ingen",
                                          "Absolut räntehöjningschock",
                                          "Absolut räntesänkningschock",
                                          "Relativ räntehöjningschock",
                                          "Relativ räntesänkningschock"),
                          choiceValues = c("none",
                                           "absolute_up",
                                           "absolute_down",
                                           "relative_up",
                                           "relative_down"),
                          inline = TRUE
             )
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
    suppressWarnings(par_t <- as.numeric(trimws(unlist(strsplit(swaprate, ",")))))
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
              options = list(paging = FALSE, info = FALSE, searching = FALSE)) %>% formatRound(columns = 2:4, digits = 8) %>% formatStyle(columns = 0:6, fontSize = "8pt")
  })
  
  # Plots
  output$wdrcplot <- renderPlot({
    par(bg = NA)
    suppressWarnings(par_t <- as.numeric(trimws(unlist(strsplit(swaprate, ",")))))
    irs <- interestRateSwap(par_t)
    wirs <- weightedInterestSwap(par_t, 
                                 T = input$maximum_maturity, 
                                 credit.adj = input$credit_adjustment, 
                                 allow.negative.rates = input$allow_negative_rates, 
                                 UFR = input$ufr,
                                 stress = stress,
                                 args.stress = list(type = input$stress_type))
    plot(wirs[, 4], xlab = "Löptid", ylab = "Nollkupongränta", type = "p", main = "Diskonteringsräntekurva")
    lines(wirs[, 4])
    grid(lty = "dotted")
  })
}
