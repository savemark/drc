# Module user interface
weightedInterestRateSwapUI <- function(id, selected = list()) {
  ns <- NS(id)
  stress_type <- match.arg(selected$stress_type, c("none", "absolute_up", "absolute_down", "relative_up", "relative_down"))
  tagList(
    fluidRow(
      column(12,
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
weightedInterestRateSwapServer <- function(input, 
                                           output, 
                                           session, 
                                           swaprate,
                                           maximum_maturity,
                                           credit_risk_adjustment,
                                           allow_negative_rates,
                                           ufr) {
  
  # Datasets
  datasetWeightedInterestRateSwap <- reactive({
    suppressWarnings(par_t <- as.numeric(trimws(unlist(strsplit(swaprate(), ",")))))
    wirs <- weightedInterestSwap(par_t, 
                                 T = maximum_maturity(), 
                                 credit.adj = credit_risk_adjustment(), 
                                 allow.negative.rates = allow_negative_rates(), 
                                 UFR = ufr(),
                                 stress = stress,
                                 args.stress = list(type = input$stress_type))
  })
  
  # Tables
  output$wirs <- DT::renderDT({
    datatable(datasetWeightedInterestRateSwap(), 
              rownames = FALSE,
              options = list(paging = FALSE, info = FALSE, searching = FALSE)) %>% formatRound(columns = 2:8, digits = 8) %>% formatStyle(columns = 1:8, fontSize = "8pt")
  })
  
  return(list(wirs = reactive({datasetWeightedInterestRateSwap()})))
}