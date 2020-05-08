# Module user interface
weightedInterestRateSwapUI <- function(id, selected = list()) {
  ns <- NS(id)
  stress_type <- match.arg(selected$stress_type, c("identity", 
                                                   "no_negative",
                                                   "absolute_up", 
                                                   "absolute_up_100bps", 
                                                   "absolute_up_50bps",
                                                   "absolute_down", 
                                                   "absolute_down_100bps", 
                                                   "absolute_down_50bps",
                                                   "relative_up", 
                                                   "relative_down")
  )
  tagList(
    fluidRow(
      column(12,
             radioButtons(ns("stress_type"), 
                          label = "Transformering (stress):",
                          choiceNames = c("Otransformerad",
                                          "Till\u00E5t ej negativa kreditriskjusterade swapr\u00E4ntor",
                                          "Absolut r\u00E4nteh\u00F6jningschock",
                                          "Absolut r\u00E4nteh\u00F6jningschock 100bps",
                                          "Absolut r\u00E4nteh\u00F6jningschock 50bps",
                                          "Absolut r\u00E4ntes\u00E4nkningschock",
                                          "Absolut r\u00E4ntes\u00E4nkningschock 100bps",
                                          "Absolut r\u00E4ntes\u00E4nkningschock 50bps",
                                          "Relativ r\u00E4nteh\u00F6jningschock",
                                          "Relativ r\u00E4ntes\u00E4nkningschock"),
                          choiceValues = c("identity", 
                                           "no_negative",
                                           "absolute_up", 
                                           "absolute_up_100bps", 
                                           "absolute_up_50bps",
                                           "absolute_down", 
                                           "absolute_down_100bps", 
                                           "absolute_down_50bps",
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
                                           shift,
                                           ufr) {
  
  # Datasets
  datasetWeightedInterestRateSwap <- reactive({
    suppressWarnings(par_t <- as.numeric(trimws(unlist(strsplit(swaprate(), ",")))))
    wirs <- weightedInterestRateSwap(par_t, 
                                     T = maximum_maturity(), 
                                     shift = shift(),
                                     UFR = ufr(),
                                     transformation = stress,
                                     args.transformation = list(type = input$stress_type))
  })
  
  # Tables
  output$wirs <- DT::renderDT({
    datatable(datasetWeightedInterestRateSwap(), 
              rownames = FALSE,
              options = list(paging = FALSE, info = FALSE, searching = FALSE)) %>% formatRound(columns = 2:8, digits = 8) %>% formatStyle(columns = 1:8, fontSize = "8pt")
  })
  
  return(list(wirs = reactive({datasetWeightedInterestRateSwap()})))
}