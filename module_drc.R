# Module user interface
weightedInterestRateSwap_ui_controls <- function(id) {
  ns <- NS(id)
  
  # Right sidebar elements
  uiOutput(ns("controls"))
}


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
  choiceNames <- c("Otransformerad",
                   "Till\u00E5t ej negativa kreditriskjusterade swapr\u00E4ntor",
                   "Absolut r\u00E4nteh\u00F6jningschock",
                   "Absolut r\u00E4nteh\u00F6jningschock 100bps",
                   "Absolut r\u00E4nteh\u00F6jningschock 50bps",
                   "Absolut r\u00E4ntes\u00E4nkningschock",
                   "Absolut r\u00E4ntes\u00E4nkningschock 100bps",
                   "Absolut r\u00E4ntes\u00E4nkningschock 50bps",
                   "Relativ r\u00E4nteh\u00F6jningschock",
                   "Relativ r\u00E4ntes\u00E4nkningschock")
  choices <- list("identity", 
                  "no_negative",
                  "absolute_up", 
                  "absolute_up_100bps", 
                  "absolute_up_50bps",
                  "absolute_down", 
                  "absolute_down_100bps", 
                  "absolute_down_50bps",
                  "relative_up", 
                  "relative_down")
  names(choices) <- choiceNames
  tagList(
    fluidRow(
      column(12,
             tags$br(),
             selectInput(ns("stress_type"), 
                         label = "Transformering (stress):",
                         choices = choices,
                         selected = stress_type
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
  
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem("Menu item", icon = icon("calendar"))
    )
  })
  
  output$controls <- renderUI({
    ns <- session$ns
    tagList(
      tags$i("Test"),
      numericInput(ns("shift"), "Kreditriskjustering", -0.0035, step = 1/10000),
      numericInput(ns("ufr"), "L\u00E5ngsiktig terminsr\u00E4nta", 0.042, step = 1/10000),
      sliderInput(ns("maximum_maturity"), "L\u00E4ngsta l\u00F6ptid", min = 20, max = 150, value = 100),
      tags$hr()
    )
  })
  
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
