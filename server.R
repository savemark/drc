library(shiny)

shinyServer(
  function(input, output) {
    
    # Datasets
    datasetInterestRateSwap <- reactive({
      suppressWarnings(par_t <- as.numeric(trimws(unlist(strsplit(input$swapvector, ",")))))
      irs <- interestRateSwap(par_t, credit.adj = input$credit_adjustment, allow.negative.rates = input$allow_negative_rates)
    })
    
    datasetWeightedInterestRateSwap <- reactive({
      suppressWarnings(par_t <- as.numeric(trimws(unlist(strsplit(input$swapvector, ",")))))
      wirs <- weightedInterestSwap(par_t, T = input$maximum_duration, credit.adj = input$credit_adjustment_2, allow.negative.rates = input$allow_negative_rates_2, UFR = input$ufr)
    })
    
    # Tables
    output$checkInput <- DT::renderDT({
     datatable(datasetInterestRateSwap(), 
               rownames = FALSE,
               options = list(paging = FALSE, info = FALSE, searching = FALSE)) %>% formatRound(columns = 2:6, digits = 8) %>% formatStyle(columns = 0:6, fontSize = "8pt")
    })
    
    output$wirs <- DT::renderDT({
      datatable(datasetWeightedInterestRateSwap(), 
                rownames = FALSE,
                options = list(paging = FALSE, info = FALSE, searching = FALSE)) %>% formatRound(columns = 2:4, digits = 8) %>% formatStyle(columns = 0:6, fontSize = "8pt")
    })
    
    # Plots
    output$drkPlot <- renderPlot({
      par(bg = NA)
      suppressWarnings(par_t <- as.numeric(trimws(unlist(strsplit(input$swapvector, ",")))))
      irs <- interestRateSwap(par_t)
      wirs <- weightedInterestSwap(par_t, T = input$maximum_duration, credit.adj = input$credit_adjustment_2, allow.negative.rates = input$allow_negative_rates_2, UFR = input$ufr)
      plot(wirs[, 4], xlab = "Löptid", ylab = "Nollkupongränta", type = "p", main = "Diskonteringsräntekurva")
      lines(wirs[, 4])
    })
    
    # Downloads
    output$downloadDatasetInterestRateSwap <- downloadHandler(
      filename = function() {
        paste("irs", ".xlsx", sep = "")
      },
      content = function(file) {
        xlsx::write.xlsx(datasetInterestRateSwap(), file, sheetName = "Nollkupongränta", 
                         col.names = TRUE, row.names = FALSE, append = FALSE)
      }
    )
    
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
)
