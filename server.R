library(shiny)

shinyServer(
  function(input, output) {
    
    output$checkInput <- renderPrint({
      suppressWarnings(par_t <- as.numeric(trimws(unlist(strsplit(input$swapvector, ",")))))
      irs <- interestRateSwap(par_t, credit.adj = input$credit_adjustment, allow.negative.rates = input$allow_negative_rates)
      print(irs)
    })
    
    #output$checkInput <- DT::renderDT({
    #  suppressWarnings(par_t <- as.numeric(trimws(unlist(strsplit(input$swapvector, ",")))))
    #  irs <- interestRateSwap(par_t, credit.adj = input$credit_adjustment, allow.negative.rates = input$allow_negative_rates)
    #  datatable(irs, options = list(paging = FALSE, info = FALSE, searching = FALSE)) %>% formatRound(columns = 2:6, digits = 5) %>% formatStyle(columns = 1:6, fontSize = "8pt")
    #})
    
    output$wirs <- renderPrint({
      suppressWarnings(par_t <- as.numeric(trimws(unlist(strsplit(input$swapvector, ",")))))
      wirs <- weightedInterestSwap(par_t, T = input$maximum_duration, credit.adj = input$credit_adjustment_2, allow.negative.rates = input$allow_negative_rates_2, UFR = input$ufr)
      print(wirs)
    })
    
    output$drkPlot <- renderPlot({
      suppressWarnings(par_t <- as.numeric(trimws(unlist(strsplit(input$swapvector, ",")))))
      irs <- interestRateSwap(par_t)
      wirs <- weightedInterestSwap(par_t, T = input$maximum_duration, credit.adj = input$credit_adjustment_2, allow.negative.rates = input$allow_negative_rates_2)
      plot(wirs[, 4], xlab = "Löptid", ylab = "Nollkupongränta", type = "l", main = "Diskonteringsräntekurva")
    })
    
  }
)
