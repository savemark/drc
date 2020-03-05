# Load packages
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(DT)
library(wesanderson)

# Load separate module and function scripts
source("functions_drc.R")
source("module_drc.R")

# User interface
ui <- dashboardPagePlus(
  skin = "black-light",
  sidebar_fullCollapse = TRUE, 
  useShinyjs(),
  header = dashboardHeaderPlus(
    title = tags$div(a(href = 'http://www.fi.se',
                       img(src = "fi-logotyp.png")), 
                     class = "dropdown"),
    enable_rightsidebar = TRUE,
    rightSidebarIcon = "gears"
  ),
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("Räntekurvsberäkningar", tabName = "tab_module_drc", icon = icon("calculator", class= "fas", lib = "font-awesome")),
      menuItem("Solvensmodellen", tabName = "tab_module_solvencymod", icon = icon("calculator", class= "fas", lib = "font-awesome")),
      menuItem("ESG-Analysverktyg", tabName = "tab_module_esg", icon = icon("calculator", class= "fas", lib = "font-awesome")),
      id = "left_side_bar"
    )
  ),
  body = dashboardBody(
    withMathJax(),
    setShadow("card"),
    tabItems(
      tabItem(tabName = "tab_module_drc",
              fluidRow(
                gradientBox(
                  title = "Diskonteringsräntekurvor för Solvens I",
                  status = "primary",
                  icon = "fa fa-th",
                  gradientColor = "maroon", 
                  boxToolSize = "xs", 
                  width = 12,
                  collapsible = TRUE,
                  closable = TRUE,
                  tags$p("Detta program beräknar diskonteringsräntekurvor både från idag och framåt i tiden,
                    för exempelvis användning till att beräkna värdet av framtida balansräkningar. Kurvorna kan enkelt skrivas till 
                    Excel i tabellformat.\n
                    Indata är marknadsnoterade swapräntor."),
                  tags$p("Finansinspektionen publicerar dessa beräkningar månadsvis.")
                )
              ),
              fluidRow(
                boxPlus(
                  title = "Information om diskonteringsräntekurvor som publiceras", 
                  status = "primary",
                  width = 6,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  closable = FALSE,
                  tags$h4("Diskonteringsräntekurvor som publiceras"),
                  tags$ul(
                    tags$li("Flikar FFFS 2013:23: diskonteringsräntekurvor som är beräknade enligt FFFS 2013:23."),
                    tags$li("Flikar FFFS 2019:21 Ordinarie: diskonteringsräntekurvor beräknade enligt FFFS 2019:21 med ordinarie metod för beräkning av långsiktig terminsränta."),
                    tags$li("Flikar FFFS 2019:21 Tillfällig: diskonteringsräntekurvor beräknade enligt FFFS 2019:21 med tillfällig metod för beräkning av långsiktig terminsränta (se 4 kap. 26 § i FFFS 2019:21).")
                  ),
                  tags$h4("Löptider på swapräntor som använts vid beräkning av diskonteringsräntekurvor"),
                  tags$ul(
                    tags$li("FFFS 2013:23: 1-10, 12, 15 och 20"),
                    tags$li("FFFS 2019:21 Ordinarie: 1-10, 12, 15 och 20"),
                    tags$li("FFFS 2019:21 Tillfällig: 1-10, 12, 15 och 20.")
                  ),
                  tags$h4("Nivå på långsiktig terminsränta som använts vid beräkning av diskonteringsräntekurvor"),
                  tags$ul(
                    tags$li("FFFS 2013:23: 4,2 % (konstant enligt Bilaga 2 till FFFS 2013:23)"),
                    tags$li("FFFS 2019:21 Ordinarie: 3,75 % (fastställs årligen enligt 4 kap. 18 § i FFFS 2019:21"),
                    tags$li("FFFS 2019:21 tillfällig 4,2 % (bestäms i enlighet med 4 kap. 26 § i FFFS 2019:21).")
                  )
                ),
                boxPlus(
                  title = "Beräkningsmetodik", 
                  status = "primary",
                  width = 6,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  closable = FALSE,
                  helpText("Programmet beräknar diskonteringsräntekurvorna genom att lösa följande optimeringsproblem."),
                  helpText("$$\\min_{x}\\left(\\text{par}(t_3)\\cdot\\sum_{i=1}^{t_1}\\text{DF}(i)+y_{t_1+1}(x)+\\dots+y_{t_3-1}(x)+\\frac{1}{(1+x)^{t_3}}-\\left(1-\\frac{1}{1+x}\\right)\\right)^2$$"),
                  helpText("För fler detaljer, se beräkningsmetodik i promemoria [FI-Dnr 13-11409 (2013-12-01)].")
                )
              ),
              fluidRow(
                box(
                  title = "Marknadsnoteringar för swapräntor", 
                  status = "warning",
                  width = 12,
                  collapsible = TRUE,
                  textInput("swaprate", "Marknadsnoteringar", "0.00178, 0.0017, 0.00178, 0.00183, 0.00213, 0.0025, 0.00293, 0.00335, 0.0038, 0.00425, NA, 0.00515, NA, NA, 0.00613, NA, NA, NA, NA, 0.00705", 
                            width = "100%"),
                  #-0.00095, -0.001, -0.00103, -0.00083, -0.00045, 0.00005, 0.00065, 0.00125, 0.00185, 0.00243, NA, 0.00348, NA, NA, 0.00465, NA, NA, NA, NA, 0.00578
                  #0.013200, 0.015275, 0.0177, 0.02008, 0.02208, 0.023630, 0.0249, 0.025930, 0.02678, 0.02745, NA, 0.028450, NA, NA, 0.029400, NA, NA, NA, NA, 0.0304
                  tags$p("Skriv in marknadsnoteringar för swapräntor. NA står för Not Available. NA behöver skrivas in för noteringar som saknas om man vill ha exempelvis 20 eller 30 nollkupongräntor. Den
                     första och den sista koordinaten i n-tupeln ovan får inte vara NA eftersom diskonteringsfaktorer beräknas rekursivt mellan kända noteringar."),
                  tags$p("Observera att decimalpunkt används för dessa tal men inte för andra tal i programmet.")
                )
              ),
              fluidRow(
                box(
                  title = "Figurer",
                  status = "info",
                  width = 12,
                  collapsible = TRUE,
                  mainPanel(
                    tabsetPanel(
                      type = "tabs",
                      tabPanel("FFFS 2013:23"),
                      tabPanel("FFFS 2019:21 Ordinarie", plotOutput("zcr_curve_all_ordinary")),
                      tabPanel("FFFS 2019:21 Tillfällig")
                    ),
                    width = 12
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Tabeller",
                  status = "primary",
                  width = 12,
                  collapsible = TRUE,
                  mainPanel(
                    tabsetPanel(
                      type = "tabs",
                      tabPanel("FFFS 2013:23"),
                      tabPanel("FFFS 2019:21 Ordinarie", 
                               tags$br(),
                               mainPanel(
                                 tabsetPanel(
                                   type = "pills",
                                   tabPanel("Ostressad", weightedInterestRateSwapUI("none_ordinary")),
                                   tabPanel("Stressad: Räntehöjningschock (absolut)", 
                                            weightedInterestRateSwapUI("abs_up_ordinary", 
                                                                       selected = list(stress_type = "absolute_up"))),
                                   tabPanel("Stressad: Räntesänkningschock (absolut)", 
                                            weightedInterestRateSwapUI("abs_down_ordinary", 
                                                                       selected = list(stress_type = "absolute_down"))),
                                   tabPanel("Stressad: Räntehöjningschock (relativ)", 
                                            weightedInterestRateSwapUI("rel_up_ordinary", 
                                                                       selected = list(stress_type = "relative_up"))),
                                   tabPanel("Stressad: Räntesänkningschock (relativ)", 
                                            weightedInterestRateSwapUI("rel_down_ordinary",
                                                                       selected = list(stress_type = "relative_down")))
                                 ),
                                 width = 12
                               )
                      ),
                      tabPanel("FFFS 2019:21 Tillfällig")
                    ),
                    width = 12
                  )
                )
              )
      ),
      tabItem(tabName = "tab_module_solvencymod"
      ),
      tabItem(tabName = "tab_module_esg"
      )
    )
  ),
  rightsidebar = rightSidebar(
    rightSidebarTabContent(
      id = "settings",
      icon = "paint-brush",
      title = "Inställningar",
      active = TRUE,
      tags$script(HTML(
        "$(\".control-sidebar\").on('webkitTransitionEnd otransitionend oTransitionEnd msTransitionEnd transitionend', function() { $(window).trigger(\"resize\"); } );"
      )),
      uiOutput("right_side_bar")
    ),
    rightSidebarTabContent(
      id = "downloads",
      icon = "table",
      title = "Nedladdningsbar data",
      active = FALSE,
      tags$script(HTML(
        "$(\".control-sidebar\").on('webkitTransitionEnd otransitionend oTransitionEnd msTransitionEnd transitionend', function() { $(window).trigger(\"resize\"); } );"
      )),
      uiOutput("right_side_bar_downloads")
    ),
    background = "light"
  ),
  footer = dashboardFooter(
    left_text = "Version 0.1",
    right_text = "Senast uppdaterad: Mars 2020"
  )
)

# Server logic
server <- function(input, output, session) {
  observe({
    if (req(input$left_side_bar) == "tab_module_drc"){
      
      shinyjs::addClass(selector = "body", class = "control-sidebar-open")
      
      # Right sidebar elements
      output$right_side_bar <- renderUI({
        tagList(
          tags$p(tags$i("FFFS 2019:21 Ordinarie")),
          numericInput("credit_risk_adjustment", "Kreditriskjustering", -0.0035, step = 1/10000),
          numericInput("ufr", "L\u00E5ngsiktig terminsr\u00E4nta", 0.0375, step = 1/10000),
          checkboxInput("allow_negative_rates", "Till\u00E5t negativa kreditriskjusterade swapr\u00E4ntor", value = TRUE),
          sliderInput("maximum_maturity", "L\u00E4ngsta l\u00F6ptid", min = 20, max = 150, value = 100),
          tags$hr(),
          tags$p(tags$i("FFFS 2019:21 Tillfällig")),
          numericInput("credit_risk_adjustment_temp", "Kreditriskjustering", -0.0035, step = 1/10000),
          numericInput("ufr_temp", "L\u00E5ngsiktig terminsr\u00E4nta", 0.042, step = 1/10000),
          checkboxInput("allow_negative_rates_temp", "Till\u00E5t negativa kreditriskjusterade swapr\u00E4ntor", value = TRUE),
          sliderInput("maximum_maturity_temp", "L\u00E4ngsta l\u00F6ptid", min = 20, max = 150, value = 100),
          tags$hr()
        )
      })
      
      output$right_side_bar_downloads <- renderUI({
        tagList(
          tags$p(tags$i("Excelformat")),
          downloadButton("downloadDatasetWeightedInterestRateSwap", "Ladda ned tabeller")
        )
      })
      
      # Modules
      wirss_none_ordinary <- callModule(weightedInterestRateSwapServer, 
                                        "none_ordinary", 
                                        swaprate = reactive({req(input$swaprate)}), 
                                        maximum_maturity = reactive({req(input$maximum_maturity)}), 
                                        credit_risk_adjustment = reactive({input$credit_risk_adjustment}),
                                        allow_negative_rates = reactive({input$allow_negative_rates}),
                                        ufr = reactive({input$ufr})
      )
      wirss_abs_up_ordinary <- callModule(weightedInterestRateSwapServer, 
                                          "abs_up_ordinary", 
                                          swaprate = reactive({req(input$swaprate)}), 
                                          maximum_maturity = reactive({req(input$maximum_maturity)}),
                                          credit_risk_adjustment = reactive({input$credit_risk_adjustment}),
                                          allow_negative_rates = reactive({input$allow_negative_rates}),
                                          ufr = reactive({input$ufr})
      )
      wirss_abs_down_ordinary <- callModule(weightedInterestRateSwapServer, 
                                            "abs_down_ordinary", 
                                            swaprate = reactive({req(input$swaprate)}), 
                                            maximum_maturity = reactive({req(input$maximum_maturity)}),
                                            credit_risk_adjustment = reactive({input$credit_risk_adjustment}),
                                            allow_negative_rates = reactive({input$allow_negative_rates}),
                                            ufr = reactive({input$ufr})
      )
      wirss_rel_up_ordinary <- callModule(weightedInterestRateSwapServer, 
                                          "rel_up_ordinary", 
                                          swaprate = reactive({req(input$swaprate)}), 
                                          maximum_maturity = reactive({req(input$maximum_maturity)}),
                                          credit_risk_adjustment = reactive({input$credit_risk_adjustment}),
                                          allow_negative_rates = reactive({input$allow_negative_rates}),
                                          ufr = reactive({input$ufr})
      )
      wirss_rel_down_ordinary <- callModule(weightedInterestRateSwapServer, 
                                            "rel_down_ordinary", 
                                            swaprate = reactive({req(input$swaprate)}), 
                                            maximum_maturity = reactive({req(input$maximum_maturity)}),
                                            credit_risk_adjustment = reactive({input$credit_risk_adjustment}),
                                            allow_negative_rates = reactive({input$allow_negative_rates}),
                                            ufr = reactive({input$ufr})
      )
      # Plots
      output$zcr_curve_all_ordinary <- renderPlot({
        req(input$swaprate, input$maximum_maturity, input$credit_risk_adjustment, input$ufr)
        par(bg = NA)
        data <- cbind(wirss_none_ordinary$wirs()[ , 8], 
                      wirss_abs_up_ordinary$wirs()[ , 8], 
                      wirss_abs_down_ordinary$wirs()[ , 8], 
                      wirss_rel_up_ordinary$wirs()[ , 8], 
                      wirss_rel_down_ordinary$wirs()[ , 8])
        plot(data[, 1], xlab = "L\u00F6ptid", ylab = "Nollkupongr\u00E4nta", type = "p", main = "Diskonteringsr\u00E4ntekurvor", ylim = 1.10*c(min(data), max(data)))
        lapply(1:5, function(x) {lines(data[ , x], col = wes_palette("Zissou1", 5)[x])})
        lapply(1:5, function(x) {points(data[ , x], col = wes_palette("Zissou1", 5)[x])})
        legend("topleft", 
               legend = c("Ostressad", 
                          "Absolut räntehöjningschock",
                          "Absolut räntesänkningschock",
                          "Relativ räntehöjningschock",
                          "Relativ räntesänkningschock"),
               border = "NA",
               col = wes_palette("Zissou1", 5),
               bty = "n",
               lty = 1,
               pch = 1,
               bg = "white",
               inset = 0.01
        )
        grid(lty = "dotted", lwd = 0.5)
      })
      
      # Downloads
      output$downloadDatasetWeightedInterestRateSwap <- downloadHandler(
        filename = function() {
          paste("wirs", ".xlsx", sep = "")
        },
        content = function(file) {
          xlsx::write.xlsx(wirss_none_ordinary$wirs(), 
                           file, 
                           sheetName = "Ostressad", 
                           col.names = TRUE, 
                           row.names = FALSE, 
                           append = FALSE)
          xlsx::write.xlsx(wirss_abs_up_ordinary$wirs(), 
                           file, 
                           sheetName = "Stress Absolut upp", 
                           col.names = TRUE, 
                           row.names = FALSE, 
                           append = TRUE)
          xlsx::write.xlsx(wirss_abs_down_ordinary$wirs(), 
                           file, 
                           sheetName = "Stress Absolut ned", 
                           col.names = TRUE, 
                           row.names = FALSE, 
                           append = TRUE)
          xlsx::write.xlsx(wirss_rel_up_ordinary$wirs(), 
                           file, 
                           sheetName = "Stress Relativ upp", 
                           col.names = TRUE, 
                           row.names = FALSE, 
                           append = TRUE)
          xlsx::write.xlsx(wirss_rel_down_ordinary$wirs(), 
                           file, 
                           sheetName = "Stress Relativ ned", 
                           col.names = TRUE, 
                           row.names = FALSE, 
                           append = TRUE)
        }
      )
      
    }
    if (req(input$left_side_bar) == "tab_module_solvencymod"){
      shinyjs::removeClass(selector = "body", class = "control-sidebar-open")
      output$right_side_bar <- renderUI({ div() })
    }
    if (req(input$left_side_bar) == "tab_module_esg"){
      shinyjs::removeClass(selector = "body", class = "control-sidebar-open")
      output$right_side_bar <- renderUI({ div() })
    }
  })
  
  
}

shinyApp(ui, server)