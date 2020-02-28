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
  enable_preloader = FALSE,
  loading_duration = 1,
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
      id = "nav"
    )
  ),
  body = dashboardBody(
    tabItems(
      tabItem(tabName = "tab_module_drc",
              fluidRow(
                gradientBox(
                  title = "Diskonteringsräntekurvor",
                  status = "primary",
                  icon = "fa fa-th",
                  gradientColor = "maroon", 
                  boxToolSize = "xs", 
                  width = 12,
                  collapsible = TRUE,
                  closable = TRUE,
                  p("Detta program beräknar diskonteringsräntekurvor både från idag och framåt i tiden,
                    för exempelvis användning till att beräkna värdet av framtida balansräkningar. Kurvorna kan enkelt skrivas till 
                    Excel i tabellformat.\n
                    Indata är marknadsnoterade swapräntor."),
                  p("Se beräkningsmetodik i PROMEMORIA 2013-12-01 [FI Dnr 13-11409].")
                )
              ),
              fluidRow(
                box(
                  title = "Marknadsnoteringar för swapräntor", 
                  status = "warning",
                  width = 12,
                  collapsible = TRUE,
                  textInput("swaprate", "Marknadsnoteringar", "-0.00095, -0.001, -0.00103, -0.00083, -0.00045, 0.00005, 0.00065, 0.00125, 0.00185, 0.00243, NA, 0.00348, NA, NA, 0.00465, NA, NA, NA, NA, 0.00578", 
                            width = "100%"),
                  #0.013200, 0.015275, 0.0177, 0.02008, 0.02208, 0.023630, 0.0249, 0.025930, 0.02678, 0.02745, NA, 0.028450, NA, NA, 0.029400, NA, NA, NA, NA, 0.0304
                  p("Skriv in marknadsnoteringar för swapräntor. NA står för Not Available. NA behöver skrivas in för noteringar som saknas om man vill ha exempelvis 20 eller 30 nollkupongräntor. Den
                     första och den sista koordinaten i n-tupeln ovan får inte vara NA eftersom diskonteringsfaktorer beräknas rekursivt mellan kända noteringar.")
                )
              ),
              fluidRow(
                box(
                  title = "Figur",
                  status = "info",
                  width = 12,
                  collapsible = TRUE,
                  plotOutput("zcr_curve_all")
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
                      tabPanel("Ostressad", weightedInterestRateSwapUI("none")),
                      tabPanel("Stressad: Räntehöjningschock (absolut)", 
                               weightedInterestRateSwapUI("abs_up", 
                                                          selected = list(stress_type = "absolute_up"))),
                      tabPanel("Stressad: Räntesänkningschock (absolut)", 
                               weightedInterestRateSwapUI("abs_down", 
                                                          selected = list(stress_type = "absolute_down"))),
                      tabPanel("Stressad: Räntehöjningschock (relativ)", 
                               weightedInterestRateSwapUI("rel_up", 
                                                          selected = list(stress_type = "relative_up"))),
                      tabPanel("Stressad: Räntesänkningschock (relativ)", 
                               weightedInterestRateSwapUI("rel_down",
                                                          selected = list(stress_type = "relative_down")))
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
    tags$script(HTML(
      "$(\".control-sidebar\").on('webkitTransitionEnd otransitionend oTransitionEnd msTransitionEnd transitionend', function() { $(window).trigger(\"resize\"); } );"
    )),
    uiOutput("right_side_bar"),
    background = "light"
  ),
  footer = dashboardFooter(
    
  )
)

# Server logic
server <- function(input, output, session) {
  observe({
    if (req(input$nav) == "tab_module_drc"){
      shinyjs::addClass(selector = "body", class = "control-sidebar-open")
      output$right_side_bar <- renderUI({
        rightSidebarTabContent(
          id = "T_tab_module_drc",
          icon = "desktop",
          title = "Inställningar",
          p("Argument för diskonteringsräntekurvorna"),
          numericInput("credit_risk_adjustment", "Kreditriskjustering", -0.0035, step = 1/10000),
          numericInput("ufr", "Ultimate Forward Rate", 0.042, step = 1/10000),
          checkboxInput("allow_negative_rates", "Till\u00E5t negativa kreditriskjusterade swapr\u00E4ntor", value = TRUE),
          sliderInput("maximum_maturity", "L\u00E4ngsta l\u00F6ptid", min = 1, max = 150, value = 120),
          downloadButton("downloadDatasetWeightedInterestRateSwap", "Ladda ned tabeller")
        )
      })
      
      # Modules
      wirss_none <- callModule(weightedInterestRateSwapServer, 
                               "none", 
                               swaprate = reactive({input$swaprate}), 
                               maximum_maturity = reactive({input$maximum_maturity}), 
                               credit_risk_adjustment = reactive({input$credit_risk_adjustment}),
                               allow_negative_rates = reactive({input$allow_negative_rates}),
                               ufr = reactive({input$ufr})
      )
      wirss_abs_up <- callModule(weightedInterestRateSwapServer, 
                                 "abs_up", 
                                 swaprate = reactive({input$swaprate}), 
                                 maximum_maturity = reactive({input$maximum_maturity}),
                                 credit_risk_adjustment = reactive({input$credit_risk_adjustment}),
                                 allow_negative_rates = reactive({input$allow_negative_rates}),
                                 ufr = reactive({input$ufr})
      )
      wirss_abs_down <- callModule(weightedInterestRateSwapServer, 
                                   "abs_down", 
                                   swaprate = reactive({input$swaprate}), 
                                   maximum_maturity = reactive({input$maximum_maturity}),
                                   credit_risk_adjustment = reactive({input$credit_risk_adjustment}),
                                   allow_negative_rates = reactive({input$allow_negative_rates}),
                                   ufr = reactive({input$ufr})
      )
      wirss_rel_up <- callModule(weightedInterestRateSwapServer, 
                                 "rel_up", 
                                 swaprate = reactive({input$swaprate}), 
                                 maximum_maturity = reactive({input$maximum_maturity}),
                                 credit_risk_adjustment = reactive({input$credit_risk_adjustment}),
                                 allow_negative_rates = reactive({input$allow_negative_rates}),
                                 ufr = reactive({input$ufr})
      )
      wirss_rel_down <- callModule(weightedInterestRateSwapServer, 
                                   "rel_down", 
                                   swaprate = reactive({input$swaprate}), 
                                   maximum_maturity = reactive({input$maximum_maturity}),
                                   credit_risk_adjustment = reactive({input$credit_risk_adjustment}),
                                   allow_negative_rates = reactive({input$allow_negative_rates}),
                                   ufr = reactive({input$ufr})
      )
      
      # Plots
      output$zcr_curve_all <- renderPlot({
        par(bg = NA)
        data <- cbind(wirss_none$wirs()[ , 8], wirss_abs_up$wirs()[ , 8], wirss_abs_down$wirs()[ , 8], wirss_rel_up$wirs()[ , 8], wirss_rel_down$wirs()[ , 8])
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
          xlsx::write.xlsx(wirss_none$wirs(), 
                           file, 
                           sheetName = "Ostressad", 
                           col.names = TRUE, 
                           row.names = FALSE, 
                           append = FALSE)
          xlsx::write.xlsx(wirss_abs_up$wirs(), 
                           file, 
                           sheetName = "Stress Absolut upp", 
                           col.names = TRUE, 
                           row.names = FALSE, 
                           append = TRUE)
          xlsx::write.xlsx(wirss_abs_down$wirs(), 
                           file, 
                           sheetName = "Stress Absolut ned", 
                           col.names = TRUE, 
                           row.names = FALSE, 
                           append = TRUE)
          xlsx::write.xlsx(wirss_rel_up$wirs(), 
                           file, 
                           sheetName = "Stress Relativ upp", 
                           col.names = TRUE, 
                           row.names = FALSE, 
                           append = TRUE)
          xlsx::write.xlsx(wirss_rel_down$wirs(), 
                           file, 
                           sheetName = "Stress Relativ ned", 
                           col.names = TRUE, 
                           row.names = FALSE, 
                           append = TRUE)
        }
      )
      
    }
    if (req(input$nav) == "tab_module_solvencymod"){
      shinyjs::removeClass(selector = "body", class = "control-sidebar-open")
      output$right_side_bar <- renderUI({ div() })
    }
    if (req(input$nav) == "tab_module_esg"){
      shinyjs::removeClass(selector = "body", class = "control-sidebar-open")
      output$right_side_bar <- renderUI({ div() })
    }
  })
  

}

shinyApp(ui, server)