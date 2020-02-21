# Load packages
library(shiny)
#library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)

# Load separate module and function scripts
source("functions_drc.R")
source("module_drc.R")

# User interface
ui <- dashboardPagePlus(
  skin = "black-light",
  header = dashboardHeaderPlus(
    title = "Verktyg",
    enable_rightsidebar = TRUE,
    rightSidebarIcon = "info-circle"
  ),
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("Räntekurvsberäkningar", tabName = "tab_module_drc", icon = icon("calculator", class= "fas", lib = "font-awesome")),
      menuItem("Solvensmodellen", tabName = "tab_module_solvencymod", icon = icon("calculator", class= "fas", lib = "font-awesome")),
      menuItem("ESG-Analysverktyg", tabName = "tab_module_esg", icon = icon("calculator", class= "fas", lib = "font-awesome"))
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
                  width = 10,
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
                  width = 10,
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
                  width = 10,
                  collapsible = TRUE,
                  plotOutput("zcr_curve_all")
                )
              ),
              fluidRow(
                box(
                  title = "Modul",
                  status = "primary",
                  width = 10,
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
    background = "dark",
    title = "Sidebar",
    rightSidebarTabContent(
      id = 1,
      icon = "desktop",
      title = "Tab 1",
      active = TRUE,
      sliderInput(
        "obs",
        "Number of observations:",
        min = 0, max = 1000, value = 500
      )
    )
  ),
  footer = dashboardFooter(
    
  )
)

# Server logic
server <- function(input, output, session) {
  sr <- reactive({input$swaprate})
  
  # Modules
  wirss_none <- callModule(weightedInterestRateSwapServer, "none", swaprate = sr)
  wirss_abs_up <- callModule(weightedInterestRateSwapServer, "abs_up", swaprate = sr)
  wirss_abs_down <- callModule(weightedInterestRateSwapServer, "abs_down", swaprate = sr)
  wirss_rel_up <- callModule(weightedInterestRateSwapServer, "rel_up", swaprate = sr)
  wirss_rel_down <- callModule(weightedInterestRateSwapServer, "rel_down", swaprate = sr)
  
  # Plots
  output$zcr_curve_all <- renderPlot({
    par(bg = NA)
    data <- wirss_none$wirs()
    plot(data[, 8], xlab = "L\u00F6ptid", ylab = "Nollkupongr\u00E4nta", type = "p", main = "Diskonteringsr\u00E4ntekurvor")
    lines(data[, 8])
    grid(lty = "dotted")
  })
}

shinyApp(ui, server)
