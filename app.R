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
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap2.css")
    ),
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
                  textInput("swaprate", "Marknadsnoteringar", "0.00075, 0.00048, 0.00045, 0.00055, 0.00075, 0.001, 0.00133, 0.00168, 0.00208, 0.00253, NA, 0.00333, NA, NA, 0.0042, NA, NA, NA, NA, 0.005", 
                            width = "100%"),
                  #-0.00095, -0.001, -0.00103, -0.00083, -0.00045, 0.00005, 0.00065, 0.00125, 0.00185, 0.00243, NA, 0.00348, NA, NA, 0.00465, NA, NA, NA, NA, 0.00578
                  #0.013200, 0.015275, 0.0177, 0.02008, 0.02208, 0.023630, 0.0249, 0.025930, 0.02678, 0.02745, NA, 0.028450, NA, NA, 0.029400, NA, NA, NA, NA, 0.0304
                  #0.00075, 0.00048, 0.00045, 0.00055, 0.00075,	0.001, 0.00133,	0.00168, 0.00208,	0.00253, NA, 0.00333, NA, NA, 0.0042, NA, NA, NA, NA,	0.005
                  #0.00178, 0.0017, 0.00178, 0.00183, 0.00213, 0.0025, 0.00293, 0.00335, 0.0038, 0.00425, NA, 0.00515, NA, NA, 0.00613, NA, NA, NA, NA, 0.00705
                  tags$p("Skriv in marknadsnoteringar för swapräntor. NA står för Not Available. NA behöver skrivas in för noteringar som saknas upp till den sista kända marknadsnoterade swapräntan. Den
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
                      tabPanel("FFFS 2013:23", plotOutput("zcr_curve_all_2013")),
                      tabPanel("FFFS 2019:21 Ordinarie", plotOutput("zcr_curve_all_ordinary")),
                      tabPanel("FFFS 2019:21 Tillfällig", plotOutput("zcr_curve_all_temp"))
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
                      tabPanel("FFFS 2013:23",
                               tags$br(),
                               mainPanel(
                                 tabsetPanel(
                                   type = "tabs",
                                   tabPanel("Tjänstepensionskurvan", 
                                            weightedInterestRateSwapUI("no_negative_2013", 
                                                                       selected = list(stress_type = "no_negative"))),
                                   tabPanel("Räntehöjningschock 100bps (absolut)", 
                                            weightedInterestRateSwapUI("abs_up_100_2013", 
                                                                       selected = list(stress_type = "absolute_up_100bps"))),
                                   tabPanel("Räntesänkningschock 100bps (absolut)", 
                                            weightedInterestRateSwapUI("abs_down_100_2013", 
                                                                       selected = list(stress_type = "absolute_down_100bps"))),
                                   tabPanel("Räntehöjningschock 50 bps (absolut)", 
                                            weightedInterestRateSwapUI("abs_up_50_2013", 
                                                                       selected = list(stress_type = "absolute_up_50bps"))),
                                   tabPanel("Räntesänkningschock 50 bps (absolut)", 
                                            weightedInterestRateSwapUI("abs_down_50_2013",
                                                                       selected = list(stress_type = "absolute_down_50bps"))),
                                   tabPanel("Otransformerad", 
                                            weightedInterestRateSwapUI("identity_2013", 
                                                                       selected = list(stress_type = "identity")))
                                 ),
                                 width = 12
                               )
                      ),
                      tabPanel("FFFS 2019:21 Ordinarie", 
                               tags$br(),
                               mainPanel(
                                 tabsetPanel(
                                   type = "tabs",
                                   tabPanel("Tjänstepensionskurvan", weightedInterestRateSwapUI("identity_ordinary")),
                                   tabPanel("Räntehöjningschock (absolut)", 
                                            weightedInterestRateSwapUI("abs_up_ordinary", 
                                                                       selected = list(stress_type = "absolute_up"))),
                                   tabPanel("Räntesänkningschock (absolut)", 
                                            weightedInterestRateSwapUI("abs_down_ordinary", 
                                                                       selected = list(stress_type = "absolute_down"))),
                                   tabPanel("Räntehöjningschock (relativ)", 
                                            weightedInterestRateSwapUI("rel_up_ordinary", 
                                                                       selected = list(stress_type = "relative_up"))),
                                   tabPanel("Räntesänkningschock (relativ)", 
                                            weightedInterestRateSwapUI("rel_down_ordinary",
                                                                       selected = list(stress_type = "relative_down")))
                                 ),
                                 width = 12
                               )
                      ),
                      tabPanel("FFFS 2019:21 Tillfällig",
                               tags$br(),
                               mainPanel(
                                 tabsetPanel(
                                   type = "tabs",
                                   tabPanel("Tjänstepensionskurvan", weightedInterestRateSwapUI("identity_temp")),
                                   tabPanel("Räntehöjningschock (absolut)", 
                                            weightedInterestRateSwapUI("abs_up_temp", 
                                                                       selected = list(stress_type = "absolute_up"))),
                                   tabPanel("Räntesänkningschock (absolut)", 
                                            weightedInterestRateSwapUI("abs_down_temp", 
                                                                       selected = list(stress_type = "absolute_down"))),
                                   tabPanel("Räntehöjningschock (relativ)", 
                                            weightedInterestRateSwapUI("rel_up_temp", 
                                                                       selected = list(stress_type = "relative_up"))),
                                   tabPanel("Räntesänkningschock (relativ)", 
                                            weightedInterestRateSwapUI("rel_down_temp",
                                                                       selected = list(stress_type = "relative_down")))
                                 ),
                                 width = 12
                               )
                      )
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
          tags$p(tags$i("FFFS 2013:23")),
          numericInput("credit_risk_adjustment_2013", "Kreditriskjustering", -0.0035, step = 1/10000),
          numericInput("ufr_2013", "L\u00E5ngsiktig terminsr\u00E4nta", 0.042, step = 1/10000),
          sliderInput("maximum_maturity_2013", "L\u00E4ngsta l\u00F6ptid", min = 20, max = 150, value = 100),
          tags$hr(),
          tags$p(tags$i("FFFS 2019:21 Ordinarie")),
          numericInput("credit_risk_adjustment", "Kreditriskjustering", -0.0015, step = 1/10000),
          numericInput("ufr", "L\u00E5ngsiktig terminsr\u00E4nta", 0.0375, step = 1/10000),
          sliderInput("maximum_maturity", "L\u00E4ngsta l\u00F6ptid", min = 20, max = 150, value = 100),
          tags$hr(),
          tags$p(tags$i("FFFS 2019:21 Tillfällig")),
          numericInput("credit_risk_adjustment_temp", "Kreditriskjustering", -0.0015, step = 1/10000),
          numericInput("ufr_temp", "L\u00E5ngsiktig terminsr\u00E4nta", 0.042, step = 1/10000),
          sliderInput("maximum_maturity_temp", "L\u00E4ngsta l\u00F6ptid", min = 20, max = 150, value = 100),
          tags$hr()
        )
      })
      
      output$right_side_bar_downloads <- renderUI({
        tagList(
          tags$p(tags$i("Excelformat")),
          downloadButton("downloadZeroCouponRatesPerFFFS", "För publicering"),
          tags$hr(),
          tags$p(tags$i("Excelformat")),
          downloadButton("downloadDatasetWeightedInterestRateSwap", "Alla tabeller")
        )
      })
      
      # Modules
      # Should construct nested modules rather than the following when time permits...
      
      # 2013 23
      wirss_identity_2013 <- callModule(weightedInterestRateSwapServer, 
                                        "identity_2013", 
                                        swaprate = reactive({req(input$swaprate)}), 
                                        maximum_maturity = reactive({req(input$maximum_maturity_2013)}), 
                                        credit_risk_adjustment = reactive({input$credit_risk_adjustment_2013}),
                                        ufr = reactive({input$ufr_2013})
      )
      wirss_no_negative_2013 <- callModule(weightedInterestRateSwapServer, 
                                           "no_negative_2013", 
                                           swaprate = reactive({req(input$swaprate)}), 
                                           maximum_maturity = reactive({req(input$maximum_maturity_2013)}), 
                                           credit_risk_adjustment = reactive({input$credit_risk_adjustment_2013}),
                                           ufr = reactive({input$ufr_2013})
      )
      wirss_abs_up_100_2013 <- callModule(weightedInterestRateSwapServer, 
                                          "abs_up_100_2013", 
                                          swaprate = reactive({req(input$swaprate)}), 
                                          maximum_maturity = reactive({req(input$maximum_maturity_2013)}),
                                          credit_risk_adjustment = reactive({input$credit_risk_adjustment_2013}),
                                          ufr = reactive({input$ufr_2013})
      )
      wirss_abs_down_100_2013  <- callModule(weightedInterestRateSwapServer, 
                                             "abs_down_100_2013", 
                                             swaprate = reactive({req(input$swaprate)}), 
                                             maximum_maturity = reactive({req(input$maximum_maturity_2013)}),
                                             credit_risk_adjustment = reactive({input$credit_risk_adjustment_2013}),
                                             ufr = reactive({input$ufr_2013})
      )
      wirss_abs_up_50_2013  <- callModule(weightedInterestRateSwapServer, 
                                          "abs_up_50_2013", 
                                          swaprate = reactive({req(input$swaprate)}), 
                                          maximum_maturity = reactive({req(input$maximum_maturity_2013)}),
                                          credit_risk_adjustment = reactive({input$credit_risk_adjustment_2013}),
                                          ufr = reactive({input$ufr_2013})
      )
      wirss_abs_down_50_2013  <- callModule(weightedInterestRateSwapServer, 
                                            "abs_down_50_2013",
                                            swaprate = reactive({req(input$swaprate)}), 
                                            maximum_maturity = reactive({req(input$maximum_maturity_2013)}),
                                            credit_risk_adjustment = reactive({input$credit_risk_adjustment_2013}),
                                            ufr = reactive({input$ufr_2013})
      )
      
      # 2019 23 Ordinarie
      wirss_identity_ordinary <- callModule(weightedInterestRateSwapServer, 
                                            "identity_ordinary", 
                                            swaprate = reactive({req(input$swaprate)}), 
                                            maximum_maturity = reactive({req(input$maximum_maturity)}), 
                                            credit_risk_adjustment = reactive({input$credit_risk_adjustment}),
                                            ufr = reactive({input$ufr})
      )
      wirss_abs_up_ordinary <- callModule(weightedInterestRateSwapServer, 
                                          "abs_up_ordinary", 
                                          swaprate = reactive({req(input$swaprate)}), 
                                          maximum_maturity = reactive({req(input$maximum_maturity)}),
                                          credit_risk_adjustment = reactive({input$credit_risk_adjustment}),
                                          ufr = reactive({input$ufr})
      )
      wirss_abs_down_ordinary <- callModule(weightedInterestRateSwapServer, 
                                            "abs_down_ordinary", 
                                            swaprate = reactive({req(input$swaprate)}), 
                                            maximum_maturity = reactive({req(input$maximum_maturity)}),
                                            credit_risk_adjustment = reactive({input$credit_risk_adjustment}),
                                            ufr = reactive({input$ufr})
      )
      wirss_rel_up_ordinary <- callModule(weightedInterestRateSwapServer, 
                                          "rel_up_ordinary", 
                                          swaprate = reactive({req(input$swaprate)}), 
                                          maximum_maturity = reactive({req(input$maximum_maturity)}),
                                          credit_risk_adjustment = reactive({input$credit_risk_adjustment}),
                                          ufr = reactive({input$ufr})
      )
      wirss_rel_down_ordinary <- callModule(weightedInterestRateSwapServer, 
                                            "rel_down_ordinary", 
                                            swaprate = reactive({req(input$swaprate)}), 
                                            maximum_maturity = reactive({req(input$maximum_maturity)}),
                                            credit_risk_adjustment = reactive({input$credit_risk_adjustment}),
                                            ufr = reactive({input$ufr})
      )
      
      # 2019 Temporär
      wirss_identity_temp <- callModule(weightedInterestRateSwapServer, 
                                        "identity_temp", 
                                        swaprate = reactive({req(input$swaprate)}), 
                                        maximum_maturity = reactive({req(input$maximum_maturity_temp)}), 
                                        credit_risk_adjustment = reactive({input$credit_risk_adjustment_temp}),
                                        ufr = reactive({input$ufr_temp})
      )
      wirss_abs_up_temp <- callModule(weightedInterestRateSwapServer, 
                                      "abs_up_temp", 
                                      swaprate = reactive({req(input$swaprate)}), 
                                      maximum_maturity = reactive({req(input$maximum_maturity_temp)}),
                                      credit_risk_adjustment = reactive({input$credit_risk_adjustment_temp}),
                                      ufr = reactive({input$ufr_temp})
      )
      wirss_abs_down_temp <- callModule(weightedInterestRateSwapServer, 
                                        "abs_down_temp", 
                                        swaprate = reactive({req(input$swaprate)}), 
                                        maximum_maturity = reactive({req(input$maximum_maturity_temp)}),
                                        credit_risk_adjustment = reactive({input$credit_risk_adjustment_temp}),
                                        ufr = reactive({input$ufr_temp})
      )
      wirss_rel_up_temp <- callModule(weightedInterestRateSwapServer, 
                                      "rel_up_temp", 
                                      swaprate = reactive({req(input$swaprate)}), 
                                      maximum_maturity = reactive({req(input$maximum_maturity_temp)}),
                                      credit_risk_adjustment = reactive({input$credit_risk_adjustment_temp}),
                                      ufr = reactive({input$ufr_temp})
      )
      wirss_rel_down_temp <- callModule(weightedInterestRateSwapServer, 
                                        "rel_down_temp", 
                                        swaprate = reactive({req(input$swaprate)}), 
                                        maximum_maturity = reactive({req(input$maximum_maturity_temp)}),
                                        credit_risk_adjustment = reactive({input$credit_risk_adjustment_temp}),
                                        ufr = reactive({input$ufr_temp})
      )
      
      # Plots
      output$zcr_curve_all_2013 <- renderPlot({
        req(input$swaprate, input$maximum_maturity_2013, input$credit_risk_adjustment_2013, input$ufr_2013)
        par(bg = NA)
        col <- c("black", wes_palette("Zissou1", 5))
        data <- cbind(wirss_no_negative_2013$wirs()[ , 8], 
                      wirss_abs_up_100_2013$wirs()[ , 8], 
                      wirss_abs_down_100_2013$wirs()[ , 8], 
                      wirss_abs_up_50_2013$wirs()[ , 8], 
                      wirss_abs_down_50_2013$wirs()[ , 8],
                      wirss_identity_2013$wirs()[ , 8])
        plot(data[, 1], 
             xlab = "L\u00F6ptid", 
             ylab = "Nollkupongr\u00E4nta", 
             type = "o", 
             main = "Diskonteringsr\u00E4ntekurvor", 
             ylim = 1.10*c(min(data), max(data)),
             pch = 0)
        lapply(1:5, function(x) {lines(data[ , x+1], type = "o", col = wes_palette("Zissou1", 5)[x], pch = x)})
        legend("topleft", 
               legend = c("Tjänstepensionskurvan", 
                          "Absolut räntehöjningschock 100bps",
                          "Absolut räntesänkningschock 100bps",
                          "Absolut räntehöjningschock 50bps",
                          "Absolut räntesänkningschock 50bps",
                          "Otransformerad"),
               border = "NA",
               col = col,
               bty = "n",
               lty = 1,
               pch = 0:5,
               bg = "white",
               inset = 0.01
        )
        grid(lty = "dotted", lwd = 0.5)
      })
      
      output$zcr_curve_all_ordinary <- renderPlot({
        req(input$swaprate, input$maximum_maturity, input$credit_risk_adjustment, input$ufr)
        par(bg = NA)
        col <- c("black", wes_palette("Zissou1", 4))
        data <- cbind(wirss_identity_ordinary$wirs()[ , 8], 
                      wirss_abs_up_ordinary$wirs()[ , 8], 
                      wirss_abs_down_ordinary$wirs()[ , 8], 
                      wirss_rel_up_ordinary$wirs()[ , 8], 
                      wirss_rel_down_ordinary$wirs()[ , 8])
        plot(data[, 1], 
             xlab = "L\u00F6ptid", 
             ylab = "Nollkupongr\u00E4nta", 
             type = "o", 
             main = "Diskonteringsr\u00E4ntekurvor", 
             ylim = 1.10*c(min(data), max(data)),
             pch = 0)
        lapply(1:4, function(x) {lines(data[ , x+1], type = "o", col = wes_palette("Zissou1", 4)[x], pch = x)})
        legend("topleft", 
               legend = c("Tjänstepensionskurvan", 
                          "Absolut räntehöjningschock",
                          "Absolut räntesänkningschock",
                          "Relativ räntehöjningschock",
                          "Relativ räntesänkningschock"),
               border = "NA",
               col = col,
               bty = "n",
               lty = 1,
               pch = 0:4,
               bg = "white",
               inset = 0.01
        )
        grid(lty = "dotted", lwd = 0.5)
      })
      
      output$zcr_curve_all_temp <- renderPlot({
        req(input$swaprate, input$maximum_maturity_temp, input$credit_risk_adjustment_temp, input$ufr_temp)
        par(bg = NA)
        col <- c("black", wes_palette("Zissou1", 4))
        data <- cbind(wirss_identity_temp$wirs()[ , 8], 
                      wirss_abs_up_temp$wirs()[ , 8], 
                      wirss_abs_down_temp$wirs()[ , 8], 
                      wirss_rel_up_temp$wirs()[ , 8], 
                      wirss_rel_down_temp$wirs()[ , 8])
        plot(data[, 1], 
             xlab = "L\u00F6ptid", 
             ylab = "Nollkupongr\u00E4nta", 
             type = "o", 
             main = "Diskonteringsr\u00E4ntekurvor", 
             ylim = 1.10*c(min(data), max(data)),
             pch = 0)
        lapply(1:4, function(x) {lines(data[ , x+1], type = "o", col = wes_palette("Zissou1", 4)[x], pch = x)})
        legend("topleft", 
               legend = c("Tjänstepensionskurvan", 
                          "Absolut räntehöjningschock",
                          "Absolut räntesänkningschock",
                          "Relativ räntehöjningschock",
                          "Relativ räntesänkningschock"),
               border = "NA",
               col = col,
               bty = "n",
               lty = 1,
               pch = 0:4,
               bg = "white",
               inset = 0.01
        )
        grid(lty = "dotted", lwd = 0.5)
      })
      
      # Data frames
      zeroCouponRatesPerFFFS <- reactive({
        datafr_FFFS_2013 <- data.frame(wirss_no_negative_2013$wirs()[ , 8], 
                                       wirss_abs_down_100_2013$wirs()[ , 8],
                                       wirss_abs_up_100_2013$wirs()[ , 8], 
                                       wirss_abs_down_50_2013$wirs()[ , 8],  
                                       wirss_abs_up_50_2013$wirs()[ , 8])
        names(datafr_FFFS_2013) <- c("Tjänstepensionskurvan", 
                                     "Stressad kurva, nedåt 100bp", 
                                     "Stressad kurva, uppåt 100bp",
                                     "Stressad kurva, nedåt 50bp",
                                     "Stressad kurva, uppåt 50bp")
        datafr_FFFS_2019_ordinary <- data.frame(wirss_identity_ordinary$wirs()[ , 8], 
                                                wirss_abs_down_ordinary$wirs()[ , 8], 
                                                wirss_abs_up_ordinary$wirs()[ , 8], 
                                                wirss_rel_down_ordinary$wirs()[ , 8],
                                                wirss_rel_up_ordinary$wirs()[ , 8])
        names(datafr_FFFS_2019_ordinary) <- c("Tjänstepensionskurvan", 
                                              "Stressad kurva, nedåt absolut", 
                                              "Stressad kurva, uppåt absolut",
                                              "Stressad kurva, nedåt relativ",
                                              "Stressad kurva, uppåt relativ")
        datafr_FFFS_2019_temp <- data.frame(wirss_identity_temp$wirs()[ , 8], 
                                            wirss_abs_down_temp$wirs()[ , 8], 
                                            wirss_abs_up_temp$wirs()[ , 8], 
                                            wirss_rel_down_temp$wirs()[ , 8],
                                            wirss_rel_up_temp$wirs()[ , 8])
        names(datafr_FFFS_2019_temp) <- c("Tjänstepensionskurvan", 
                                          "Stressad kurva, nedåt absolut", 
                                          "Stressad kurva, uppåt absolut",
                                          "Stressad kurva, nedåt relativ",
                                          "Stressad kurva, uppåt relativ")
        return(list(datafr_FFFS_2013 = datafr_FFFS_2013,
                    datafr_FFFS_2019_ordinary = datafr_FFFS_2019_ordinary,
                    datafr_FFFS_2019_temp = datafr_FFFS_2019_temp)
        )
      })
      
      # Downloads
      output$downloadZeroCouponRatesPerFFFS <- downloadHandler(
        filename = function() {
          paste("Diskonteringsräntekurvor för publicering", ".xlsx", sep = "")
        },
        content = function(file) {
          xlsx::write.xlsx(zeroCouponRatesPerFFFS()$datafr_FFFS_2013, 
                           file, 
                           sheetName = "FFFS 2013 23", 
                           col.names = TRUE, 
                           row.names = FALSE,
                           showNA = FALSE,
                           append = FALSE)
          xlsx::write.xlsx(zeroCouponRatesPerFFFS()$datafr_FFFS_2019_ordinary, 
                           file, 
                           sheetName = "FFFS 2019 21 Ordinarie", 
                           col.names = TRUE, 
                           row.names = FALSE,
                           showNA = FALSE,
                           append = TRUE)
          xlsx::write.xlsx(zeroCouponRatesPerFFFS()$datafr_FFFS_2019_temp, 
                           file, 
                           sheetName = "FFFS 2019 21 Tillfällig", 
                           col.names = TRUE, 
                           row.names = FALSE,
                           showNA = FALSE,
                           append = TRUE)
        }
      )
      
      output$downloadDatasetWeightedInterestRateSwap <- downloadHandler(
        filename = function() {
          paste("All_data", ".xlsx", sep = "")
        },
        content = function(file) {
          xlsx::write.xlsx(wirss_identity_ordinary$wirs(), 
                           file, 
                           sheetName = "Otransformerad", 
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