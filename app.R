# Load packages
# packages <- c("shiny", "shinyWidgets", "shinydashboard", "shinydashboardPlus", "shinyjs", "DT", "viridis", "openxlsx")
# 
# for (package in packages) {
#   if (!require(package, character.only = TRUE, quietly = FALSE)) {
#     install.packages(package)
#     library(package, character.only = TRUE)
#   }
# }

library("shiny")
library("markdown")
library("shinyWidgets")
library("shinydashboard")
library("shinydashboardPlus")
library("shinyjs")
library("DT")
library("viridis")
library("openxlsx")

# Load separate module and function scripts
source("functions_drc.R")
source("module_drc.R")

# User interface
ui <- dashboardPagePlus(
  skin = "black",
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
    tabItems(
      tabItem(tabName = "tab_module_drc",
              fluidRow(
                gradientBox(
                  title = "Diskonteringsräntekurvor för Solvens I",
                  status = "info",
                  icon = "fa fa-th",
                  gradientColor = "maroon", 
                  boxToolSize = "xs", 
                  width = 12,
                  collapsible = TRUE,
                  closable = TRUE,
                  includeMarkdown("markdown/introduktion.md")
                ),
                boxPlus(
                  title = "Information om diskonteringsräntekurvor som publiceras", 
                  status = "info",
                  width = 12,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  closable = FALSE,
                  includeMarkdown("markdown/information.md")
                ),
                boxPlus(
                  title = "Lathund", 
                  status = "info",
                  width = 12,
                  collapsible = TRUE,
                  collapsed = FALSE,
                  closable = FALSE,
                  includeMarkdown("markdown/lathund.md")
                ),
                boxPlus(
                  title = "Beräkningsmetodik", 
                  status = "info",
                  width = 12,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  closable = FALSE,
                  includeMarkdown("markdown/math_theory.md")
                ),
                boxPlus(
                  title = "Marknadsnoteringar för swapräntor", 
                  status = "warning",
                  width = 12,
                  collapsible = TRUE,
                  closable = FALSE,
                  fluidPage(
                    fluidRow(
                      column(3, dateInput("drc_date", label = "Datum", value = Sys.Date(), language = "sv")),
                      column(9, textInput("swaprate", "Marknadsnoteringar", "0.00075, 0.00048, 0.00045, 0.00055, 0.00075, 0.001, 0.00133, 0.00168, 0.00208, 0.00253, NA, 0.00333, NA, NA, 0.0042, NA, NA, NA, NA, 0.005", width = "100%"))
                    ),
                    fluidRow(
                      includeMarkdown("markdown/helptext_input.md")
                    )
                  )
                ),
                boxPlus(
                  title = "Figurer",
                  status = "primary",
                  width = 12,
                  collapsible = TRUE,
                  closable = FALSE,
                  mainPanel(
                    tabsetPanel(
                      type = "tabs",
                      tabPanel("FFFS 2013:23", plotOutput("zcr_curve_all_2013")),
                      tabPanel("FFFS 2019:21 Ordinarie", plotOutput("zcr_curve_all_ordinary")),
                      tabPanel("FFFS 2019:21 Tillfällig", plotOutput("zcr_curve_all_temp"))
                    ),
                    width = 12
                  )
                ),
                boxPlus(
                  title = "Tabeller",
                  status = "primary",
                  width = 12,
                  collapsible = TRUE,
                  closable = FALSE,
                  mainPanel(
                    tabsetPanel(
                      type = "tabs",
                      tabPanel("FFFS 2013:23",
                               tags$br(),
                               tabsetPanel(
                                 type = "pills",
                                 tabPanel("Tjänstepensionskurvan", weightedInterestRateSwapUI("no_negative_2013", selected = list(stress_type = "no_negative"))),
                                 tabPanel("Annan Försäkring", weightedInterestRateSwapUI("other_2013", selected = list(stress_type = "no_negative"))),
                                 tabPanel("Räntehöjningschock 100bps", weightedInterestRateSwapUI("abs_up_100_2013", selected = list(stress_type = "absolute_up_100bps"))),
                                 tabPanel("Räntesänkningschock 100bps", weightedInterestRateSwapUI("abs_down_100_2013", selected = list(stress_type = "absolute_down_100bps"))),
                                 tabPanel("Räntehöjningschock 50 bps", weightedInterestRateSwapUI("abs_up_50_2013", selected = list(stress_type = "absolute_up_50bps"))),
                                 tabPanel("Räntesänkningschock 50 bps", weightedInterestRateSwapUI("abs_down_50_2013", selected = list(stress_type = "absolute_down_50bps"))),
                                 tabPanel("Otransformerad", weightedInterestRateSwapUI("identity_2013", selected = list(stress_type = "identity")))
                               ),
                               width = 12
                      ),
                      tabPanel("FFFS 2019:21 Ordinarie", 
                               tags$br(),
                               tabsetPanel(
                                 type = "pills",
                                 tabPanel("Tjänstepensionskurvan", weightedInterestRateSwapUI("identity_ordinary")),
                                 tabPanel("Räntehöjningschock (absolut)", weightedInterestRateSwapUI("abs_up_ordinary", selected = list(stress_type = "absolute_up"))),
                                 tabPanel("Räntesänkningschock (absolut)", weightedInterestRateSwapUI("abs_down_ordinary", selected = list(stress_type = "absolute_down"))),
                                 tabPanel("Räntehöjningschock (relativ)", weightedInterestRateSwapUI("rel_up_ordinary", selected = list(stress_type = "relative_up"))),
                                 tabPanel("Räntesänkningschock (relativ)", weightedInterestRateSwapUI("rel_down_ordinary", selected = list(stress_type = "relative_down")))
                               ),
                               width = 12
                      ),
                      tabPanel("FFFS 2019:21 Tillfällig",
                               tags$br(),
                               tabsetPanel(
                                 type = "pills",
                                 tabPanel("Tjänstepensionskurvan", weightedInterestRateSwapUI("identity_temp")),
                                 tabPanel("Räntehöjningschock (absolut)", weightedInterestRateSwapUI("abs_up_temp", selected = list(stress_type = "absolute_up"))),
                                 tabPanel("Räntesänkningschock (absolut)", weightedInterestRateSwapUI("abs_down_temp", selected = list(stress_type = "absolute_down"))),
                                 tabPanel("Räntehöjningschock (relativ)", weightedInterestRateSwapUI("rel_up_temp", selected = list(stress_type = "relative_up"))),
                                 tabPanel("Räntesänkningschock (relativ)", weightedInterestRateSwapUI("rel_down_temp", selected = list(stress_type = "relative_down")))
                               ),
                               width = 12
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
      uiOutput("right_side_bar"),
      #weightedInterestRateSwap_ui_controls("test")
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
    right_text = "Senast uppdaterad: Maj 2020"
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
          numericInput("shift_2013", "Kreditriskjustering", -0.0035, step = 1/10000),
          numericInput("ufr_2013", "L\u00E5ngsiktig terminsr\u00E4nta", 0.042, step = 1/10000),
          sliderInput("maximum_maturity_2013", "L\u00E4ngsta l\u00F6ptid", min = 20, max = 150, value = 100),
          tags$hr(),
          tags$p(tags$i("FFFS 2019:21 Ordinarie")),
          numericInput("shift", "Avdrag", -0.0015, step = 1/10000),
          numericInput("ufr", "L\u00E5ngsiktig terminsr\u00E4nta", 0.0375, step = 1/10000),
          sliderInput("maximum_maturity", "L\u00E4ngsta l\u00F6ptid", min = 20, max = 150, value = 100),
          tags$hr(),
          tags$p(tags$i("FFFS 2019:21 Tillfällig")),
          numericInput("shift_temp", "Avdrag", -0.0015, step = 1/10000),
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
          tags$p(tags$i("All data från tabellerna i excelformat")),
          downloadButton("downloadDatasetWeightedInterestRateSwap_FFFS_2013", "FFFS 2013:23"),
          tags$br(),
          tags$br(),
          downloadButton("downloadDatasetWeightedInterestRateSwap_FFFS_2019_ordinary", "FFFS 2019:21 Ordinarie"),
          tags$br(),
          tags$br(),
          downloadButton("downloadDatasetWeightedInterestRateSwap_FFFS_2019_temp", "FFFS 2019:21 Tillfällig")
        )
      })
      
      # Modules
      # Should construct nested modules rather than the following when time permits...
      
      # test <- callModule(weightedInterestRateSwapServer, 
      #                    "test", 
      #                    swaprate = reactive({req(input$swaprate)}), 
      #                    maximum_maturity = reactive({req(input$maximum_maturity_test)}), 
      #                    shift = reactive({req(input$shift_test)}),
      #                    ufr = reactive({req(input$ufr_test)})
      # )
      
      # 2013:23
      wirss_identity_2013 <- callModule(weightedInterestRateSwapServer, 
                                        "identity_2013", 
                                        swaprate = reactive({req(input$swaprate)}), 
                                        maximum_maturity = reactive({req(input$maximum_maturity_2013)}), 
                                        shift = reactive({req(input$shift_2013)}),
                                        ufr = reactive({req(input$ufr_2013)})
      )
      wirss_no_negative_2013 <- callModule(weightedInterestRateSwapServer, 
                                           "no_negative_2013", 
                                           swaprate = reactive({req(input$swaprate)}), 
                                           maximum_maturity = reactive({req(input$maximum_maturity_2013)}), 
                                           shift = reactive({req(input$shift_2013)}),
                                           ufr = reactive({req(input$ufr_2013)})
      )
      wirss_other_2013 <- callModule(weightedInterestRateSwapServer, 
                                     "other_2013", 
                                     swaprate = reactive({req(input$swaprate)}), 
                                     maximum_maturity = reactive({req(input$maximum_maturity_2013)}), 
                                     shift = reactive({req(input$shift_2013-20/10000)}),
                                     ufr = reactive({req(input$ufr_2013)})
      )
      wirss_abs_up_100_2013 <- callModule(weightedInterestRateSwapServer, 
                                          "abs_up_100_2013", 
                                          swaprate = reactive({req(input$swaprate)}), 
                                          maximum_maturity = reactive({req(input$maximum_maturity_2013)}),
                                          shift = reactive({req(input$shift_2013)}),
                                          ufr = reactive({req(input$ufr_2013)})
      )
      wirss_abs_down_100_2013  <- callModule(weightedInterestRateSwapServer, 
                                             "abs_down_100_2013", 
                                             swaprate = reactive({req(input$swaprate)}), 
                                             maximum_maturity = reactive({req(input$maximum_maturity_2013)}),
                                             shift = reactive({req(input$shift_2013)}),
                                             ufr = reactive({req(input$ufr_2013)})
      )
      wirss_abs_up_50_2013  <- callModule(weightedInterestRateSwapServer, 
                                          "abs_up_50_2013", 
                                          swaprate = reactive({req(input$swaprate)}), 
                                          maximum_maturity = reactive({req(input$maximum_maturity_2013)}),
                                          shift = reactive({req(input$shift_2013)}),
                                          ufr = reactive({req(input$ufr_2013)})
      )
      wirss_abs_down_50_2013  <- callModule(weightedInterestRateSwapServer, 
                                            "abs_down_50_2013",
                                            swaprate = reactive({req(input$swaprate)}), 
                                            maximum_maturity = reactive({req(input$maximum_maturity_2013)}),
                                            shift = reactive({req(input$shift_2013)}),
                                            ufr = reactive({req(input$ufr_2013)})
      )
      
      # 2019:23 Ordinarie
      wirss_identity_ordinary <- callModule(weightedInterestRateSwapServer, 
                                            "identity_ordinary", 
                                            swaprate = reactive({req(input$swaprate)}), 
                                            maximum_maturity = reactive({req(input$maximum_maturity)}), 
                                            shift = reactive({req(input$shift)}),
                                            ufr = reactive({req(input$ufr)})
      )
      wirss_abs_up_ordinary <- callModule(weightedInterestRateSwapServer, 
                                          "abs_up_ordinary", 
                                          swaprate = reactive({req(input$swaprate)}), 
                                          maximum_maturity = reactive({req(input$maximum_maturity)}),
                                          shift = reactive({req(input$shift)}),
                                          ufr = reactive({req(input$ufr)})
      )
      wirss_abs_down_ordinary <- callModule(weightedInterestRateSwapServer, 
                                            "abs_down_ordinary", 
                                            swaprate = reactive({req(input$swaprate)}), 
                                            maximum_maturity = reactive({req(input$maximum_maturity)}),
                                            shift = reactive({req(input$shift)}),
                                            ufr = reactive({req(input$ufr)})
      )
      wirss_rel_up_ordinary <- callModule(weightedInterestRateSwapServer, 
                                          "rel_up_ordinary", 
                                          swaprate = reactive({req(input$swaprate)}), 
                                          maximum_maturity = reactive({req(input$maximum_maturity)}),
                                          shift = reactive({req(input$shift)}),
                                          ufr = reactive({req(input$ufr)})
      )
      wirss_rel_down_ordinary <- callModule(weightedInterestRateSwapServer, 
                                            "rel_down_ordinary", 
                                            swaprate = reactive({req(input$swaprate)}), 
                                            maximum_maturity = reactive({req(input$maximum_maturity)}),
                                            shift = reactive({req(input$shift)}),
                                            ufr = reactive({req(input$ufr)})
      )
      
      # 2019:23 Temporär
      wirss_identity_temp <- callModule(weightedInterestRateSwapServer, 
                                        "identity_temp", 
                                        swaprate = reactive({req(input$swaprate)}), 
                                        maximum_maturity = reactive({req(input$maximum_maturity_temp)}), 
                                        shift = reactive({req(input$shift_temp)}),
                                        ufr = reactive({req(input$ufr_temp)})
      )
      wirss_abs_up_temp <- callModule(weightedInterestRateSwapServer, 
                                      "abs_up_temp", 
                                      swaprate = reactive({req(input$swaprate)}), 
                                      maximum_maturity = reactive({req(input$maximum_maturity_temp)}),
                                      shift = reactive({req(input$shift_temp)}),
                                      ufr = reactive({req(input$ufr_temp)})
      )
      wirss_abs_down_temp <- callModule(weightedInterestRateSwapServer, 
                                        "abs_down_temp", 
                                        swaprate = reactive({req(input$swaprate)}), 
                                        maximum_maturity = reactive({req(input$maximum_maturity_temp)}),
                                        shift = reactive({req(input$shift_temp)}),
                                        ufr = reactive({req(input$ufr_temp)})
      )
      wirss_rel_up_temp <- callModule(weightedInterestRateSwapServer, 
                                      "rel_up_temp", 
                                      swaprate = reactive({req(input$swaprate)}), 
                                      maximum_maturity = reactive({req(input$maximum_maturity_temp)}),
                                      shift = reactive({req(input$shift_temp)}),
                                      ufr = reactive({req(input$ufr_temp)})
      )
      wirss_rel_down_temp <- callModule(weightedInterestRateSwapServer, 
                                        "rel_down_temp", 
                                        swaprate = reactive({req(input$swaprate)}), 
                                        maximum_maturity = reactive({req(input$maximum_maturity_temp)}),
                                        shift = reactive({req(input$shift_temp)}),
                                        ufr = reactive({req(input$ufr_temp)})
      )
      
      # Plots
      output$zcr_curve_all_2013 <- renderPlot({
        req(input$swaprate, input$maximum_maturity_2013, input$shift_2013, input$ufr_2013)
        par(bg = NA)
        data <- cbind(wirss_no_negative_2013$wirs()[ , 8], 
                      wirss_other_2013$wirs()[ , 8],
                      wirss_abs_up_100_2013$wirs()[ , 8], 
                      wirss_abs_down_100_2013$wirs()[ , 8], 
                      wirss_abs_up_50_2013$wirs()[ , 8], 
                      wirss_abs_down_50_2013$wirs()[ , 8],
                      wirss_identity_2013$wirs()[ , 8])
        ncols <- dim(data)[2]
        col <- c("black", plasma(ncols))
        plot(data[, 1], 
             xlab = "L\u00F6ptid", 
             ylab = "Nollkupongr\u00E4nta", 
             type = "o", 
             main = "Diskonteringsr\u00E4ntekurvor", 
             ylim = 1.10*c(min(data), max(data)),
             pch = 1)
        lapply(2:ncols, function(x) {lines(data[ , x], type = "o", col = col[x], pch = x)})
        legend("topleft", 
               legend = c("Tjänstepensionskurvan", 
                          "Annan Försäkring",
                          "Absolut räntehöjningschock 100bps",
                          "Absolut räntesänkningschock 100bps",
                          "Absolut räntehöjningschock 50bps",
                          "Absolut räntesänkningschock 50bps",
                          "Otransformerad"),
               border = "NA",
               col = col,
               bty = "n",
               lty = 1,
               pch = 1:ncols,
               bg = "white",
               inset = 0.01
        )
        grid(lty = "dotted", lwd = 0.5)
      })
      
      output$zcr_curve_all_ordinary <- renderPlot({
        req(input$swaprate, input$maximum_maturity, input$shift, input$ufr)
        par(bg = NA)
        data <- cbind(wirss_identity_ordinary$wirs()[ , 8], 
                      wirss_abs_up_ordinary$wirs()[ , 8], 
                      wirss_abs_down_ordinary$wirs()[ , 8], 
                      wirss_rel_up_ordinary$wirs()[ , 8], 
                      wirss_rel_down_ordinary$wirs()[ , 8])
        ncols <- dim(data)[2]
        col <- c("black", plasma(ncols))
        plot(data[, 1], 
             xlab = "L\u00F6ptid", 
             ylab = "Nollkupongr\u00E4nta", 
             type = "o", 
             main = "Diskonteringsr\u00E4ntekurvor", 
             ylim = 1.10*c(min(data), max(data)),
             pch = 1)
        lapply(2:ncols, function(x) {lines(data[ , x], type = "o", col = col[x], pch = x)})
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
               pch = 1:ncols,
               bg = "white",
               inset = 0.01
        )
        grid(lty = "dotted", lwd = 0.5)
      })
      
      output$zcr_curve_all_temp <- renderPlot({
        req(input$swaprate, input$maximum_maturity_temp, input$shift_temp, input$ufr_temp)
        par(bg = NA)
        data <- cbind(wirss_identity_temp$wirs()[ , 8], 
                      wirss_abs_up_temp$wirs()[ , 8], 
                      wirss_abs_down_temp$wirs()[ , 8], 
                      wirss_rel_up_temp$wirs()[ , 8], 
                      wirss_rel_down_temp$wirs()[ , 8])
        ncols <- dim(data)[2]
        col <- c("black", plasma(ncols))
        plot(data[, 1], 
             xlab = "L\u00F6ptid", 
             ylab = "Nollkupongr\u00E4nta", 
             type = "o", 
             main = "Diskonteringsr\u00E4ntekurvor", 
             ylim = 1.10*c(min(data), max(data)),
             pch = 1)
        lapply(2:ncols, function(x) {lines(data[ , x], type = "o", col = col[x], pch = x)})
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
               pch = 1:ncols,
               bg = "white",
               inset = 0.01
        )
        grid(lty = "dotted", lwd = 0.5)
      })
      
      # Data frames
      zeroCouponRatesPerFFFS <- reactive({
        datafr_FFFS_2013 <- data.frame(wirss_no_negative_2013$wirs()[ , 8], 
                                       wirss_other_2013$wirs()[ , 8],
                                       wirss_abs_down_100_2013$wirs()[ , 8],
                                       wirss_abs_up_100_2013$wirs()[ , 8], 
                                       wirss_abs_down_50_2013$wirs()[ , 8],  
                                       wirss_abs_up_50_2013$wirs()[ , 8])
        names(datafr_FFFS_2013) <- c("Tjänstepensionskurvan", 
                                     "Annan Försäkring",
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
          paste("Diskonteringsräntekurvor", ".xlsx", sep = "")
        },
        content = function(file) {
          wb <- openxlsx::loadWorkbook("./data/template.xlsx")
          openxlsx::writeData(wb, 
                              sheet = "FFFS 2013 23",
                              x = zeroCouponRatesPerFFFS()$datafr_FFFS_2013,
                              startCol = 2,
                              startRow = 9,
                              colNames = FALSE
          )
          openxlsx::writeData(wb, 
                              sheet = "FFFS 2019 21 ordinarie",
                              x = zeroCouponRatesPerFFFS()$datafr_FFFS_2019_ordinary,
                              startCol = 2,
                              startRow = 9,
                              colNames = FALSE
          )
          openxlsx::writeData(wb, 
                              sheet = "FFFS 2019 21 tillfällig",
                              x = zeroCouponRatesPerFFFS()$datafr_FFFS_2019_temp,
                              startCol = 2,
                              startRow = 9,
                              colNames = FALSE
          )
          # Datum
          openxlsx::writeData(wb, 
                              sheet = "FFFS 2013 23",
                              x = matrix(as.character(input$drc_date), nrow = 1, ncol = 6),
                              startCol = 2,
                              startRow = 8,
                              colNames = FALSE
          )
          openxlsx::writeData(wb, 
                              sheet = "FFFS 2019 21 ordinarie",
                              x = matrix(as.character(input$drc_date), nrow = 1, ncol = 5),
                              startCol = 2,
                              startRow = 8,
                              colNames = FALSE
          )
          openxlsx::writeData(wb, 
                              sheet = "FFFS 2019 21 tillfällig",
                              x = matrix(as.character(input$drc_date), nrow = 1, ncol = 5),
                              startCol = 2,
                              startRow = 8,
                              colNames = FALSE
          )
          openxlsx::saveWorkbook(wb, file = "./data/temporary.xlsx", overwrite = TRUE)
          file.copy(from = "./data/temporary.xlsx", to = file)
        }
      )
      
      output$downloadDatasetWeightedInterestRateSwap_FFFS_2013 <- downloadHandler(
        filename = function() {
          paste("FFFS_2013_23", ".xlsx", sep = "")
        },
        content = function(file) {
          xlsx::write.xlsx(wirss_no_negative_2013$wirs(), 
                           file, 
                           sheetName = "Tjänstepensionskurvan", 
                           col.names = TRUE, 
                           row.names = FALSE,
                           showNA = FALSE,
                           append = FALSE)
          xlsx::write.xlsx(wirss_other_2013$wirs(), 
                           file, 
                           sheetName = "Annan Försäkring", 
                           col.names = TRUE, 
                           row.names = FALSE, 
                           showNA = FALSE,
                           append = TRUE)
          xlsx::write.xlsx(wirss_abs_up_100_2013$wirs(), 
                           file, 
                           sheetName = "Stress Absolut upp 100bp", 
                           col.names = TRUE, 
                           row.names = FALSE, 
                           showNA = FALSE,
                           append = TRUE)
          xlsx::write.xlsx(wirss_abs_down_100_2013$wirs(), 
                           file, 
                           sheetName = "Stress Absolut ned 100bp", 
                           col.names = TRUE, 
                           row.names = FALSE, 
                           showNA = FALSE,
                           append = TRUE)
          xlsx::write.xlsx(wirss_abs_up_50_2013$wirs(), 
                           file, 
                           sheetName = "Stress Absolut upp 50bp", 
                           col.names = TRUE, 
                           row.names = FALSE, 
                           showNA = FALSE,
                           append = TRUE)
          xlsx::write.xlsx(wirss_abs_down_50_2013$wirs(), 
                           file, 
                           sheetName = "Stress Absolut ned 50bp", 
                           col.names = TRUE, 
                           row.names = FALSE, 
                           showNA = FALSE,
                           append = TRUE)
          xlsx::write.xlsx(wirss_identity_2013$wirs(), 
                           file, 
                           sheetName = "Otransformerad", 
                           col.names = TRUE, 
                           row.names = FALSE, 
                           showNA = FALSE,
                           append = TRUE)
        }
      )
      
      output$downloadDatasetWeightedInterestRateSwap_FFFS_2019_ordinary <- downloadHandler(
        filename = function() {
          paste("FFFS_2019_21_Ordinarie", ".xlsx", sep = "")
        },
        content = function(file) {
          xlsx::write.xlsx(wirss_identity_ordinary$wirs(), 
                           file, 
                           sheetName = "Tjänstepensionskurvan", 
                           col.names = TRUE, 
                           row.names = FALSE,
                           showNA = FALSE,
                           append = FALSE)
          xlsx::write.xlsx(wirss_abs_up_ordinary$wirs(), 
                           file, 
                           sheetName = "Stress Absolut upp", 
                           col.names = TRUE, 
                           row.names = FALSE, 
                           showNA = FALSE,
                           append = TRUE)
          xlsx::write.xlsx(wirss_abs_down_ordinary$wirs(), 
                           file, 
                           sheetName = "Stress Absolut ned", 
                           col.names = TRUE, 
                           row.names = FALSE, 
                           showNA = FALSE,
                           append = TRUE)
          xlsx::write.xlsx(wirss_rel_up_ordinary$wirs(), 
                           file, 
                           sheetName = "Stress Relativ upp", 
                           col.names = TRUE, 
                           row.names = FALSE, 
                           showNA = FALSE,
                           append = TRUE)
          xlsx::write.xlsx(wirss_rel_down_ordinary$wirs(), 
                           file, 
                           sheetName = "Stress Relativ ned", 
                           col.names = TRUE, 
                           row.names = FALSE, 
                           showNA = FALSE,
                           append = TRUE)
        }
      )
      
      output$downloadDatasetWeightedInterestRateSwap_FFFS_2019_temp <- downloadHandler(
        filename = function() {
          paste("FFFS_2019_21_Tillfällig", ".xlsx", sep = "")
        },
        content = function(file) {
          xlsx::write.xlsx(wirss_identity_temp$wirs(), 
                           file, 
                           sheetName = "Tjänstepensionskurvan", 
                           col.names = TRUE, 
                           row.names = FALSE,
                           showNA = FALSE,
                           append = FALSE)
          xlsx::write.xlsx(wirss_abs_up_temp$wirs(), 
                           file, 
                           sheetName = "Stress Absolut upp", 
                           col.names = TRUE, 
                           row.names = FALSE, 
                           showNA = FALSE,
                           append = TRUE)
          xlsx::write.xlsx(wirss_abs_down_temp$wirs(), 
                           file, 
                           sheetName = "Stress Absolut ned", 
                           col.names = TRUE, 
                           row.names = FALSE, 
                           showNA = FALSE,
                           append = TRUE)
          xlsx::write.xlsx(wirss_rel_up_temp$wirs(), 
                           file, 
                           sheetName = "Stress Relativ upp", 
                           col.names = TRUE, 
                           row.names = FALSE, 
                           showNA = FALSE,
                           append = TRUE)
          xlsx::write.xlsx(wirss_rel_down_temp$wirs(), 
                           file, 
                           sheetName = "Stress Relativ ned", 
                           col.names = TRUE, 
                           row.names = FALSE, 
                           showNA = FALSE,
                           append = TRUE)
        }
      )
      
    }
    if (req(input$left_side_bar) == "tab_module_solvencymod"){
      shinyjs::removeClass(selector = "body", class = "control-sidebar-open")
      output$right_side_bar <- renderUI({div()})
      output$right_side_bar_downloads <- renderUI({div()})
    }
    if (req(input$left_side_bar) == "tab_module_esg"){
      shinyjs::removeClass(selector = "body", class = "control-sidebar-open")
      output$right_side_bar <- renderUI({div()})
      output$right_side_bar_downloads <- renderUI({div()})
    }
  })
  
  
}

shinyApp(ui, server)
