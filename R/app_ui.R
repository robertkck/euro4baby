#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#' @import echarts4r
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    navbarPage(
      theme = bslib::bs_theme(bootswatch = "minty"),

      # Application title
      title = "Kinderbetreuungsgeld",

      tabPanel(
        "Planer",
        # Sidebar with a slider input for number of bins
        sidebarLayout(
          sidebarPanel(
            # textInput(
            #   "nameParent1",
            #   "Name",
            #   value = "Kathi"
            # ),
            sliderInput(
              "incomeMother",
              "Nettoeinkomen Elternteil 1",
              min = 0,
              max = 5000,
              value = 2000
            ),
            checkboxInput(
              "givesBirth",
              "gebährend",
              value = TRUE
            ),
            numericInput(
              "incomePastYearMother",
              "Einkommen Vorjahr",
              min = 0,
              max = 1000000,
              value = 28000
            ),
            tags$hr(),
            # textInput(
            #   "nameParent2",
            #   "Name",
            #   value = "Robi"
            # ),
            sliderInput(
              "incomeFather",
              "Nettoeinkomen Elternteil 2",
              min = 0,
              max = 5000,
              value = 2000
            ),
            numericInput(
              "incomePastYearFather",
              "Einkommen Vorjahr",
              min = 0,
              max = 1000000,
              value = 28000
            ),
            tags$hr(),
            dateInput(
              "birthDate",
              "Geburtsdatum",
              value = "2023-09-01"
            ),checkboxInput(
              "exactDate",
              "Exaktes Geburtsdatum",
              value = TRUE
            ),
            tags$hr(),
            selectInput(
              "order",
              "Reihenfolge",
              choices = order_labels$name,
              selected = "Elterteil 1 - Elternteil 2"
            ),
            fluidRow(
              column(
                width = 4,
                numericInput(
                  "period1",
                  "Periode 1",
                  value = 213,
                  min = 0,
                  max = config$kbgKonto$maxDuration
                )
              ),
              column(
                width = 4,
                uiOutput("period2")
              ),
              column(
                width = 4,
                uiOutput("period3")
              )
            ),
            tags$hr(),
            bookmarkButton()
          ),

          # Show a plot of the generated distribution
          mainPanel(
            fluidRow(
              column(
                width = 4,
                div(
                  class = "p-3 mb-2 bg-primary text-white rounded",
                  tags$b(
                    uiOutput("box_totalMutterschutz")
                  ),
                  p("Wochengeld")
                )
              ),
              column(
                width = 4,
                div(
                  class = "p-3 mb-2 bg-primary text-white rounded",
                  tags$b(
                    uiOutput("box_totalKinderbetreuungsgeld")
                  ),
                  p("Kinderbetreuungsgeld")
                )
                # div(
                #   class = "p-3 mb-2 bg-primary text-white rounded",
                #   tags$b(
                #     uiOutput("box_employmentMother")
                #   ),
                #   p("Durchgehende Anstellung seit")
                # )
              ),
              column(
                width = 4,
                div(
                  class = "p-3 mb-2 bg-primary text-white rounded",
                  tags$b(
                    uiOutput("box_totalDuration")
                  ),
                  p("Tage")
                )
              )
            ),
            fluidRow(
              column(
                width = 6,
                h3("Zahlungen"),
                echarts4r::echarts4rOutput("figPayments")
              ),
              column(
                width = 6,
                h3("Modelle"),
                echarts4rOutput("modelle")
              )
            ),
            fluidRow(
              column(
                width = 6,
                mod_income_ui("parent1")
              ),
              column(
                width = 6,
                mod_income_ui("parent2")
              )
            )
          )
        )
      ),
      tabPanel(
        "Voraussetzungen",
        uiOutput("preconditions"),
        # Exact calendar with https://dreamrs.github.io/toastui/articles/extras/calendar.html
        uiOutput("importantDates")
      ),
      tabPanel(
        "Impressum",
        mod_imprint_ui("imprint")
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  config <<- config::get(file = app_sys("config.yml"))

  tags$head(
    favicon(ext = 'png'),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "lol"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
