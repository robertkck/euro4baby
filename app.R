#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(echarts4r)

order_choices <- list(c(1,2), c(1,2,1), c(2, 1), c(2, 1, 2), c(1))
order_labels <- tibble(
    name = order_choices |>
      purrr::map(~paste("Elternteil", .x)) |>
      purrr::map_chr(~paste(.x, collapse = " - ")),
    value = order_choices |>
      purrr::map(~paste("Elternteil", .x))
  )
format_date <- function(date){
  lubridate::stamp("1. Mai, 1999", locale = "de")(date)
}
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Euro 4 Baby"),

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
              value = 3000
            ),
            checkboxInput(
              "isMother",
              "Mutter",
              value = TRUE
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
              value = 3000
            ),
            tags$hr(),
            checkboxInput(
              "exactDate",
              "Exaktes Geburtsdatum",
              value = TRUE
            ),
            dateInput(
              "birthDate",
              "Geburtsdatum",
              value = "2023-09-01"
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
                  max = 365
                )
              ),
              column(
                width = 4,
                numericInput(
                  "period2",
                  "Periode 2",
                  value = 213,
                  min = 0,
                  max = 365
                )
              ),
              column(
                width = 4,
                uiOutput("period3")
              )
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           # fluidRow(
           #   column(
           #     width = 6,
           #     mod_income_ui("mother")
           #    ),
           #   column(
           #     width = 6,
           #     mod_income_ui("father")
           #   ),
           #   # mod_timeframe_ui("timeframe"),
           #   # verbatimTextOutput("totalReturnTogether")
           #  ),
           fluidRow(
             column(
               width = 6,
               uiOutput("totals")
             ),
             column(
               width = 6,
               echarts4rOutput("modelle")
             )
           ),
           h3("Zahlungen"),
           # Exact calendar with https://dreamrs.github.io/toastui/articles/extras/calendar.html
           echarts4r::echarts4rOutput("figPayments")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {


# modules -----------------------------------------------------------------

  r.incomeMother <- mod_income_server(
    "mother",
    parent = "mother",
    netSalary = reactive(input$incomeMother)
  )

  r.incomeFather <- mod_income_server(
    "father",
    parent = "father",
    netSalary = reactive(input$incomeFather)
  )

# Inputs ------------------------------------------------------------------

  order_parsed <- reactive({
    order_labels |>
      filter(name == input$order) |>
      pull(value) |>
      unlist()
  })

  output$period3 <- renderUI({
    if (length(order_parsed()) == 3){
      print(order_parsed())
      numericInput(
        "period3",
        "Periode 3",
        value = 365,
        min = 0,
        max = 365
      )
    } else {
      NULL
    }
  })

  output$totalReturnTogether <- renderPrint({
    print(r.payments())
  })

# Calculate base ----------------------------------------------------------

# Calculate payments ------------------------------------------------------

    r.days <- mod_timeframe_server(
      "timeframe",
      order = reactive(order_parsed()),
      exactDate = reactive(input$exactDate),
      birthDate = reactive(input$birthDate),
      period1 = reactive(input$period1),
      period2 = reactive(input$period2),
      period3 = reactive(input$period3)
    )

    r.payments <- reactive({
      req(r.incomeMother(), r.incomeFather())

      tbl.payments <- r.days() |>
        mutate(
          value = case_when(
            parent == "Elternteil 1" & periodType == "Mutterschutz"
              ~ r.incomeMother()$tagsatzWochengeld,
            parent == "Elternteil 1" ~ r.incomeMother()$tagsatz,
            parent == "Elternteil 2" ~ r.incomeFather()$tagsatz
          )
        )

      return(tbl.payments)
    })



# Calculate totals ---------------------------------------------------------

    output$totals <- renderUI({

      tagList(
        h3("Voraussetzungen"),
        p(tags$b("Durchgehende Beschäftigung Mutter seit: "),
          format_date(input$birthDate - lubridate::weeks(8) - lubridate::days(182))
        ),
        p(tags$b("Durchgehende Beschäftigung Vater seit: "),
          format_date(input$birthDate - lubridate::days(182))
        ),
        h3("Wichtige Daten"),
        p(tags$b("Beginn Mutterschutz:"), format_date(input$birthDate - lubridate::weeks(8))),
        p(tags$b("Geburt: "), format_date(input$birthDate)),
        p(tags$b("Ende der Kinderbetreuung: "), format_date(max(r.days()$date))),
        p(
          tags$b("Dauer der Kindergeld: "),
          r.days() |>
            filter(periodType == "Kindergeld") |>
            summarise(dauer = max(date) - min(date)) |>
            pull(dauer),
          " Tage"
        )
      )
    })

    output$modelle <- renderEcharts4r({
      tbl.payments <- r.payments() |>
        group_by(parent, periodType) |>
        summarise(value = sum(value, na.rm = TRUE)) |>
        mutate(model = "Einkommensabhängiges Kinderbetreuungsgeld")

      tbl.modelle <- bind_rows(
        tbl.payments,
        tbl.payments |>
          filter(periodType == "Mutterschutz") |>
          mutate(model = "Kinderbetreuungsgeld Konto")
      )

      tbl.modelle |>
        group_by(paste(parent, periodType)) |>
        e_charts(model) |>
        e_bar(value, stack = "stack") |>
        e_tooltip()
    })


# Chart -------------------------------------------------------------------


    output$figPayments <- echarts4r::renderEcharts4r({
      tbl.payments <- r.payments() |>
        mutate(month = lubridate::floor_date(date, "months")) |>
        group_by(parent, periodType, month) |>
        summarise(value = sum(value, na.rm = TRUE), .groups = NULL) |>
        ungroup() |>
        tidyr::complete(month, parent, periodType, fill = list(value = 0)) |>
        filter(
          !(parent == "Elternteil 2" & periodType == "Mutterschutz")
        )

      tbl.payments |>
        group_by(paste(parent, periodType)) |>
        echarts4r::e_charts(month) |>
        echarts4r::e_bar(value, stack = "stack") |>
        echarts4r::e_tooltip() |>
        e_mark_line(data = list(xAxis = as.Date(input$birthDate)), title = "Geburt")
    })
}

# Run the application
shinyApp(ui = ui, server = server)
