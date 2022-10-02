#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#' @import echarts4r
#' @noRd
app_server <- function(input, output, session) {


  # modules -----------------------------------------------------------------

  r.incomeMother <- mod_income_server(
    "parent1",
    parent = "Elternteil 1",
    givesBirth = reactive(input$givesBirth),
    netSalary = reactive(input$incomeMother),
    incomePastYear = reactive(input$incomePastYearMother)
  )

  r.incomeFather <- mod_income_server(
    "parent2",
    parent = "Elternteil 2",
    givesBirth = reactive(FALSE),
    netSalary = reactive(input$incomeFather),
    incomePastYear = reactive(input$incomePastYearFather)
  )

  # Inputs ------------------------------------------------------------------

  order_parsed <- reactive({
    order_labels |>
      filter(name == input$order) |>
      pull(value) |>
      unlist()
  })


  output$period2 <- renderUI({
    if (length(order_parsed()) >= 2){
      numericInput(
        "period2",
        "Periode 2",
        value = 213,
        min = 0,
        max = 365
      )
    } else {
      NULL
    }
  })

  output$period3 <- renderUI({
    if (length(order_parsed()) == 3){
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

  r.payments <- mod_payments_server(
    "Konto",
    days = r.days,
    incomeMother = r.incomeMother,
    incomeFather = r.incomeFather
  )


  # Infoboxes ---------------------------------------------------------------

  # Infoboxes
  ## Total Mutterschutz
  r.totalMutterschutz <- reactive({
    r.payments$mutterschutz() |>
      # filter(periodType == "Mutterschutz") |>
      summarise(total = sum(value, na.rm = TRUE)) |>
      pull(total)
  })

  output$box_totalMutterschutz <- renderUI({
    scales::dollar(
      prefix = "€ ",
      r.totalMutterschutz()
    )
  })

  ## Total Kinderbetreuungsgeld
  r.totalKinderbetreuungsgeld <- reactive({
    r.payments$incomeDependent() |>
      # filter(periodType == "Kinderbetreuungsgeld") |>
      summarise(total = sum(value, na.rm = TRUE)) |>
      pull(total)
  })

  output$box_totalKinderbetreuungsgeld <- renderUI({
    scales::dollar(
      prefix = "€ ",
      r.totalKinderbetreuungsgeld()
    )
  })

  ## Total duration
  r.totalDuration <- reactive({
    r.days$duration()
    # max(r.days()$date) - min(r.days()$date)
  })

  output$box_totalDuration <- renderUI({
    as.numeric(r.totalDuration()) |>
      round(0)
  })

  ## Start Beschaeftigungsverbot
  r.employmentDateMother <- reactive({
    input$birthDate - lubridate::weeks(8) - lubridate::days(182)
  })

  output$box_employmentMother <- renderUI({
    format_date(r.employmentDateMother())
  })

  # Calculate Preconditions ---------------------------------------------------------

  # Preconditions
  ## KGB
  # Lebensmittelpunkt in Österreich, Bezug Familienbeihilfe, Gemeinsamer Wohnsitz mit Kind, Einhaltung der Zuverdienstgrenze, Mutter-Kind-Pass Untersuchungen

  ### Einkommensabhängig
  # Aufrechtes Arbeitsverhältnis, 182 Tage Erwerbstätigkeit, Kein AMS Geld
  # Sozialversicherungspflichtige Erwerbstäigkeit = Einkommen über Geringfügigkeitsgrenze
  # Unterbrechung von max. 14 Tage
  # Alleine bis 365 Tag, zu zweit bis 426
  # Zuverdienst 7300 bei 365 Tagen = geringfügig

  ### Konto
  # Alleine 12366€
  # Geteilt 15449€
  # Alleine 365 bis 851 Tage, zu zweit von 456 bis 1063 Tage

  output$importantDates <- renderUI({

    tagList(
      h3("Wichtige Daten"),
      p(tags$b("Beginn Mutterschutz:"), format_date(input$birthDate - lubridate::weeks(8))),
      p(tags$b("Geburt: "), format_date(input$birthDate)),
      p(tags$b("Ende der Kinderbetreuung: "), format_date(max(r.days$tbl()$date))),
      p(
        tags$b("Dauer des Kinderbetreuungsgelds: "),
        r.days$tbl() |>
          filter(periodType == "Kinderbetreuungsgeld") |>
          summarise(dauer = max(date) - min(date)) |>
          pull(dauer),
        " Tage"
      )
    )
  })

  output$preconditions <- renderUI({

    tagList(
      h3("Voraussetzungen"),
      p(tags$b("Durchgehende Beschäftigung Elternteil 1 seit: "),
        format_date(input$birthDate - lubridate::weeks(8) - lubridate::days(182))
      ),
      p(tags$b("Durchgehende Beschäftigung Elternteil 2 seit: "),
        format_date(input$birthDate - lubridate::days(182))
      )
    )
  })


  # Chart -------------------------------------------------------------------

  output$modelle <- renderEcharts4r({

    tbl.incomeDependent <- bind_rows(
      r.payments$mutterschutz(),
      r.payments$incomeDependent()
    ) |>
      group_by(parent, paymentType) |>
      summarise(value = sum(value, na.rm = TRUE)) %>%
      mutate(model = "Einkommensabhängig")

    tbl.konto <- bind_rows(
      r.payments$mutterschutz(),
      r.payments$konto()
    ) |>
      group_by(parent, paymentType) |>
      summarise(value = sum(value, na.rm = TRUE)) %>%
      mutate(model = "Konto")

    tbl.models <- bind_rows(
      tbl.incomeDependent,
      tbl.konto
    )

    tbl.models |>
      group_by(paste(parent, paymentType)) |>
      e_charts(model) |>
      e_bar(value, stack = "stack") |>
      e_color(color = palette) |>
      e_tooltip()
  })


  output$figPayments <- echarts4r::renderEcharts4r({
    tbl.payments <- bind_rows(
      r.payments$mutterschutz(),
      r.payments$incomeDependent()
    ) |>
      # filter(model == "Einkommensabhängiges Kinderbetreuungsgeld") %>%
      mutate(month = lubridate::floor_date(date, "months")) |>
      group_by(parent, paymentType, month) |>
      summarise(value = sum(value, na.rm = TRUE), .groups = NULL) |>
      ungroup() |>
      tidyr::complete(month, parent, paymentType, fill = list(value = 0)) |>
      filter(
        !(parent == "Elternteil 2" & paymentType == "Wochengeld")
      )

    tbl.payments |>
      group_by(paste(parent, paymentType)) |>
      echarts4r::e_charts(month) |>
      echarts4r::e_bar(value, stack = "stack") |>
      echarts4r::e_tooltip() |>
      e_color(color = palette) |>
      e_mark_line(data = list(xAxis = as.Date(input$birthDate)), title = "Geburt")
  })
}
