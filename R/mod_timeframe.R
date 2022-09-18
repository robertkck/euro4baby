generate_kinderbetreuungsgeld_period <- function(startDate, period, parent){
  tibble(
    date = seq(
      startDate,
      startDate + period,
      "day"
    ),
    parent = parent,
    periodType = "Kinderbetreuungsgeld"
  )
}

mod_timeframe_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # verbatimTextOutput(ns("stats"))
  )
}

mod_timeframe_server <- function(
    id,
    order = reactive(c("Elternteil 1", "Elternteil 2")),
    exactDate = reactive(TRUE),
    birthDate = reactive(as.Date("2023-09-01")),
    period1 = reactive(356),
    period2 = reactive(61),
    period3 = reactive(NULL)
  ) {
  moduleServer(id, function(input, output, session) {

    r.days <- reactive({
      req(birthDate(), period1())

      mutterschutzPeriod <- tibble(
        date = seq(
          birthDate() - lubridate::weeks(8),
          birthDate() + lubridate::weeks(8),
          "day"
        ),
        parent = "Elternteil 1",
        periodType = "Mutterschutz"
      )

      kinderbetreuungsgeldPeriod1 <- generate_kinderbetreuungsgeld_period(
        startDate = max(mutterschutzPeriod$date) + 1,
        period = period1(),
        parent = order()[1]
      )

      tbl.days <- bind_rows(
        mutterschutzPeriod,
        kinderbetreuungsgeldPeriod1
      )

      if (length(order()) >= 2) {
        req(period2())
        kinderbetreuungsgeldPeriod2 <- generate_kinderbetreuungsgeld_period(
          startDate = max(kinderbetreuungsgeldPeriod1$date) + 1,
          period = period2(),
          parent = order()[2]
        )

        tbl.days <- bind_rows(
          tbl.days,
          kinderbetreuungsgeldPeriod2
        )
      }

      if (length(order()) == 3){
        req(period3())
        kinderbetreuungsgeldPeriod3 <- generate_kinderbetreuungsgeld_period(
          startDate = max(kinderbetreuungsgeldPeriod2$date),
          period = period3(),
          parent = order()[3]
        )

        tbl.days <- bind_rows(
          tbl.days,
          kinderbetreuungsgeldPeriod3
        )
      }
      return(
        tbl.days |>
          arrange(date)
      )
    })

    output$stats <- renderPrint({
      print(r.days())
    })

    duration <- reactive({
      max(r.days()$date) - min(r.days()$date)
    })

    return(
      list(
        tbl = r.days,
        birthDate = birthDate,
        duration = duration
      )
    )
  }
)}
