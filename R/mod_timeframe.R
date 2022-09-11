generate_kindergeld_period <- function(startDate, period, parent){
  tibble(
    date = seq(
      startDate,
      startDate + period,
      "day"
    ),
    parent = parent,
    periodType = "Kindergeld"
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

      kindergeldPeriod1 <- generate_kindergeld_period(
        startDate = max(mutterschutzPeriod$date),
        period = period1(),
        parent = order()[1]
      )

      tbl.days <- bind_rows(
        mutterschutzPeriod,
        kindergeldPeriod1
      )

      if (length(order()) >= 2) {
        kindergeldPeriod2 <- generate_kindergeld_period(
          startDate = max(kindergeldPeriod1$date),
          period = period2(),
          parent = order()[2]
        )

        tbl.days <- bind_rows(
          tbl.days,
          kindergeldPeriod2
        )
      }

      if (length(order()) == 3){
        req(period3())
        kindergeldPeriod3 <- generate_kindergeld_period(
          startDate = max(kindergeldPeriod2$date),
          period = period3(),
          parent = order()[3]
        )

        tbl.days <- bind_rows(
          tbl.days,
          kindergeldPeriod3
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

    return(r.days)
  }
)}
