

mod_payments_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # uiOutput(ns("ui"))
  )
}

mod_payments_server <- function(
    id,
    days = reactive(NULL),
    incomeMother = reactive(NULL),
    incomeFather = reactive(NULL)
  ) {

  # stopifnot(is.reactive(data))
  # stopifnot(!is.reactive(filter))

  moduleServer(id, function(input, output, session) {

    tbl.paymentsMutterschutz <- reactive({
      tbl.days <- days$tbl()

      tbl.days |>
        filter(parent == "Elternteil 1" & periodType == "Mutterschutz") |>
        mutate(
          paymentType = "Wochengeld",
          value = incomeMother()$tagsatzWochengeld
        )
    })

    tbl.paymentsIncomeDependent <- reactive({
      tbl.days <- days$tbl()

      tbl.paymentsKonto <- tbl.days |>
        filter(!(parent == "Elternteil 1" & periodType == "Mutterschutz")) |>
        mutate(
          paymentType = "Kinderbetreuungsgeld",
          value = case_when(
            parent == "Elternteil 1" ~ incomeMother()$tagsatz,
            parent == "Elternteil 2" ~ incomeFather()$tagsatz
          )
        )
    })

    tbl.paymentsKonto <- reactive({
      tbl.days <- days$tbl()

      birthDate <- days$birthDate()

      duration <- as.numeric(max(tbl.days$date) - birthDate)

      durationElternteil2 <- tbl.days |>
        filter(parent == "Elternteil 2") |>
        nrow()

      if (durationElternteil2 / duration >= 0.2){
        # TODO: Also 20% for Elternteil1
        amount <- config$kbgKonto$amount2Parents
      } else {
        amount <- config$kbgKonto$amount1Parent
      }
      tagsatzKonto <- amount / duration

      # If the Wochengeld is larger than KGB, "KGB ist ruhend gestellt"
      # If KGB is larger than Wochengeld, receive Wochengeld + the differce

      tbl.paymentsKonto <- tbl.days |>
        filter(date >= birthDate) |>
        mutate(
          paymentType = "Kinderbetreuungsgeld",
          value = case_when(
            parent == "Elternteil 1" & periodType == "Mutterschutz"
              ~ max(tagsatzKonto - incomeMother()$tagsatzWochengeld, 0),
            parent == "Elternteil 1" ~ tagsatzKonto,
            parent == "Elternteil 2" ~ tagsatzKonto,
          )
        )
    })

  return(
    list(
      mutterschutz = tbl.paymentsMutterschutz,
      incomeDependent = tbl.paymentsIncomeDependent,
      konto = tbl.paymentsKonto
    )
  )

  })
}
