
get_wochengeld_base <- function(netSalary, parent = "father"){
  stopifnot(parent %in% c("mother", "father"))

  # Haben Sie vorher Arbeitslosengeld oder Notstandshilfe bezogen, erhalten Sie den bisherigen Betrag plus einen Zuschlag von 80 Prozent.
  # Wenn Sie Kinderbetreuungsgeld beziehen, erhalten Sie ein tägliches Wochengeld in der Höhe des täglichen Kinderbetreuungsgeldes, sofern Sie vor dem aktuellen Kinderbetreuungsgeld Wochengeld bezogen haben.
  # Selbstversicherte bei geringfügiger Beschäftigung erhalten einen festen Betrag als tägliches Wochengeld von derzeit EUR 9,78.

  n.days <- 90

  if (parent == "mother") {
    incomeBase <- (netSalary * 3) * 1.17 / n.days
  } else {
    incomeBase <- (netSalary * 3) * 1.17 / n.days
  }

  return(incomeBase)
}


get_guenstigkeit_base <- function(incomePastYear, parent = "father"){
  stopifnot(parent %in% c("mother", "father"))


  if (parent == "mother") {
    incomeBase <- incomePastYear
  } else {
    incomeBase <- incomePastYear
  }

  return(incomeBase)
}

calculate_tagsatz_wochengeld <- function(incomeBase){
  tagsatz <- incomeBase * 0.8
  return(tagsatz)
}

calculate_tagsatz_guenstigkeit <- function(incomeBase){
  tagsatz <- (incomeBase * 0.62 + 4000)/365
  return(tagsatz)
}

mod_income_ui <- function(id) {
  ns <- NS(id)
  tagList(
    verbatimTextOutput(ns("totalReturn"))
  )
}

mod_income_server <- function(
    id,
    parent = "father",
    netSalary = reactive(3000),
    incomePastYear = reactive(0)
  ) {
  moduleServer(id, function(input, output, session) {

    r.tagsatzWochengeld <- reactive({
      tagsatzWochengeld <- netSalary() |>
        get_wochengeld_base(parent) |>
        calculate_tagsatz_wochengeld()

      tagsatzWochengeld <- max(tagsatzWochengeld, config$wochengeld$geringfuegig)
      return(tagsatzWochengeld)
    })

    r.tagsatzGuenstigkeit <- reactive({
      tagsatzGuenstigkeit <- incomePastYear() |>
        get_guenstigkeit_base(parent) |>
        calculate_tagsatz_guenstigkeit()
      return(tagsatzGuenstigkeit)
    })

    r.tagsatz <- reactive({
      tagsatz <- max(
        r.tagsatzWochengeld(),
        r.tagsatzGuenstigkeit()
      )
      tagsatz <- max(c(tagsatz, config$tagsatz$min))
      tagsatz <- min(c(tagsatz, config$tagsatz$max))
      return(tagsatz)
    })

    r.income <- reactive({
      list(
        netSalary = netSalary(),
        income = netSalary() * 12,
        tagsatzWochengeld = r.tagsatzWochengeld(),
        tagsatz = r.tagsatz()
      )
    })

    output$totalReturn <- renderPrint({
      # req(r.tagsatzWochengeld())()
      print(parent)
      print(paste("Jareseinkommen:", r.income()$income))
      print(paste("Tagsatz Wochengeld:", get_wochengeld_base(netSalary(), parent)))
      print(paste("KGB Tagsatz Wochengeld:", scales::dollar(r.tagsatzWochengeld(), prefix =  "€ ")))
      print(paste("KBG Tagsatz Günstigkeit:", scales::dollar(r.tagsatzGuenstigkeit(), prefix =  "€ ")))
      print(paste("KGB:", scales::dollar(r.income()$tagsatz, prefix =  "€ ")))
      # print("Tagsatz Wochengeld:", r.tagsatzWochengeldMother())
    })

    return(r.income)
  })
}
