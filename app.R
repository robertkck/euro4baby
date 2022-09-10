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

config <- config::get()

get_wochengeld_base <- function(income, parent = "father"){
  stopifnot(parent %in% c("mother", "father"))

  # Haben Sie vorher Arbeitslosengeld oder Notstandshilfe bezogen, erhalten Sie den bisherigen Betrag plus einen Zuschlag von 80 Prozent.
  # Wenn Sie Kinderbetreuungsgeld beziehen, erhalten Sie ein tägliches Wochengeld in der Höhe des täglichen Kinderbetreuungsgeldes, sofern Sie vor dem aktuellen Kinderbetreuungsgeld Wochengeld bezogen haben.
  # Selbstversicherte bei geringfügiger Beschäftigung erhalten einen festen Betrag als tägliches Wochengeld von derzeit EUR 9,78.

  n.days <- 90

  if (parent == "mother") {
    incomeBase <- (income$netSalayMother * 3) * 1.17 / n.days
  } else {
    incomeBase <- (income$netSalayFather * 3) * 1.17 / n.days
  }

  return(incomeBase)
}


get_guenstigkeit_base <- function(income, parent = "father"){
  stopifnot(parent %in% c("mother", "father"))


  if (parent == "mother") {
    incomeBase <- income$incomeMother
  } else {
    incomeBase <- income$incomeFather
  }

  return(incomeBase)
}

calculate_tagsatz_wochengeld <- function(incomeBase){
  tagsatz <- incomeBase * 0.8
  # tagsatz <- max(c(tagsatz, config$tagsatz$min))
  # tagsatz <- min(c(tagsatz, config$tagsatz$max))
  return(tagsatz)
}

calculate_tagsatz_guenstigkeit <- function(incomeBase){
  tagsatz <- (incomeBase * 0.62 + 4000)/365
  tagsatz <- max(c(tagsatz, config$tagsatz$min))
  tagsatz <- min(c(tagsatz, config$tagsatz$max))
  return(tagsatz)
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Euro 4 Baby"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput(
              "incomeMother",
              "Nettoeinkomen Elternteil 1",
              min = 0,
              max = 10000,
              value = 3000
            ),
            checkboxInput(
              "isMother",
              "Mutter",
              value = TRUE
            ),
            tags$hr(),
            sliderInput(
              "incomeFather",
              "Nettoeinkomen Elternteil 2",
              min = 0,
              max = 10000,
              value = 3000
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           fluidRow(
             column(
               width = 6,
               verbatimTextOutput("totalReturnMother")
              ),
             column(
               width = 6,
               verbatimTextOutput("totalReturnFather")
             ),
             verbatimTextOutput("totalReturnTogether")
            ),
           # Exact calendar with https://dreamrs.github.io/toastui/articles/extras/calendar.html
           echarts4r::echarts4rOutput("figPayments")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {


# Inputs ------------------------------------------------------------------

    r.income <- reactive(
      list(
        netSalaryMother = input$incomeMother,
        netSalaryFather = input$incomeFather,
        incomeMother = input$incomeMother * 12,
        incomeFather = input$incomeFather * 12
      )
    )

    # observe({
    #   r.income$netSalaryMother <- input$incomeMother
    #   r.income$netSalaryFather <- input$incomeFather
    #   r.income$incomeMother <- input$incomeMother * 12
    #   r.income$incomeFather <- input$incomeFather * 12
    # })

    output$totalReturnMother <- renderPrint({
      # req(r.tagsatzWochengeldMother())

        print("Mother")
        print(paste("Income:", r.income()$incomeMother))
        # print("Tagsatz Wochengeld:", r.tagsatzWochengeldMother())

    })

      output$totalReturnFather <- renderPrint({
        print("Father")
        print(paste("Income:", r.income()$incomeFather))
      })

    output$totalReturnTogether <- renderPrint({
      print(r.payments())
    })

# Calculate base ----------------------------------------------------------

    # Father

    r.tagsatzGuenstigkeitFather <- reactive({
      tagsatzGuenstigkeit <- r.income() |>
        get_guenstigkeit_base("father") |>
        calculate_tagsatz_guenstigkeit()
      return(tagsatzGuenstigkeit)
    })

    r.tagsatzWochengeldFather <- reactive({
      tagsatzWochengeld <- r.income() |>
        get_wochengeld_base("father") |>
        calculate_tagsatz_wochengeld()
      return(tagsatzWochengeld)
    })

    r.tagsatzFather <- reactive({
      tagsatzFather <- max(
        r.tagsatzWochengeldFather(),
        r.tagsatzGuenstigkeitFather()
      )
      return(tagsatzFather)
    })

    # Mother

    r.tagsatzWochengeldMother <- reactive({
      tagsatzWochengeld <- r.income() |>
        get_wochengeld_base("mother") |>
        calculate_tagsatz_wochengeld()
      return(tagsatzWochengeld)
    })

    r.tagsatzGuenstigkeitMother <- reactive({
      tagsatzGuenstigkeit <- r.income() |>
        get_guenstigkeit_base("mother") |>
        calculate_tagsatz_guenstigkeit()
      return(tagsatzGuenstigkeit)
    })

    r.tagsatzMother <- reactive({
      tagsatzMother <- max(
        r.tagsatzWochengeldMother(),
        r.tagsatzGuenstigkeitMother()
      )
      return(tagsatzMother)
    })


# Calculate payments ------------------------------------------------------

    r.payments <- reactive({
      req(r.tagsatzMother(), r.tagsatzFather())
      vec.days <- seq(as.Date("2023-01-01"), as.Date("2025-01-01"), "day")

      tbl.paymentsMother <- tibble(
        date = vec.days,
        parent = "Elternteil 1",
        value = r.tagsatzMother()
      )
      tbl.paymentsFather <- tibble(
        date = vec.days,
        parent = "Elternteil 2",
        value = r.tagsatzFather()
      )

      bind_rows(
        tbl.paymentsMother,
        tbl.paymentsFather
      )
    })



# Calculate totals ---------------------------------------------------------




# Chart -------------------------------------------------------------------


    output$figPayments <- echarts4r::renderEcharts4r({
      tbl.payments <- r.payments() |>
        mutate(month = lubridate::floor_date(date, "months")) |>
        group_by(parent, month) |>
        summarise(value = sum(value, na.rm = TRUE))

      tbl.payments |>
        group_by(parent) |>
        echarts4r::e_charts(month) |>
        echarts4r::e_area(value, stack = "stack") |>
        echarts4r::e_title("Zahlungen")
    })
}

# Run the application
shinyApp(ui = ui, server = server)
