mod_imprint_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4),
      column(
        width = 4,
        h1(paste(emo::ji("wave"), "Hallo")),
        p(
          "Ich habe den Kinderbetreuungsgeld-Planer entwickelt, um uns durch den
          Dschungel der Bestimmungen und Berechnungen zu helfen. Ich hoffe mit dem
          Planer vielen werdenden Eltern zu helfen."
        ),
        p(
          "Für Feedback und Anregungen schreibe mir direkt. Du findest mich unter ", a(target = "_blank", href = "https://rhawlik.me", "rhawlik.me", ".")
        ),
        p(
          "Unterstütze das Projekt auf ",
          a(target = "_blank", href = "https://github.com/robertkck/euro4baby/issues", "Github"),
          " oder "
        ),

        div(
          style = "height:20px;",
          HTML(
            '<script type="text/javascript" src="https://cdnjs.buymeacoffee.com/1.0.0/button.prod.min.js" data-name="bmc-button" data-slug="Ovh7aGdX0" data-color="#78c2ad" data-emoji=""  data-font="Cookie" data-text="Spendier mir einen Kaffee" data-outline-color="#000000" data-font-color="#000000" data-coffee-color="#ffffff" ></script>'
          )
        )
      ),
      column(4)
    )
  )
}

mod_imprint_server <- function(id) {
  moduleServer(id, function(input, output, session) {

  })
}
