#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      # Skapar en panel med sidomeny och huvudfönster
      sidebarLayout(
        # Skapar sidomenyn
        sidebarPanel(
          # Skapar rullista där art kan väljas
          selectInput(
            "artVal",
            "Välj art: ",
            choices = sort(unique(as.character(data$Art))),
            multiple = TRUE,
            selected = "Abborre"
          ),
          # Skapar slidermeny för min- och maxvärde för storlek
          # Eftersom alternativen påverkas av andra menyval behöver de skapas i serverdelen och sedan anropas hit med uiOutput()
          uiOutput("strlkSelection"),
          # Slidermeny för storleksklasser
          uiOutput("binSelection"),
          # Slidermeny för län
          uiOutput("länSelection"),
          # Slidermeny för år
          sliderInput(
            "årVal",
            "Välj år:",
            min = 2003,
            max = 2016,
            value = c(2003, 2016)
          ),
          # Slidermeny för månad kan väljas.
          sliderInput(
            "månVal",
            "Välj månad:",
            min = 1,
            max = 12,
            value = c(1, 12)
          )
        ),
        mainPanel(
          plotOutput("distPlot"), type = "html", loader = "loader6"
        )
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

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "LengthDistributions"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
