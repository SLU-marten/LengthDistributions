#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  output$länSelection <- renderUI({
    selectInput(
      "länVal",
      "Välj Län: ",
      choices = sort(unique(as.character(unlist(
        data[data$Art == input$artVal, "Län"]
      )))),
      selected = as.character(sort(unlist(data$Län)))[1],
      multiple = T
    )
  })
  output$binSelection <- renderUI({
    sliderInput(
      "bins",
      "Längdgruppstorlek (mm)",
      min = 1,
      max = 100,
      value = 5
    )
  })
  output$strlkSelection <- renderUI({
    sliderInput(
      "strlk",
      "Längdintervall",
      min = min(
        as.numeric(
          unlist(
            dplyr::filter(data, Art %in% input$artVal) |>
              dplyr::filter(dplyr::between(Mån, input$månVal[1], input$månVal[2])) |>
              dplyr::select(Längd)
          )
        )
      ) - 5,
      max = max(
        as.numeric(
          unlist(
            dplyr::filter(data, Art %in% input$artVal) |>
              dplyr::filter(dplyr::between(Mån, input$månVal[1], input$månVal[2])) |>
              dplyr::select(Längd)
          )
        )
      ) + 5,
      value = c(
        min(
          unlist(
            data[data$Art == input$artVal, "Längd"]
          )
        ),
        max = max(
          unlist(
            data[data$Art ==input$artVal, "Längd"]
          )
        )
      )
    )
  })
  output$distPlot <- renderPlot({
    req(input$länVal)
    d <- dplyr::filter(data, Art %in% input$artVal) |>
      dplyr::filter(dplyr::between(År, input$årVal[1], input$årVal[2])) |>
      dplyr::filter(dplyr::between(Mån, input$månVal[1], input$månVal[2])) |>
      dplyr::filter(Län %in% input$länVal) |>
      dplyr::select(Längd, Art)
    ggplot2::ggplot(d, aes(x = Längd, fill = Art, Color = Art)) +
      theme_light() +
      labs(title = "Längdfördelning") +
      labs(x = "Längd (mm)", y = "Antal") +
      geom_histogram(binwidth = input$bins,
                     position = "identity",
                     alpha = 0.5) +
      xlim(c(input$strlk[1], input$strlk[2])) +
      theme(
        axis.title = element_text(size = 15,
                                  face = "bold"),
        axis.text = element_text(size = 15),
        plot.title = element_text(size = 20,
                                  hjust = 0.5),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15,
                                    face = "bold"),
        panel.grid.major = element_line(colour = "gray95"),
        panel.grid.minor = element_line(colour = "gray95",
                                        linetype = "dashed")
      )
  })
}
