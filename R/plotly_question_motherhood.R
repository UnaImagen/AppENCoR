plotly_question_motherhood <- function(.data, q) {

   .data %>%
      dplyr::mutate(
         variable := !!rlang::sym(q)
      ) %>%
      dplyr::group_by(
         sexo,
         variable
      ) %>%
      dplyr::summarise(
         n = base::sum(peso, na.rm = TRUE),
         .groups = "drop_last"
      ) %>%
      dplyr::mutate(
         prop = n / sum(n)
      ) %>%
      plotly::plot_ly() %>%
      plotly::add_trace(
         x = ~variable,
         y = ~prop,
         color = ~sexo,
         colors = "Dark2",
         type = "bar",
         hovertemplate = ~base::paste0(
            "%{y:0.2%}"
         )
      ) %>%
      plotly::layout(
         xaxis = base::list(
            title = NA
         ),
         yaxis = base::list(
            title = "<b>Porcentaje</b>",
            tickformat = "%"
         ),
         legend = base::list(
            title = base::list(
               text = "<b>Sexo de quien<br>responde<b>"
            ),
            bgcolor = "#E2E2E2",
            orientation = "h",
            yanchor = "bottom",
            xanchor = "left",
            y = -.40
         ),
         hovermode = "x"
      ) %>%
      plotly::config(
         locale = "es",
         displayModeBar = TRUE,
         displaylogo = FALSE,
         modeBarButtonsToRemove = base::c(
            "zoom2d",
            "zoomIn2d",
            "zoomOut2d",
            "select2d",
            "drawclosedpath",
            "lasso2d",
            "pan2d",
            "drawrect",
            "autoScale2d",
            "hoverClosestCartesian",
            "hoverCompareCartesian",
            "toggleSpikelines"
         )
      )

}