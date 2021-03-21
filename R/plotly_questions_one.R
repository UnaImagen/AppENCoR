plotly_questions_one <- function(.data, q, th) {

   titulo <- dplyr::case_when(
      q == "cantidad_ideal_hijos" ~ "<b>Cantidad ideal de hijos</b>",
      q == "edad_ideal_primer_hijo" ~ "<b>Edad ideal para primer hijo</b>"
   )

   .data %>%
      dplyr::mutate(
         variable := !!rlang::sym(q)
      ) %>%
      dplyr::filter(
         tuvo_hijos == th
      ) %>%
      base::droplevels() %>%
      dplyr::group_by(
         sexo,
         variable
      ) %>%
      dplyr::summarise(
         n = base::sum(peso, na.rm = TRUE),
         .groups = "drop_last"
      ) %>%
      dplyr::mutate(
         prop = n / base::sum(n)
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
            title = titulo
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