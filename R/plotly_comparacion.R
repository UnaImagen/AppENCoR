plotly_comparacion <- function(.data, gender, var_x, var_y) {

   color <- dplyr::case_when(

      gender == "hombre" ~ "rgba(27, 158, 119, 1)",
      gender == "mujer" ~ "rgba(217, 95, 2, 1)"

   )

   titulo_x <- dplyr::case_when(

      var_x == "cantidad_ideal_hijos" ~ "Si pudiera volver atrás en el tiempo y elegir el número de hijos para tener en su vida, ¿cuántos serían?",
      var_x == "cantidad_hijos" ~ "¿Cuántos hijos nacidos vivos ha tenido a lo largo de su vida?",
      var_x == "edad_ideal_primer_hijo" ~ "Si pudiera volver atrás en el tiempo y elegir la edad a la cual tener su primer hijo/a, ¿cuál sería?",
      var_x == "edad_limit_inf_sexo_mujeres" ~ "¿A qué edad le parece que una mujer es demasiado joven para tener relaciones sexuales?",
      var_x == "edad_limit_inf_sexo_varones" ~ "¿A qué edad le parece que un hombre es demasiado joven para tener relaciones sexuales?",
      var_x == "edad_limit_inf_hijos_mujeres" ~ "¿A qué edad le parece que una mujer es demasiado joven para tener hijos?",
      var_x == "edad_limit_inf_hijos_varones" ~ "¿A qué edad le parece que un hombre es demasiado joven para tener hijos?",
      var_x == "edad_limit_sup_hijos_mujeres" ~ "¿A qué edad le parece que una mujer es demasiado mayor para tener hijos?",
      var_x == "edad_limit_sup_hijos_varones" ~ "¿A qué edad le parece que un hombre es demasiado mayor para tener hijos?",
      var_x == "edad_limit_inf_abandonar_estudios_mujeres" ~ "¿A qué edad le parece que una mujer es demasiado joven para abandonar los estudios en forma definitiva?",
      var_x == "edad_limit_inf_abandonar_estudios_varones" ~ "¿A qué edad le parece que un hombre es demasiado joven para abandonar los estudios en forma definitiva?"

   )

   titulo_x <- base::paste("<br>", stringr::str_wrap(string = titulo_x, width = 50))

   titulo_y <- dplyr::case_when(

      var_y == "cantidad_ideal_hijos" ~ "Si pudiera volver atrás en el tiempo y elegir el número de hijos para tener en su vida, ¿cuántos serían?",
      var_y == "cantidad_hijos" ~ "¿Cuántos hijos nacidos vivos ha tenido a lo largo de su vida?",
      var_y == "edad_ideal_primer_hijo" ~ "Si pudiera volver atrás en el tiempo y elegir la edad a la cual tener su primer hijo/a, ¿cuál sería?",
      var_y == "edad_limit_inf_sexo_mujeres" ~ "¿A qué edad le parece que una mujer es demasiado joven para tener relaciones sexuales?",
      var_y == "edad_limit_inf_sexo_varones" ~ "¿A qué edad le parece que un hombre es demasiado joven para tener relaciones sexuales?",
      var_y == "edad_limit_inf_hijos_mujeres" ~ "¿A qué edad le parece que una mujer es demasiado joven para tener hijos?",
      var_y == "edad_limit_inf_hijos_varones" ~ "¿A qué edad le parece que un hombre es demasiado joven para tener hijos?",
      var_y == "edad_limit_sup_hijos_mujeres" ~ "¿A qué edad le parece que una mujer es demasiado mayor para tener hijos?",
      var_y == "edad_limit_sup_hijos_varones" ~ "¿A qué edad le parece que un hombre es demasiado mayor para tener hijos?",
      var_y == "edad_limit_inf_abandonar_estudios_mujeres" ~ "¿A qué edad le parece que una mujer es demasiado joven para abandonar los estudios en forma definitiva?",
      var_y == "edad_limit_inf_abandonar_estudios_varones" ~ "¿A qué edad le parece que un hombre es demasiado joven para abandonar los estudios en forma definitiva?"

   )

   titulo_y <- base::paste(stringr::str_wrap(string = titulo_y, width = 40), "<br>")

   .data %>%
      dplyr::filter(
         sexo == gender
      ) %>%
      dplyr::group_by(
         var_x = base::as.integer(!!rlang::sym(var_x)),
         var_y = base::as.integer(!!rlang::sym(var_y))
      ) %>%
      dplyr::summarise(
         n = base::sum(peso, na.rm = TRUE),
         .groups = "drop_last"
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
         prop = n / base::sum(n)
      ) %>%
      plotly::plot_ly(
         x = ~var_x,
         y = ~var_y,
         type = "scatter",
         mode = "markers",
         marker = base::list(
            color = color,
            line = base::list(
               color = color,
               width = 1
            ),
            size = ~(prop * 100 * 5),
            sizeref = 1,
            sizemode = 'area'
         ),
         hovertemplate = ~base::paste0(
            "<b>Porcentaje de encuestados: </b>",
            formattable::percent(
               x = prop,
               digits = 2L,
               big.mark = ".",
               decimal.mark = ","
            )
         ),
         name = " "
      ) %>%
      plotly::layout(
         xaxis = base::list(
            title = titulo_x,
            spikemode = "toaxis"
         ),
         yaxis = base::list(
            title = titulo_y,
            spikemode = "toaxis"
         ),
         scene = base::list(
            aspectration = base::list(
               x = 1,
               y = 1
            )
         ),
         margin = base::list(
            pad = 4
         )
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