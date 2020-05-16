#===========================#
#### FUNCIONES APP ENCOR ####
#===========================#

## Genera plot para preguntas de ideales (tipo 1)
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
         n = base::sum(peso, na.rm = TRUE)
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

## Genera plot para preguntas de ideales (tipo 2)
plotly_questions_two <- function(.data, q) {

   titulo <- dplyr::case_when(

      stringr::str_detect(q, pattern = "inf") ~ "<b>Edad límite inferior</b>",
      stringr::str_detect(q, pattern = "sup") ~ "<b>Edad límite superior</b>"

   )

   .data %>%
      dplyr::mutate(
         variable := !!rlang::sym(q)
      ) %>%
      dplyr::group_by(
         sexo,
         variable
      ) %>%
      dplyr::summarise(
         n = base::sum(peso, na.rm = TRUE)
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

## Genera plot para preguntas de ideales (motherhood)
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
         n = base::sum(peso, na.rm = TRUE)
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

## Genera sankey diagram para métodos anticonceptivos
generar_sankey <- function(.data, var_1, var_2) {

   aux_data <- .data %>%
      dplyr::group_by(
         var_1 := !!rlang::sym(var_1),
         var_2 := !!rlang::sym(var_2)
      ) %>%
      dplyr::summarise(
         n = base::sum(peso, na.rm = TRUE)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::filter(
         stats::complete.cases(.)
      ) %>%
      dplyr::transmute(
         source = var_1,
         target = stringr::str_c(var_2, " "),
         value = n / base::sum(n)
      )

   # Define nodos
   nodes <- base::data.frame(name = base::c(base::as.character(aux_data$source), base::as.character(aux_data$target)) %>% base::unique())

   # Agrega IDs con 0 indexing (porque JS usa 0 indexing)
   aux_data$IDsource <- base::match(aux_data$source, nodes$name) - 1
   aux_data$IDtarget <- base::match(aux_data$target, nodes$name) - 1

   # Construye el Sankey
   networkD3::sankeyNetwork(
      Links = base::as.data.frame(aux_data),
      Nodes = nodes,
      Source = "IDsource",
      Target = "IDtarget",
      Value = "value",
      NodeID = "name",
      sinksRight = FALSE,
      nodeWidth = 40,
      fontSize = 13,
      nodePadding = 20
   )

}

## Genera plot para comparación entre preguntas
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
         n = base::sum(peso, na.rm = TRUE)
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

#===============#
#### THE END ####
#===============#