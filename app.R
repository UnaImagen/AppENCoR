#=================#
#### APP ENCOR ####
#=================#

library(shiny)
library(magrittr, quietly = TRUE)

encor <- readr::read_rds(path = "encore.rds")
metodos_anticonceptivos <- readr::read_rds(path = "metodos_anticonceptivos.rds")

# UI ----------------------------------------------------------------------
ui <- shiny::tagList(

   shiny::includeCSS(path = "style.css"),

   shiny::navbarPage(

      theme = shinythemes::shinytheme(theme = "united"),

      title = "ENCoR",

      # Tab ideales -------------------------------------------------------------
      shiny::tabPanel(

         title = "Ideales",

         shiny::sidebarPanel(

            shiny::h4("Encuesta Nacional de Comportamientos Reproductivos"),

            shiny::selectInput(
               inputId = "pregunta_ideales",
               label = "Seleccione una categoría",
               choices = base::c(
                  "Cantidad ideal de hijes",
                  "Edad ideal para tener el primer hije",
                  "Edad límite inferior para tener sexo",
                  "Edad límite inferior para tener hijes",
                  "Edad límite superior para tener hijes",
                  "Edad límite inferior para abandonar estudios"
               ),
               selected = "Cantidad ideal de hijes"
            ),

            shiny::p("Fuente: Instituto Nacional de Estadística")

         ),

         shiny::mainPanel(

            shiny::div(

               class = 'questionDiv',

               shiny::h3(

                  shiny::textOutput(
                     outputId = "texto_pregunta_sup"
                  )

               )

            ),

            plotly::plotlyOutput(
               outputId = "plot_sup"
            ),

            shiny::div(

               class = 'questionDiv',

               shiny::h3(

                  shiny::textOutput(
                     outputId = "texto_pregunta_inf"
                  )

               )

            ),

            plotly::plotlyOutput(
               outputId = "plot_inf"
            )

         )

      ),

      # Tab motherhood ----------------------------------------------------------
      shiny::tabPanel(

         title = "Maternidad",

         shiny::sidebarPanel(

            shiny::h4("Encuesta Nacional de Comportamientos Reproductivos"),

            shiny::selectInput(
               inputId = "pregunta_motherhood",
               label = "Seleccione una categoría",
               choices = base::c(
                  "Madre antes de los 18",
                  "No tener hijes",
                  "Vivir en pareja sin casarse",
                  "Tener hijes con concubino",
                  "Trabajar tiempo completo con hijes",
                  "Divorciarse con hijes",
                  "Cuidado de los hijes",
                  "Realización (mujeres)",
                  "Vida familiar",
                  "Realización (varones)"
               ),
               selected = "Madre antes de los 18"
            ),

            shiny::p("Fuente: Instituto Nacional de Estadística")

         ),

         shiny::mainPanel(

            shiny::div(

               class = 'questionDiv',

               shiny::h3(

                  shiny::textOutput(
                     outputId = "texto_pregunta_motherhood"
                  )

               )

            ),

            plotly::plotlyOutput(
               outputId = "plot_motherhood"
            )

         )

      ),

      # Tab métodos anticonceptivos ---------------------------------------------
      shiny::tabPanel(

         title = "Métodos anticonceptivos",

         shiny::sidebarPanel(

            shiny::h4("Encuesta Nacional de Comportamientos Reproductivos"),

            shiny::selectInput(
               inputId = "select_ma_var_1",
               label = "Comparar... ",
               choices = base::c("Primer relación", "Últimos 6 meses"),
               selected = "Primer relación"
            ),

            shiny::selectInput(
               inputId = "select_ma_var_2",
               label = "... con ",
               choices = base::c("Últimos 6 meses", "Última relación"),
               selected = "Últimos 6 meses"
            ),

            shiny::selectInput(

               inputId = "select_ma_sexo",
               label = "Sexo del encuestado/a: ",
               choices = base::c("Mujer", "Hombre"),
               selected = base::c("Mujer", "Hombre"),
               multiple = TRUE

            ),

            shiny::selectInput(

               inputId = "select_ma_rango_edad",
               label = "Edad del encuestado/a: ",
               choices = base::levels(metodos_anticonceptivos$rango_edad),
               selected = base::levels(metodos_anticonceptivos$rango_edad),
               multiple = TRUE

            ),

            shiny::sliderInput(

               inputId = "select_ma_rango_edad_primera_relacion",
               label = "Edad a la que tuvo la primer relación: ",
               min = base::min(metodos_anticonceptivos$edad_primera_relacion, na.rm = TRUE),
               max = base::max(metodos_anticonceptivos$edad_primera_relacion, na.rm = TRUE),
               value = base::c(
                  base::min(metodos_anticonceptivos$edad_primera_relacion, na.rm = TRUE),
                  base::max(metodos_anticonceptivos$edad_primera_relacion, na.rm = TRUE)),
               step = 1,
               animate = TRUE

            ),

            shiny::p("Fuente: Instituto Nacional de Estadística")

         ),

         shiny::mainPanel(

            shiny::div(

               class = 'questionDiv',

               shiny::p(

                  shiny::textOutput(
                     outputId = "texto_metodos_anticonceptivos"
                  )

               )

            ),

            networkD3::sankeyNetworkOutput(
               outputId = "sankey_metodos_anticonceptivos"
            )

         )

      )

   )

)

# Server ------------------------------------------------------------------
server <- function(input, output) {

   # Funciones ---------------------------------------------------------------

   plotly_questions_one <- function(q, th) {

      titulo <- dplyr::case_when(
         q == "cantidad_ideal_hijos" ~ "<b>Cantidad ideal de hijes</b>",
         q == "edad_ideal_primer_hijo" ~ "<b>Edad ideal para primer hije</b>"
      )

      encor %>%
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
            n = dplyr::n()
         ) %>%
         dplyr::mutate(
            prop = n / base::sum(n)
         ) %>%
         plotly::plot_ly() %>%
         plotly::add_trace(
            x = ~variable,
            y = ~prop,
            color = ~sexo,
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
            )
         ) %>%
         plotly::config(
            locale = "es",
            displayModeBar = TRUE
         )

   }

   plotly_questions_two <- function(q) {

      titulo <- dplyr::case_when(

         stringr::str_detect(q, pattern = "inf") ~ "<b>Edad límite inferior</b>",
         stringr::str_detect(q, pattern = "sup") ~ "<b>Edad límite superior</b>"

      )

      encor %>%
         dplyr::mutate(
            variable := !!rlang::sym(q)
         ) %>%
         dplyr::group_by(
            sexo,
            variable
         ) %>%
         dplyr::summarise(
            n = dplyr::n()
         ) %>%
         dplyr::mutate(
            prop = n / base::sum(n)
         ) %>%
         plotly::plot_ly() %>%
         plotly::add_trace(
            x = ~variable,
            y = ~prop,
            color = ~sexo,
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
            )
         ) %>%
         plotly::config(
            locale = "es",
            displayModeBar = TRUE
         )

   }

   plotly_question_motherhood <- function(q) {

      encor %>%
         dplyr::mutate(
            variable := !!rlang::sym(q)
         ) %>%
         dplyr::group_by(
            sexo,
            variable
         ) %>%
         dplyr::tally() %>%
         dplyr::mutate(
            prop = n / sum(n)
         ) %>%
         plotly::plot_ly() %>%
         plotly::add_trace(
            x = ~variable,
            y = ~prop,
            color = ~sexo,
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
            )
         ) %>%
         plotly::config(
            locale = "es",
            displayModeBar = TRUE
         )

   }

   generar_sankey <- function(.data, var_1, var_2) {

      var_1 <- dplyr::case_when(

         input$select_ma_var_1 == "Primer relación" ~ "metodo_primera_relacion",
         input$select_ma_var_1 == "Últimos 6 meses" ~ "metodo_ultimos_seis_meses"

      )

      var_2 <- dplyr::case_when(

         input$select_ma_var_2 == "Últimos 6 meses" ~ "metodo_ultimos_seis_meses",
         input$select_ma_var_2 == "Última relación" ~ "metodo_ultima_relacion"

      )

      aux_data <- .data %>%
         dplyr::group_by(
            var_1 := !!rlang::sym(var_1),
            var_2 := !!rlang::sym(var_2)
         ) %>%
         dplyr::summarise(
            n = dplyr::n()
         ) %>%
         dplyr::ungroup() %>%
         dplyr::filter(
            stats::complete.cases(.)
         ) %>%
         dplyr::transmute(
            source = var_1,
            target = stringr::str_c(var_2, " "),
            value = n
         )

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
         # width = 100,
         sinksRight = FALSE,
         nodeWidth = 40,
         fontSize = 13,
         nodePadding = 20
      )

   }


   # Tab ideales -------------------------------------------------------------
   var_name <- shiny::reactive({

      dplyr::case_when(

         input$pregunta_ideales == "Cantidad ideal de hijes" ~ "cantidad_ideal_hijos",
         input$pregunta_ideales == "Edad ideal para tener el primer hije" ~ "edad_ideal_primer_hijo",
         input$pregunta_ideales == "Edad límite inferior para tener sexo" ~ "edad_limit_inf_sexo_",
         input$pregunta_ideales == "Edad límite inferior para tener hijes" ~ "edad_limit_inf_hijos_",
         input$pregunta_ideales == "Edad límite superior para tener hijes" ~ "edad_limit_sup_hijos_",
         input$pregunta_ideales == "Edad límite inferior para abandonar estudios" ~ "edad_limit_inf_abandonar_estudios_"

      )

   })

   ## Texto de la pregunta superior
   output$texto_pregunta_sup <- shiny::renderText({

      texto_pregunta <- dplyr::case_when(

         input$pregunta_ideales == "Cantidad ideal de hijes" ~ "Si pudiera volver atrás en el tiempo y elegir el número de hijos para tener en su vida, ¿cuántos serían? (Para quienes tuvieron hijos)",
         input$pregunta_ideales == "Edad ideal para tener el primer hije" ~ "Si pudiera volver atrás en el tiempo y elegir la edad a la cual tener su primer hijo/a, ¿cuál sería? (Para quienes tuvieron hijos)",
         input$pregunta_ideales == "Edad límite inferior para tener sexo" ~ "¿A qué edad le parece que una mujer es demasiado joven para tener relaciones sexuales?",
         input$pregunta_ideales == "Edad límite inferior para tener hijes" ~ "¿A qué edad le parece que una mujer es demasiado joven para tener hijos?",
         input$pregunta_ideales == "Edad límite superior para tener hijes" ~ "¿A qué edad le parece que una mujer es demasiado mayor para tener hijos?",
         input$pregunta_ideales == "Edad límite inferior para abandonar estudios" ~ "¿A qué edad le parece que una mujer es demasiado joven para abandonar los estudios en forma definitiva?"

      )

      base::paste("Pregunta:", texto_pregunta)

   })

   ## Plot superior
   output$plot_sup <- plotly::renderPlotly({

      if (var_name() %in% base::c("cantidad_ideal_hijos", "edad_ideal_primer_hijo")) {

         plotly_questions_one(
            q = var_name(),
            th = "Sí"
         )

      } else {

         plotly_questions_two(
            q = base::paste0(var_name(), "mujeres")
         )

      }

   })

   ## Texto de la pregunta inferior
   output$texto_pregunta_inf <- shiny::renderText({

      texto_pregunta <- dplyr::case_when(

         input$pregunta_ideales == "Cantidad ideal de hijes" ~ "Si pudiera volver atrás en el tiempo y elegir el número de hijos para tener en su vida, ¿cuántos serían? (Para quienes no tuvieron hijos)",
         input$pregunta_ideales == "Edad ideal para tener el primer hije" ~ "Si pudiera volver atrás en el tiempo y elegir la edad a la cual tener su primer hijo/a, ¿cuál sería? (Para quienes no tuvieron hijos)",
         input$pregunta_ideales == "Edad límite inferior para tener sexo" ~ "¿A qué edad le parece que un hombre es demasiado joven para tener relaciones sexuales?",
         input$pregunta_ideales == "Edad límite inferior para tener hijes" ~ "¿A qué edad le parece que un hombre es demasiado joven para tener hijos?",
         input$pregunta_ideales == "Edad límite superior para tener hijes" ~ "¿A qué edad le parece que un hombre es demasiado mayor para tener hijos?",
         input$pregunta_ideales == "Edad límite inferior para abandonar estudios" ~ "¿A qué edad le parece que un hombre es demasiado joven para abandonar los estudios en forma definitiva?"

      )

      base::paste("Pregunta:", texto_pregunta)

   })

   ## Plot inferior
   output$plot_inf <- plotly::renderPlotly({

      if (var_name() %in% base::c("cantidad_ideal_hijos", "edad_ideal_primer_hijo")) {

         plotly_questions_one(
            q = var_name(),
            th = "No"
         )

      } else {

         plotly_questions_two(
            q = base::paste0(var_name(), "varones")
         )

      }

   })


   # Tab maternidad ----------------------------------------------------------
   var_name_motherhood <- shiny::reactive({

      dplyr::case_when(

         input$pregunta_motherhood == "Madre antes de los 18" ~ "madre_antes_18",
         input$pregunta_motherhood == "No tener hijes" ~ "mujer_no_tener_hijos",
         input$pregunta_motherhood == "Vivir en pareja sin casarse" ~ "mujer_vivir_en_pareja_sin_casarse",
         input$pregunta_motherhood == "Tener hijes con concubino" ~ "mujer_tener_hijos_con_concu",
         input$pregunta_motherhood == "Trabajar tiempo completo con hijes" ~ "mujer_trabajar_full_con_hijos_menores_3",
         input$pregunta_motherhood == "Divorciarse con hijes" ~ "mujer_divorciarse_con_hijos_menores_12",
         input$pregunta_motherhood == "Cuidado de los hijes" ~ "cuidado_hijos_mujer_ppal",
         input$pregunta_motherhood == "Realización (mujeres)" ~ "mujer_se_realiza_cuando_es_madre",
         input$pregunta_motherhood == "Vida familiar" ~ "mujer_trabaja_full_perjudica_flia",
         input$pregunta_motherhood == "Realización (varones)" ~ "varon_se_realiza_cuando_es_padre"

      )

   })

   ## Texto de la pregunta motherhood
   output$texto_pregunta_motherhood <- shiny::renderText({

      texto_pregunta <- dplyr::case_when(

         input$pregunta_motherhood == "Madre antes de los 18" ~ "Una mujer puede ser madre antes de los 18 años",
         input$pregunta_motherhood == "No tener hijes" ~ "Una mujer puede decidir no tener hijos",
         input$pregunta_motherhood == "Vivir en pareja sin casarse" ~ "Una mujer puede vivir en pareja sin estar casada",
         input$pregunta_motherhood == "Tener hijes con concubino" ~ "Una mujer puede tener hijos/as con la pareja que vive sin estar casada",
         input$pregunta_motherhood == "Trabajar tiempo completo con hijes" ~ "Una mujer puede tener un trabajo a tiempo completo teniendo hijos/as menores de 3 años",
         input$pregunta_motherhood == "Divorciarse con hijes" ~ "Una mujer puede separarse o divorciarse teniendo hijos menores de 12 años",
         input$pregunta_motherhood == "Cuidado de los hijes" ~ "El cuidado de los hijos debe ser tarea principalmente de la mujer",
         input$pregunta_motherhood == "Realización (mujeres)" ~ "Una mujer se realiza plenamente cuando es madre",
         input$pregunta_motherhood == "Vida familiar" ~ "Cuando la mujer tiene un trabajo de jornada completa la vida familiar se perjudica",
         input$pregunta_motherhood == "Realización (varones)" ~ "Un hombre se realiza plenamente cuando es padre"

      )

      base::paste("Pregunta:", texto_pregunta)

   })

   ## Plot motherhood
   output$plot_motherhood <- plotly::renderPlotly({

      plotly_question_motherhood(
         q = var_name_motherhood()
      )

   })


   # Tab métodos anticonceptivos ---------------------------------------------

   ## Texto explicativo
   output$texto_metodos_anticonceptivos <- shiny::renderText({

      base::paste(
         "Métodos anticonceptivos utilizados por los encuestados, para aquellos que ya tuvieron su primer relación sexual. En la columna izquierda,
         los métodos utilizados durante",
         dplyr::case_when(
            input$select_ma_var_1 == "Primer relación" ~ "la",
            input$select_ma_var_1 == "Últimos 6 meses" ~ "los"
         ),
         stringr::str_to_lower(input$select_ma_var_1),
         ". En la columna de la derecha, los métodos utilizados durante",
         dplyr::case_when(
            input$select_ma_var_2 == "Últimos 6 meses" ~ "los",
            input$select_ma_var_2 == "Última relación" ~ "la"
         ),
         stringr::str_to_lower(input$select_ma_var_2),
         "."
         )

   })

   output$sankey_metodos_anticonceptivos <- networkD3::renderSankeyNetwork({

      metodos_anticonceptivos %>%
         dplyr::filter(
            sexo %in% input$select_ma_sexo,
            rango_edad %in% input$select_ma_rango_edad,
            dplyr::between(edad_primera_relacion, input$select_ma_rango_edad_primera_relacion[1], input$select_ma_rango_edad_primera_relacion[2])
         ) %>%
         generar_sankey(
            var_1 = input$select_ma_var_1,
            var_2 = input$select_ma_var_2
         )

   })

}


# Shiny App ---------------------------------------------------------------

shiny::shinyApp(
   ui = ui,
   server = server
)

#===============#
#### THE END ####
#===============#