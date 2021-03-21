#=================#
#### APP ENCOR ####
#=================#

library(shiny, quietly = TRUE)
library(magrittr, quietly = TRUE)

encor <- readr::read_rds(file = "encor.rds")
metodos_anticonceptivos <- readr::read_rds(file = "metodos_anticonceptivos.rds")

title <- "Encuesta Nacional de Comportamientos Reproductivos"

data_source <- "Fuente: Instituto Nacional de Estadística"

links <- shiny::HTML(
   '<a href="https://danielczarnievicz.netlify.app/portfolio/comportamientos-reporductivos/"><i class="fas fa-arrow-circle-left"></i></a>&nbsp;
   <a href="https://github.com/daczarne/AppENCoR"><i class="fab fa-github"></i></a>&nbsp;
   <a href="https://stackoverflow.com/users/5908830/daniel?tab=profile"><i class="fab fa-stack-overflow"></i></a>&nbsp;
   <a href="https://twitter.com/daczarne"><i class="fab fa-twitter"></i></a>&nbsp;
   <a href="https://www.linkedin.com/in/danielczarnievicz/"><i class="fab fa-linkedin"></i></a>&nbsp;
   <a href="https://danielczarnievicz.netlify.app/portfolio/"><i class="fas fa-home"></i></a>&nbsp;'
)

# UI ----------------------------------------------------------------------
ui <- shiny::tagList(

   shiny::includeCSS(path = "www/style.css"),

   shiny::navbarPage(

      theme = shinythemes::shinytheme(theme = "united"),

      title = "ENCoR",

      # Tab ideales -------------------------------------------------------------
      shiny::tabPanel(

         title = "Ideales",

         shiny::sidebarPanel(

            shiny::h4(title),

            shiny::selectInput(
               inputId = "pregunta_ideales",
               label = "Seleccione una categoría",
               choices = base::list(
                  "Cantidad ideal de hijos" = "cantidad_ideal_hijos",
                  "Edad ideal para tener el primer hijo" = "edad_ideal_primer_hijo",
                  "Edad límite inferior para tener sexo" = "edad_limit_inf_sexo_",
                  "Edad límite inferior para tener hijos" = "edad_limit_inf_hijos_",
                  "Edad límite superior para tener hijos" = "edad_limit_sup_hijos_",
                  "Edad límite inferior para abandonar estudios" = "edad_limit_inf_abandonar_estudios_"
               ),
               selected = "cantidad_ideal_hijos",
               multiple = FALSE
            ),

            shiny::p(data_source),

            shiny::p(links),

            shiny::icon(" ")

         ),

         shiny::mainPanel(

            questionDivUI("texto_pregunta_sup"),
            plotly::plotlyOutput(
               outputId = "plot_sup"
            ),

            questionDivUI("texto_pregunta_inf"),
            plotly::plotlyOutput(
               outputId = "plot_inf"
            )

         )

      ),

      # Tab motherhood ----------------------------------------------------------
      shiny::tabPanel(

         title = "Maternidad",

         shiny::sidebarPanel(

            shiny::h4(title),

            shiny::selectInput(
               inputId = "pregunta_motherhood",
               label = "Seleccione una categoría",
               choices = base::list(
                  "Madre antes de los 18" = "madre_antes_18",
                  "No tener hijos" = "mujer_no_tener_hijos",
                  "Vivir en pareja sin casarse" = "mujer_vivir_en_pareja_sin_casarse",
                  "Tener hijos con concubino" = "mujer_tener_hijos_con_concu",
                  "Trabajar tiempo completo con hijos" = "mujer_trabajar_full_con_hijos_menores_3",
                  "Divorciarse con hijos" = "mujer_divorciarse_con_hijos_menores_12",
                  "Cuidado de los hijos" = "cuidado_hijos_mujer_ppal",
                  "Realización (mujeres)" = "mujer_se_realiza_cuando_es_madre",
                  "Vida familiar" = "mujer_trabaja_full_perjudica_flia",
                  "Realización (varones)" = "varon_se_realiza_cuando_es_padre"
               ),
               selected = "madre_antes_18"
            ),

            shiny::p(data_source),

            shiny::p(links),

         ),

         shiny::mainPanel(

            questionDivUI("texto_pregunta_motherhood"),
            plotly::plotlyOutput(
               outputId = "plot_motherhood"
            )

         )

      ),

      # Tab métodos anticonceptivos ---------------------------------------------
      shiny::tabPanel(

         title = "Métodos anticonceptivos",

         shiny::sidebarPanel(

            shiny::h4(title),

            shiny::selectInput(
               inputId = "select_ma_var_1",
               label = "Comparar... ",
               choices = base::list(
                  "Primer relación" = "metodo_primera_relacion",
                  "Últimos 6 meses" = "metodo_ultimos_seis_meses"
               ),
               selected = "metodo_primera_relacion"
            ),

            shiny::selectInput(
               inputId = "select_ma_var_2",
               label = "... con ",
               choices = base::list(
                  "Últimos 6 meses" = "metodo_ultimos_seis_meses",
                  "Última relación" = "metodo_ultima_relacion"
               ),
               selected = "metodo_ultima_relacion"
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

            shiny::p(data_source),

            shiny::p(links),

         ),

         shiny::mainPanel(

            questionDivUI("texto_metodos_anticonceptivos"),
            networkD3::sankeyNetworkOutput(
               outputId = "sankey_metodos_anticonceptivos"
            )

         )

      ),

      # Tab comparar respuestas ---------------------------------------------------
      shiny::tabPanel(

         title = "Comparar respuestas",

         shiny::sidebarPanel(

            shiny::h4("Encuesta Nacional de Comportamientos Reproductivos"),

            shiny::p("Compará las respuestas de una misma persona a dos preguntas distintas. El tamaño de los puntos indica el porcentaje de
                     personas que contestaron la misma combinación de respuestas."),

            shiny::selectInput(
               inputId = "select_qc_sexo",
               label = "Cómo contestaron... ",
               choices = base::c("hombres", "mujeres"),
               selected = "hombres"
            ),

            shiny::selectInput(
               inputId = "select_qc_var_x",
               label = "... a ... ",
               choices = base::list(
                  "Cantidad ideal de hijos" = "cantidad_ideal_hijos",
                  "Cantidad de hijos" = "cantidad_hijos",
                  "Edad ideal para tener el primer hijo" = "edad_ideal_primer_hijo",
                  "Edad límite inferior para tener sexo (mujeres)" = "edad_limit_inf_sexo_mujeres",
                  "Edad límite inferior para tener sexo (hombres)" = "edad_limit_inf_sexo_varones",
                  "Edad límite inferior para tener hijos (mujeres)" = "edad_limit_inf_hijos_mujeres",
                  "Edad límite inferior para tener hijos (hombres)" = "edad_limit_inf_hijos_varones",
                  "Edad límite superior para tener hijos (mujeres)" = "edad_limit_sup_hijos_mujeres",
                  "Edad límite superior para tener hijos (hombres)" = "edad_limit_sup_hijos_varones",
                  "Edad límite inferior para abandonar estudios (mujeres)" = "edad_limit_inf_abandonar_estudios_mujeres",
                  "Edad límite inferior para abandonar estudios (hombres)" = "edad_limit_inf_abandonar_estudios_varones"
               ),
               selected = "cantidad_ideal_hijos",
               multiple = FALSE
            ),

            shiny::selectInput(
               inputId = "select_qc_var_y",
               label = "... versus ... ",
               choices = base::c(
                  "Cantidad ideal de hijos" = "cantidad_ideal_hijos",
                  "Cantidad de hijos" = "cantidad_hijos",
                  "Edad ideal para tener el primer hijo" = "edad_ideal_primer_hijo",
                  "Edad límite inferior para tener sexo (mujeres)" = "edad_limit_inf_sexo_mujeres",
                  "Edad límite inferior para tener sexo (hombres)" = "edad_limit_inf_sexo_varones",
                  "Edad límite inferior para tener hijos (mujeres)" = "edad_limit_inf_hijos_mujeres",
                  "Edad límite inferior para tener hijos (hombres)" = "edad_limit_inf_hijos_varones",
                  "Edad límite superior para tener hijos (mujeres)" = "edad_limit_sup_hijos_mujeres",
                  "Edad límite superior para tener hijos (hombres)" = "edad_limit_sup_hijos_varones",
                  "Edad límite inferior para abandonar estudios (mujeres)" = "edad_limit_inf_abandonar_estudios_mujeres",
                  "Edad límite inferior para abandonar estudios (hombres)" = "edad_limit_inf_abandonar_estudios_varones"
               ),
               selected = "cantidad_hijos"
            ),

            shiny::p("Fuente: Instituto Nacional de Estadística"),

            shiny::p(links),

         ),

         shiny::mainPanel(

            plotly::plotlyOutput(
               outputId = "plot_comparar"
            )

         )

      )

   )

)

# Server ------------------------------------------------------------------
server <- function(input, output) {

   # Tab ideales -------------------------------------------------------------

   ## Texto de la pregunta superior
   output$texto_pregunta_sup <- shiny::renderText({

      texto_pregunta <- dplyr::case_when(

         input$pregunta_ideales == "cantidad_ideal_hijos" ~ "Si pudiera volver atrás en el tiempo y elegir el número de hijos para tener en su vida, ¿cuántos serían? (para quienes tuvieron hijos)",
         input$pregunta_ideales == "edad_ideal_primer_hijo" ~ "Si pudiera volver atrás en el tiempo y elegir la edad a la cual tener su primer hijo/a, ¿cuál sería? (para quienes tuvieron hijos)",
         input$pregunta_ideales == "edad_limit_inf_sexo_" ~ "¿A qué edad le parece que una mujer es demasiado joven para tener relaciones sexuales?",
         input$pregunta_ideales == "edad_limit_inf_hijos_" ~ "¿A qué edad le parece que una mujer es demasiado joven para tener hijos?",
         input$pregunta_ideales == "edad_limit_sup_hijos_" ~ "¿A qué edad le parece que una mujer es demasiado mayor para tener hijos?",
         input$pregunta_ideales == "edad_limit_inf_abandonar_estudios_" ~ "¿A qué edad le parece que una mujer es demasiado joven para abandonar los estudios en forma definitiva?"

      )

      base::paste("Pregunta:", texto_pregunta)

   })

   ## Plot superior
   output$plot_sup <- plotly::renderPlotly({

      if (input$pregunta_ideales %in% base::c("cantidad_ideal_hijos", "edad_ideal_primer_hijo")) {

         encor %>%
            plotly_questions_one(
               q = input$pregunta_ideales,
               th = "Sí"
            )

      } else {

         encor %>%
            plotly_questions_two(
               q = base::paste0(input$pregunta_ideales, "mujeres")
            )

      }

   })

   ## Texto de la pregunta inferior
   output$texto_pregunta_inf <- shiny::renderText({

      texto_pregunta <- dplyr::case_when(

         input$pregunta_ideales == "cantidad_ideal_hijos" ~ "Si pudiera volver atrás en el tiempo y elegir el número de hijos para tener en su vida, ¿cuántos serían? (para quienes no tuvieron hijos)",
         input$pregunta_ideales == "edad_ideal_primer_hijo" ~ "Si pudiera volver atrás en el tiempo y elegir la edad a la cual tener su primer hijo/a, ¿cuál sería? (para quienes no tuvieron hijos)",
         input$pregunta_ideales == "edad_limit_inf_sexo_" ~ "¿A qué edad le parece que un hombre es demasiado joven para tener relaciones sexuales?",
         input$pregunta_ideales == "edad_limit_inf_hijos_" ~ "¿A qué edad le parece que un hombre es demasiado joven para tener hijos?",
         input$pregunta_ideales == "edad_limit_sup_hijos_" ~ "¿A qué edad le parece que un hombre es demasiado mayor para tener hijos?",
         input$pregunta_ideales == "edad_limit_inf_abandonar_estudios_" ~ "¿A qué edad le parece que un hombre es demasiado joven para abandonar los estudios en forma definitiva?"

      )

      base::paste("Pregunta:", texto_pregunta)

   })

   ## Plot inferior
   output$plot_inf <- plotly::renderPlotly({

      if (input$pregunta_ideales %in% base::c("cantidad_ideal_hijos", "edad_ideal_primer_hijo")) {

         encor %>%
            plotly_questions_one(
               q = input$pregunta_ideales,
               th = "No"
            )

      } else {

         encor %>%
            plotly_questions_two(
               q = base::paste0(input$pregunta_ideales, "varones")
            )

      }

   })


   # Tab maternidad ----------------------------------------------------------

   ## Texto de la pregunta motherhood
   output$texto_pregunta_motherhood <- shiny::renderText({

      texto_pregunta <- dplyr::case_when(

         input$pregunta_motherhood == "madre_antes_18" ~ "Una mujer puede ser madre antes de los 18 años",
         input$pregunta_motherhood == "mujer_no_tener_hijos" ~ "Una mujer puede decidir no tener hijos",
         input$pregunta_motherhood == "mujer_vivir_en_pareja_sin_casarse" ~ "Una mujer puede vivir en pareja sin estar casada",
         input$pregunta_motherhood == "mujer_tener_hijos_con_concu" ~ "Una mujer puede tener hijos/as con la pareja que vive sin estar casada",
         input$pregunta_motherhood == "mujer_trabajar_full_con_hijos_menores_3" ~ "Una mujer puede tener un trabajo a tiempo completo teniendo hijos/as menores de 3 años",
         input$pregunta_motherhood == "mujer_divorciarse_con_hijos_menores_12" ~ "Una mujer puede separarse o divorciarse teniendo hijos menores de 12 años",
         input$pregunta_motherhood == "cuidado_hijos_mujer_ppal" ~ "El cuidado de los hijos debe ser tarea principalmente de la mujer",
         input$pregunta_motherhood == "mujer_se_realiza_cuando_es_madre" ~ "Una mujer se realiza plenamente cuando es madre",
         input$pregunta_motherhood == "mujer_trabaja_full_perjudica_flia" ~ "Cuando la mujer tiene un trabajo de jornada completa la vida familiar se perjudica",
         input$pregunta_motherhood == "varon_se_realiza_cuando_es_padre" ~ "Un hombre se realiza plenamente cuando es padre"

      )

      base::paste("Pregunta:", texto_pregunta)

   })

   ## Plot motherhood
   output$plot_motherhood <- plotly::renderPlotly({

      encor %>%
         plotly_question_motherhood(
            q = input$pregunta_motherhood
         )

   })


   # Tab métodos anticonceptivos ---------------------------------------------

   ## Texto explicativo
   output$texto_metodos_anticonceptivos <- shiny::renderText({

      base::paste(
         "Métodos anticonceptivos utilizados por los encuestados, para aquellos que ya tuvieron su primer relación sexual. En la columna izquierda,
         los métodos utilizados durante",
         dplyr::case_when(
            input$select_ma_var_1 == "metodo_primera_relacion" ~ "la primer relación.",
            input$select_ma_var_1 == "metodo_ultimos_seis_meses" ~ "los últimos seis meses."
         ),
         "En la columna de la derecha, los métodos utilizados durante",
         dplyr::case_when(
            input$select_ma_var_2 == "metodo_ultimos_seis_meses" ~ "los últimos seis meses.",
            input$select_ma_var_2 == "metodo_ultima_relacion" ~ "la última relación."
         )
      )

   })

   ## Sankey plot
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


   # Tab comparar respuestas ---------------------------------------------------

   output$plot_comparar <- plotly::renderPlotly({

      ## Plot que compara las respuestas
      encor %>%
         plotly_comparacion(
            gender = dplyr::if_else(input$select_qc_sexo == "hombres", "hombre", "mujer"),
            var_x = input$select_qc_var_x,
            var_y = input$select_qc_var_y
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