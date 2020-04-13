#=================#
#### APP ENCOR ####
#=================#

library(shiny)


# UI ----------------------------------------------------------------------
ui <- shiny::tagList(

   shiny::includeCSS(path = "style.css"),

   shiny::navbarPage(

      theme = shinythemes::shinytheme(theme = "united"),

      title = "Encor",

      shiny::tabPanel(

         title = "Resultados",

         shiny::sidebarPanel(

            shiny::h4("Encuesta Nacional de Comportamientos Reporductivos"),

            shiny::selectInput(
               inputId = "pregunta",
               label = "Seleccione una pregunta",
               choices = base::c(
                  "Número ideal de hijes",
                  "Edad ideal para tener el primer hije",
                  "Edad límite inferior para tener relaciones sexuales (mujeres)",
                  "Edad límite inferior para tener hijes (mujeres)",
                  "Edad límite superior para tener hijes (mujeres)",
                  "Edad límite inferior para abandonar estudios (mujeres)",
                  "Edad límite inferior para tener relaciones sexuales (hombres)",
                  "Edad límite inferior para tener hijes (hombres)",
                  "Edad límite superior para tener hijes (hombres)",
                  "Edad límite inferior para abandonar estudios (hombres)"
               ),
               selected = "Número ideal de hijos"
            ),

            shiny::p("Fuente: Instituto Nacional de Estadística")

         ),

         shiny::mainPanel(

            shiny::div(

               class = 'questionDiv',

               shiny::h3(

                  shiny::textOutput(
                     outputId = "textopregunta"
                  )

               )

            ),

            plotly::plotlyOutput(
               outputId = "plot"
            )

         )

      )

   )

)

# Server ------------------------------------------------------------------
server <- function(input, output) {

   library(magrittr, quietly = TRUE)

   encor <- readr::read_rds(path = "encore.rds")

   ## Plotly functions
   plotly_questions <- function(q1) {

      titulo <- dplyr::case_when(
         q1 %in% base::c("ina44_1", "ina45_1", "ina47_1", "ina48_1", "ina49_1", "ina51_1") ~ "<b>Edad límite inferior</b>",
         q1 %in% base::c("ina46_1", "ina50_1") ~ "<b>Edad límite superior</b>"
      )

      encor %>%
         dplyr::transmute(
            sexo,
            variable := !!rlang::sym(q1)
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

   plotly_questions2 <- function(q1, q2) {

      titulo <- dplyr::case_when(
         q1 == "ina40_1" ~ "<b>Cantidad ideal de hijes</b>",
         q1 == "ina41_1" ~ "<b>Edad ideal para primer hije</b>"
      )

      encor %>%
         dplyr::mutate(
            variable = dplyr::if_else(is.na(!!rlang::sym(q1)), !!rlang::sym(q2), !!rlang::sym(q1)),
            variable = forcats::as_factor(variable),
            variable = forcats::fct_explicit_na(
               f = variable,
               na_level = "Ns/Nc"
            )
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

   ## Texto de la pregunta
   output$textopregunta <- shiny::renderText({

      textopregunta <- dplyr::case_when(

         input$pregunta == "Edad límite inferior para tener relaciones sexuales (mujeres)" ~ "A qué edad le parece que una mujer es demasiado joven para tener relaciones sexuales?",
         input$pregunta == "Edad límite inferior para tener hijes (mujeres)" ~ "¿A qué edad le parece que una mujer es demasiado joven para tener hijos?",
         input$pregunta == "Edad límite superior para tener hijes (mujeres)" ~ "¿A qué edad le parece que una mujer es demasiado mayor para tener hijos?",
         input$pregunta == "Edad límite inferior para abandonar estudios (mujeres)" ~ "¿A qué edad le parece que una mujer es demasiado joven para abandonar los estudios en forma definitiva?",
         input$pregunta == "Edad límite inferior para tener relaciones sexuales (hombres)" ~ "¿A qué edad le parece que un hombre es demasiado joven para tener relaciones sexuales?",
         input$pregunta == "Edad límite inferior para tener hijes (hombres)" ~ "¿A qué edad le parece que un hombre es demasiado joven para tener hijos?",
         input$pregunta == "Edad límite superior para tener hijes (hombres)" ~ "¿A qué edad le parece que un hombre es demasiado mayor para tener hijos?",
         input$pregunta == "Edad límite inferior para abandonar estudios (hombres)" ~ "¿A qué edad le parece que un hombre es demasiado joven para abandonar los estudios en forma definitiva?",

         input$pregunta == "Número ideal de hijes" ~ "Si pudiera volver atrás en el tiempo y elegir el número de hijos para tener en su vida, ¿cuántos serían?",
         input$pregunta == "Edad ideal para tener el primer hije" ~ "Si pudiera volver atrás en el tiempo y elegir la edad a la cual tener su primer hijo/a, ¿cuál sería?",

         TRUE ~ input$pregunta
      )

      base::paste("Pregunta:", textopregunta)

   })


   ## Plot de resultados
   output$plot <- plotly::renderPlotly({

      question_set <- dplyr::case_when(

         input$pregunta %in% base::c(
            "Edad límite inferior para tener relaciones sexuales (mujeres)",
            "Edad límite inferior para tener hijes (mujeres)",
            "Edad límite superior para tener hijes (mujeres)",
            "Edad límite inferior para abandonar estudios (mujeres)",
            "Edad límite inferior para tener relaciones sexuales (hombres)",
            "Edad límite inferior para tener hijes (hombres)",
            "Edad límite superior para tener hijes (hombres)",
            "Edad límite inferior para abandonar estudios (hombres)"
         ) ~ 1,

         input$pregunta %in% base::c(
            "Número ideal de hijes",
            "Edad ideal para tener el primer hije"
         ) ~ 2

      )

      var_id <- dplyr::case_when(

         input$pregunta == "Edad límite inferior para tener relaciones sexuales (mujeres)" ~ "ina44_1",
         input$pregunta == "Edad límite inferior para tener hijes (mujeres)" ~ "ina45_1",
         input$pregunta == "Edad límite superior para tener hijes (mujeres)" ~ "ina46_1",
         input$pregunta == "Edad límite inferior para abandonar estudios (mujeres)" ~ "ina47_1",
         input$pregunta == "Edad límite inferior para tener relaciones sexuales (hombres)" ~ "ina48_1",
         input$pregunta == "Edad límite inferior para tener hijes (hombres)" ~ "ina49_1",
         input$pregunta == "Edad límite superior para tener hijes (hombres)" ~ "ina50_1",
         input$pregunta == "Edad límite inferior para abandonar estudios (hombres)" ~ "ina51_1",

         input$pregunta == "Número ideal de hijes" ~ base::c("ina40_1", "ina42_1"),
         input$pregunta == "Edad ideal para tener el primer hije" ~ base::c("ina41_1", "ina43_1"),

         TRUE ~ input$pregunta
      )

      if (question_set == 1) {

         plotly_questions(
            q1 = var_id[1]
         )

      } else  if (question_set == 2) {

         plotly_questions2(
            q1 = var_id[1],
            q2 = var_id[2]
         )

      }

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