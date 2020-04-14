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

         title = "Ideales",

         shiny::sidebarPanel(

            shiny::h4("Encuesta Nacional de Comportamientos Reporductivos"),

            shiny::selectInput(
               inputId = "pregunta",
               label = "Seleccione una pregunta",
               choices = base::c(
                  "Cantidad ideal de hijes",
                  "Edad ideal para tener el primer hije"
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

      )

   )

)

# Server ------------------------------------------------------------------
server <- function(input, output) {

   library(magrittr, quietly = TRUE)

   encor <- readr::read_rds(path = "encore.rds")

   ## Plotly functions
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

   var_name <- shiny::reactive({

      dplyr::case_when(

         input$pregunta == "Cantidad ideal de hijes" ~ "cantidad_ideal_hijos",
         input$pregunta == "Edad ideal para tener el primer hije" ~ "edad_ideal_primer_hijo"

      )

   })

   ## Texto de la pregunta superior
   output$texto_pregunta_sup <- shiny::renderText({

      texto_pregunta <- dplyr::case_when(

         input$pregunta == "Cantidad ideal de hijes" ~ "Si pudiera volver atrás en el tiempo y elegir el número de hijos para tener en su vida, ¿cuántos serían? (Para quienes tuvieron hijos)",
         input$pregunta == "Edad ideal para tener el primer hije" ~ "Si pudiera volver atrás en el tiempo y elegir la edad a la cual tener su primer hijo/a, ¿cuál sería? (Para quienes tuvieron hijos)"

      )

      base::paste("Pregunta:", texto_pregunta)

   })

   ## Plot superior
   output$plot_sup <- plotly::renderPlotly({

      plotly_questions_one(
         q = var_name(),
         th = "Sí"
      )

   })

   ## Texto de la pregunta inferior
   output$texto_pregunta_inf <- shiny::renderText({

      texto_pregunta <- dplyr::case_when(

         input$pregunta == "Cantidad ideal de hijes" ~ "Si pudiera volver atrás en el tiempo y elegir el número de hijos para tener en su vida, ¿cuántos serían? (Para quienes no tuvieron hijos)",
         input$pregunta == "Edad ideal para tener el primer hije" ~ "Si pudiera volver atrás en el tiempo y elegir la edad a la cual tener su primer hijo/a, ¿cuál sería? (Para quienes no tuvieron hijos)"

      )

      base::paste("Pregunta:", texto_pregunta)

   })

   ## Plot inferior
   output$plot_inf <- plotly::renderPlotly({

      plotly_questions_one(
         q = var_name(),
         th = "No"
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