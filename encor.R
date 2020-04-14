#=============#
#### ENCoR ####
#=============#

# library(survey)
library(magrittr)

# Carga datos -------------------------------------------------------------
encor <- haven::read_sav(
   file = here::here("Base ENCoR terceros.sav")
)


# Genera rds para App -----------------------------------------------------
encor %<>%
   dplyr::mutate(
      # Sexo
      sexo = forcats::as_factor(sexo),

      # Tuvo hijos
      tuvo_hijos = stringr::str_to_sentence(forcats::as_factor(hr10)),
      tuvo_hijos = forcats::as_factor(tuvo_hijos),

      # Cantidad ideal de hijos
      ina40 = dplyr::if_else(ina40 == 1 & base::is.na(ina40_1), 2, base::as.numeric(ina40)),
      ina42 = dplyr::if_else(ina42 == 1 & base::is.na(ina42_1), 2, base::as.numeric(ina42)),
      cantidad_ideal_hijos = dplyr::case_when(
         hr10 == 1 & ina40 == 1 ~ ina40_1,
         hr10 == 1 & ina40 == 2 ~ NA_real_,
         hr10 == 2 & ina42 == 1 ~ ina42_1,
         hr10 == 2 & ina42 == 2 ~ NA_real_,
         TRUE ~ NA_real_
      ),
      cantidad_ideal_hijos = base::factor(
         x = cantidad_ideal_hijos,
         levels = base::sort(base::unique(cantidad_ideal_hijos))
      ),
      cantidad_ideal_hijos = forcats::fct_explicit_na(
         f = cantidad_ideal_hijos,
         na_level = "Ns/Nc"
      ),

      # Edad ideal para tener el primer hijo
      ina41 = dplyr::if_else(ina41 == 1 & base::is.na(ina41_1), 2, base::as.numeric(ina41)),
      ina43 = dplyr::if_else(ina43 == 1 & base::is.na(ina43_1), 2, base::as.numeric(ina43)),
      edad_ideal_primer_hijo = dplyr::case_when(
         hr10 == 1 & ina41 == 1 ~ ina41_1,
         hr10 == 1 & ina41 == 2 ~ NA_real_,
         hr10 == 2 & ina43 == 1 ~ ina43_1,
         hr10 == 2 & ina43 == 2 ~ NA_real_,
         TRUE ~ NA_real_
      ),
      edad_ideal_primer_hijo = base::factor(
         x = edad_ideal_primer_hijo,
         levels = base::sort(base::unique(edad_ideal_primer_hijo))
      ),
      edad_ideal_primer_hijo = forcats::fct_explicit_na(
         f = edad_ideal_primer_hijo,
         na_level = "Ns/Nc"
      )
   ) %>%
   dplyr::select(
      sexo,
      edad_act,
      tuvo_hijos,
      cantidad_ideal_hijos,
      edad_ideal_primer_hijo
   )

readr::write_rds(x = encor, path = here::here("encore.rds"))


encor %>%
   dplyr::mutate(
      variable := cantidad_ideal_hijos
   ) %>%
   dplyr::filter(
      tuvo_hijos == "Sí"
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
         title = "titulo"
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



# para comparar -----------------------------------------------------------
encor %>%
   dplyr::select(
      sexo,
      ina44_1, # !!rlang::sym(mujer),
      ina48_1, #!!rlang::sym(hombre),
      peso
   ) %>%
   dplyr::gather(
      key = pregunta,
      value = edad,
      -sexo,
      -peso
   ) %>%
   dplyr::mutate(
      pregunta = dplyr::case_when(
         pregunta == "ina44_1" ~ "Mujer",
         pregunta == "ina48_1" ~ "Hombre",
      ),
      sexo = forcats::as_factor(sexo),
      sexo = forcats::fct_recode(
         .f = sexo,
         "La encuestada es mujer" = "mujer",
         "El encuestado es hombre" = "hombre"
      )
   )


#===============#
#### THE END ####
#===============#