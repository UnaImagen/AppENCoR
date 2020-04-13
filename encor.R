#=============#
#### ENCoR ####
#=============#

# library(survey)
library(tidyverse)
library(magrittr)

# Carga datos -------------------------------------------------------------
encor <- haven::read_sav(
   file = here::here("Base ENCoR terceros.sav")
) %>%
   mutate(
      sexo = as_factor(sexo)
   )

readr::write_rds(x = encor, path = here::here("encore.rds"))



# unir preguntas ----------------------------------------------------------



plotly_questions2(q1 = "ina40_1", q2 = "ina42_1")
plotly_questions2(q1 = "ina41_1", q2 = "ina43_1")


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