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

## agregar inas adicionales




# para comparar -----------------------------------------------------------

## hace scatter plots donde el tamaño sea la cantidad de personas que contestaron a esa pregutna en esas cantidades

#===============#
#### THE END ####
#===============#