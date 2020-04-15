#=============#
#### ENCoR ####
#=============#

# library(survey)
library(magrittr)
library(tidyverse)

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
      ),

      ## Edad límit inferior para tener sexo (mujeres)
      edad_limit_inf_sexo_mujeres = dplyr::if_else(ina44 == 2, base::max(encor$ina44_1, na.rm = TRUE) + 1, ina44_1),
      edad_limit_inf_sexo_mujeres = base::factor(
         x = edad_limit_inf_sexo_mujeres,
         levels = base::sort(base::unique(edad_limit_inf_sexo_mujeres))
      ),
      edad_limit_inf_sexo_mujeres = forcats::fct_explicit_na(
         f = edad_limit_inf_sexo_mujeres,
         na_level = "Ns/Nc"
      ),
      edad_limit_inf_sexo_mujeres = forcats::fct_recode(
         .f = edad_limit_inf_sexo_mujeres,
         Depende = base::as.character(base::max(encor$ina44_1, na.rm = TRUE) + 1)
      ),

      ## Edad límit inferior para tener sexo (varones)
      edad_limit_inf_sexo_varones = dplyr::if_else(ina48 == 2, base::max(encor$ina48_1, na.rm = TRUE) + 1, ina48_1),
      edad_limit_inf_sexo_varones = base::factor(
         x = edad_limit_inf_sexo_varones,
         levels = base::sort(base::unique(edad_limit_inf_sexo_varones))
      ),
      edad_limit_inf_sexo_varones = forcats::fct_explicit_na(
         f = edad_limit_inf_sexo_varones,
         na_level = "Ns/Nc"
      ),
      edad_limit_inf_sexo_varones = forcats::fct_recode(
         .f = edad_limit_inf_sexo_varones,
         Depende = base::as.character(base::max(encor$ina48_1, na.rm = TRUE) + 1)
      ),

      ## Edad límit inferior para tener hijos (mujeres)
      edad_limit_inf_hijos_mujeres = dplyr::if_else(ina45 == 2, base::max(encor$ina45_1, na.rm = TRUE) + 1, ina45_1),
      edad_limit_inf_hijos_mujeres = base::factor(
         x = edad_limit_inf_hijos_mujeres,
         levels = base::sort(base::unique(edad_limit_inf_hijos_mujeres))
      ),
      edad_limit_inf_hijos_mujeres = forcats::fct_explicit_na(
         f = edad_limit_inf_hijos_mujeres,
         na_level = "Ns/Nc"
      ),
      edad_limit_inf_hijos_mujeres = forcats::fct_recode(
         .f = edad_limit_inf_hijos_mujeres,
         Depende = base::as.character(base::max(encor$ina45_1, na.rm = TRUE) + 1)
      ),

      ## Edad límit superior para tener hijos (mujeres)
      edad_limit_sup_hijos_mujeres = dplyr::if_else(ina46 == 2, base::max(encor$ina46_1, na.rm = TRUE) + 1, ina46_1),
      edad_limit_sup_hijos_mujeres = base::factor(
         x = edad_limit_sup_hijos_mujeres,
         levels = base::sort(base::unique(edad_limit_sup_hijos_mujeres))
      ),
      edad_limit_sup_hijos_mujeres = forcats::fct_explicit_na(
         f = edad_limit_sup_hijos_mujeres,
         na_level = "Ns/Nc"
      ),
      edad_limit_sup_hijos_mujeres = forcats::fct_recode(
         .f = edad_limit_sup_hijos_mujeres,
         Depende = base::as.character(base::max(encor$ina46_1, na.rm = TRUE) + 1)
      ),

      ## Edad límit inferior para tener hijos (varones)
      edad_limit_inf_hijos_varones = dplyr::if_else(ina49 == 2, base::max(encor$ina49_1, na.rm = TRUE) + 1, ina49_1),
      edad_limit_inf_hijos_varones = base::factor(
         x = edad_limit_inf_hijos_varones,
         levels = base::sort(base::unique(edad_limit_inf_hijos_varones))
      ),
      edad_limit_inf_hijos_varones = forcats::fct_explicit_na(
         f = edad_limit_inf_hijos_varones,
         na_level = "Ns/Nc"
      ),
      edad_limit_inf_hijos_varones = forcats::fct_recode(
         .f = edad_limit_inf_hijos_varones,
         Depende = base::as.character(base::max(encor$ina49_1, na.rm = TRUE) + 1)
      ),

      ## Edad límit superior para tener hijos (varones)
      edad_limit_sup_hijos_varones = dplyr::if_else(ina50 == 2, base::max(encor$ina50_1, na.rm = TRUE) + 1, ina50_1),
      edad_limit_sup_hijos_varones = base::factor(
         x = edad_limit_sup_hijos_varones,
         levels = base::sort(base::unique(edad_limit_sup_hijos_varones))
      ),
      edad_limit_sup_hijos_varones = forcats::fct_explicit_na(
         f = edad_limit_sup_hijos_varones,
         na_level = "Ns/Nc"
      ),
      edad_limit_sup_hijos_varones = forcats::fct_recode(
         .f = edad_limit_sup_hijos_varones,
         Depende = base::as.character(base::max(encor$ina50_1, na.rm = TRUE) + 1)
      ),

      ## Edad límit inferior para abandonar estudios (mujeres)
      edad_limit_inf_abandonar_estudios_mujeres = dplyr::if_else(ina47 == 2, base::max(encor$ina47_1, na.rm = TRUE) + 1, ina47_1),
      edad_limit_inf_abandonar_estudios_mujeres = base::factor(
         x = edad_limit_inf_abandonar_estudios_mujeres,
         levels = base::sort(base::unique(edad_limit_inf_abandonar_estudios_mujeres))
      ),
      edad_limit_inf_abandonar_estudios_mujeres = forcats::fct_explicit_na(
         f = edad_limit_inf_abandonar_estudios_mujeres,
         na_level = "Ns/Nc"
      ),
      edad_limit_inf_abandonar_estudios_mujeres = forcats::fct_recode(
         .f = edad_limit_inf_abandonar_estudios_mujeres,
         Depende = base::as.character(base::max(encor$ina47_1, na.rm = TRUE) + 1)
      ),

      ## Edad límit inferior para abandonar estudios (varones)
      edad_limit_inf_abandonar_estudios_varones = dplyr::if_else(ina50 == 2, base::max(encor$ina50_1, na.rm = TRUE) + 1, ina50_1),
      edad_limit_inf_abandonar_estudios_varones = base::factor(
         x = edad_limit_inf_abandonar_estudios_varones,
         levels = base::sort(base::unique(edad_limit_inf_abandonar_estudios_varones))
      ),
      edad_limit_inf_abandonar_estudios_varones = forcats::fct_explicit_na(
         f = edad_limit_inf_abandonar_estudios_varones,
         na_level = "Ns/Nc"
      ),
      edad_limit_inf_abandonar_estudios_varones = forcats::fct_recode(
         .f = edad_limit_inf_abandonar_estudios_varones,
         Depende = base::as.character(base::max(encor$ina50_1, na.rm = TRUE) + 1)
      )
   ) %>%
   dplyr::select(
      sexo,
      edad_act,
      tuvo_hijos,
      cantidad_ideal_hijos,
      edad_ideal_primer_hijo,
      edad_limit_inf_sexo_mujeres,
      edad_limit_inf_sexo_varones,
      edad_limit_inf_hijos_mujeres,
      edad_limit_inf_hijos_varones,
      edad_limit_sup_hijos_mujeres,
      edad_limit_sup_hijos_varones,
      edad_limit_inf_abandonar_estudios_mujeres,
      edad_limit_inf_abandonar_estudios_varones
   )

readr::write_rds(x = encor, path = here::here("encore.rds"))

plotly_questions_two <- function(q, genero) {

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
         type = "bar"
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

}



# para comparar -----------------------------------------------------------

## hace scatter plots donde el tamaño sea la cantidad de personas que contestaron a esa pregutna en esas cantidades

#===============#
#### THE END ####
#===============#