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
      ),

      ## Madre antes de los 18
      madre_antes_18 = forcats::as_factor(ina52_1),

      ## Una mujer puede decidir no tener hijos
      mujer_no_tener_hijos = forcats::as_factor(ina52_2),

      ## Madre vivir en pareja sin estar casada
      mujer_vivir_en_pareja_sin_casarse = forcats::as_factor(ina52_3),

      ## Una mujer puede tener hijos/as con la pareja que vive sin estar casada
      mujer_tener_hijos_con_concu = forcats::as_factor(ina52_4),

      ## Una mujer puede tener un trabajo completo teniendo hijos/as menores de 3 años
      mujer_trabajar_full_con_hijos_menores_3 = forcats::as_factor(ina52_5),

      ## Una mujer puede separarse o divorciarse teniendo hijos menores de 12 años
      mujer_divorciarse_con_hijos_menores_12 = forcats::as_factor(ina52_6),

      ## El cuidado de los hijos debe ser tarea principalmente de la mujer
      cuidado_hijos_mujer_ppal = forcats::as_factor(ina52_7),

      ## Una mujer se realiza plenamente cuando es madre
      mujer_se_realiza_cuando_es_madre = forcats::as_factor(ina52_8),

      ## Cuando la mujer tiene un trabajo de jornada completa la vida familiar se perjudica
      mujer_trabaja_full_perjudica_flia = forcats::as_factor(ina52_9),

      ## Un hombre se realiza plenamente cuando es padre
      varon_se_realiza_cuando_es_padre = forcats::as_factor(ina52_10),


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
      edad_limit_inf_abandonar_estudios_varones,

      madre_antes_18,
      mujer_no_tener_hijos,
      mujer_vivir_en_pareja_sin_casarse,
      mujer_tener_hijos_con_concu,
      mujer_trabajar_full_con_hijos_menores_3,
      mujer_divorciarse_con_hijos_menores_12,
      cuidado_hijos_mujer_ppal,
      mujer_se_realiza_cuando_es_madre,
      mujer_trabaja_full_perjudica_flia,
      varon_se_realiza_cuando_es_padre

   )

readr::write_rds(x = encor, path = here::here("encore.rds"))

# para comparar -----------------------------------------------------------

## hace scatter plots donde el tamaño sea la cantidad de personas que contestaron a esa pregutna en esas cantidades

#===============#
#### THE END ####
#===============#