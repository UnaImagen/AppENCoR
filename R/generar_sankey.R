generar_sankey <- function(.data, var_1, var_2) {

   aux_data <- .data %>%
      dplyr::group_by(
         var_1 := !!rlang::sym(var_1),
         var_2 := !!rlang::sym(var_2)
      ) %>%
      dplyr::summarise(
         n = base::sum(peso, na.rm = TRUE),
         .groups = "drop_last"
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