# dataflows <- c("WEO", "FM", "BOP", "CPI", "FSIC", "LS", "EER", "IIP", "NSDP")
# 
# # 1. Creamos una lista vacía para guardar los resultados
# lista_dfs <- list()
# 
# # 2. Iteramos (nota los paréntesis en el for)
# for (dataflow in dataflows) {
#   # Imprimimos mensaje para saber por dónde va el proceso
#   message("Descargando: ", dataflow)
# 
#   # Hacemos la llamada a la API
#   temp_df <- imfapi::imf_get_codelists(dimension_ids = "INDICATOR", dataflow_id = dataflow)
# 
#   # IMPORTANTE: Es muy útil añadir una columna con el nombre del dataflow
#   # para saber a qué base de datos pertenece cada fila después de unirlos.
#   if (!is.null(temp_df)) {
#     temp_df$source_dataflow <- dataflow
# 
#     # Guardamos en la lista
#     lista_dfs[[dataflow]] <- temp_df
#   }
# }
# 
# # 3. Unimos todos los dataframes de la lista en uno solo
# fmi_df <- dplyr::bind_rows(lista_dfs) |>
#   dplyr::select(code, name, description, codelist_id, codelist_agency)
# 
# write.csv(fmi_df, "indicadores_fmi.csv", row.names = FALSE)
#
#
# wb_df <- wbstats::wb_indicators(lang = "es") |>
#   dplyr::filter(!is.na(indicator)) |> dplyr::select(-topics)
#
# write.csv(wb_df, "indicadores_wb.csv", row.names = FALSE)

# eurostat_df <-
#   eurostat::search_eurostat(pattern = "") |> 
#   dplyr::filter(
#     nchar(data.start) == 4,
#     nchar(data.end) == 4,
#     data.end >= lubridate::today() |> lubridate::year() - 1
#   ) |> 
#   dplyr::distinct(code, .keep_all = TRUE)
#   
# write.csv(eurostat_df, "indicadores_eurostat.csv", row.names = FALSE)
