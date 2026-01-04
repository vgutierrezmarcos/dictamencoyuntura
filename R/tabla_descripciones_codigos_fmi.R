dataflows <- c("WEO", "FM", "BOP", "CPI", "FSIC")

# 1. Creamos una lista vacía para guardar los resultados
lista_dfs <- list()

# 2. Iteramos (nota los paréntesis en el for)
for (dataflow in dataflows) {
  
  # Imprimimos mensaje para saber por dónde va el proceso
  message("Descargando: ", dataflow)
  
  # Hacemos la llamada a la API
  temp_df <- imfapi::imf_get_codelists(
    dimension_ids = "INDICATOR",
    dataflow_id = dataflow
  )
  
  # IMPORTANTE: Es muy útil añadir una columna con el nombre del dataflow
  # para saber a qué base de datos pertenece cada fila después de unirlos.
  if (!is.null(temp_df)) {
    temp_df$source_dataflow <- dataflow
    
    # Guardamos en la lista
    lista_dfs[[dataflow]] <- temp_df
  }
}

# 3. Unimos todos los dataframes de la lista en uno solo
final_df <- dplyr::bind_rows(lista_dfs)

