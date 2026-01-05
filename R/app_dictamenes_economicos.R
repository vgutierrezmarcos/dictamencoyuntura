# ============================================================================
# APLICACIÓN SHINY PARA DESCARGA DE DATOS MACROECONÓMICOS
# Preparación de Dictámenes Económicos
# ============================================================================
#
# FUENTES DE DATOS:
# 1. FMI (WEO, FM, BOP, FSI) - Usando paquete imfapi (API SDMX 3.0)
# 2. Eurostat
# 3. Banco Mundial (World Development Indicators)
# 4. OMC (Organización Mundial del Comercio)
# 5. BIS (Bank for International Settlements)
#
# Fecha: Enero 2026
# ============================================================================

#' Lanzar la aplicación de Dictámenes de Coyuntura Económica
#'
#' Esta función inicia la aplicación Shiny para la descarga y análisis
#' de datos macroeconómicos de múltiples fuentes internacionales.
#'
#' @param output_dir Directorio donde se guardarán los archivos exportados.
#'                   Por defecto "output" en el directorio de trabajo.
#' @param ... Argumentos adicionales pasados a shiny::runApp()
#'
#' @return Inicia la aplicación Shiny
#' @export
#'
#' @examples
#' \dontrun{
#' dictamencoyuntura_app()
#' dictamencoyuntura_app(output_dir = "mis_exportaciones")
#' }
dictamencoyuntura_app <- function(output_dir = "output", ...) {
  
  # ============================================================================
  # VERIFICACIÓN E INSTALACIÓN DE PAQUETES
  # ============================================================================
  
  paquetes_necesarios <- c(
    "shiny",
    "bslib",
    "dplyr",
    "tidyr",
    "purrr",
    "officer",
    "flextable",
    "openxlsx",
    "DT",
    "lubridate",
    "stringr",
    "httr",
    "readxl",
    "countrycode",
    "wbstats",
    "imfr",
    "wtor",
    "eurostat",
    "BIS",
    "imfapi",
    "shinyjs",
    "OECD",
    "rdbnomics"
  )
  
  paquetes_faltantes <- paquetes_necesarios[!(paquetes_necesarios %in% installed.packages()[, "Package"])]
  if (length(paquetes_faltantes) > 0) {
    message("Instalando paquetes necesarios: ", paste(paquetes_faltantes, collapse = ", "))
    install.packages(paquetes_faltantes, repos = "http://cran.rstudio.com/")
  }
  
  # Cargar librerías
  suppressPackageStartupMessages({
    library(shiny)
    library(bslib)
    library(dplyr)
    library(tidyr)
    library(officer)
    library(flextable)
    library(openxlsx)
    library(DT)
    library(lubridate)
    library(stringr)
    library(httr)
    library(readxl)
    library(shinyjs)
  })
  
  # Crear directorio de salida si no existe
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    message("Directorio de salida creado: ", output_dir)
  }
  
  # ============================================================================
  # TEMA PERSONALIZADO - COLORES PASTEL
  # ============================================================================
  
  tema_pastel <- bs_theme(
    version = 5,
    bootswatch = "flatly",
    bg = "#E2EFD9",
    fg = "#2c3e50",
    primary = "#5F2987",
    secondary = "#95a5a6",
    success = "#217346",
    info = "#87ceeb",
    warning = "#2B579A",
    danger = "#e8a0a0",
    base_font = font_google("Source Sans Pro"),
    heading_font = font_google("Source Sans Pro"),
    font_scale = 0.95,
    `enable-rounded` = TRUE
  ) |>
    bs_add_rules("
      body, .form-control, .btn, .card, .nav-link, h1, h2, h3, h4, h5, h6, p, span, div, label {
        font-family: 'Segoe UI', 'Source Sans Pro', -apple-system, BlinkMacSystemFont, sans-serif !important;
      }

      .navbar {
        background: linear-gradient(135deg, #5F2987 0%, #7B3BA3 100%) !important;
        box-shadow: 0 2px 10px rgba(0,0,0,0.1);
      }

      .navbar-nav .nav-link {
        color: rgba(255, 255, 255, 0.8) !important;
        font-weight: 500;
        transition: all 0.2s ease;
      }

      .navbar-nav .nav-link:hover {
        color: #ffffff !important;
      }

      .navbar-nav .nav-link.active {
        color: #ffffff !important;
        font-weight: 600;
        background-color: rgba(255, 255, 255, 0.15) !important;
        border-radius: 6px;
      }

      .navbar-brand {
        font-weight: 700 !important;
        font-size: 1.3rem !important;
      }

      .card {
        border: none;
        border-radius: 12px;
        box-shadow: 0 4px 15px rgba(0,0,0,0.08);
        transition: transform 0.2s ease, box-shadow 0.2s ease;
        background-color: #ffffff;
      }

      .card:hover {
        transform: translateY(-2px);
        box-shadow: 0 6px 20px rgba(0,0,0,0.12);
      }

      .card-header {
        background: linear-gradient(135deg, #5F2987 0%, #7B3BA3 100%);
        color: white;
        border-radius: 12px 12px 0 0 !important;
        font-weight: 600;
        padding: 1rem 1.25rem;
      }

      .btn-primary {
        background: linear-gradient(135deg, #5F2987 0%, #7B3BA3 100%);
        border: none;
        border-radius: 8px;
        font-weight: 600;
        padding: 0.6rem 1.5rem;
        transition: all 0.3s ease;
      }

      .btn-primary:hover {
        background: linear-gradient(135deg, #7B3BA3 0%, #8E4DB6 100%);
        transform: translateY(-2px);
        box-shadow: 0 4px 12px rgba(95, 41, 135, 0.4);
      }

      .btn-success {
        background: linear-gradient(135deg, #217346 0%, #1D6B3F 100%);
        border: none;
        color: white;
        border-radius: 8px;
        font-weight: 600;
      }

      .btn-success:hover {
        background: linear-gradient(135deg, #1D6B3F 0%, #185C36 100%);
        color: white;
        transform: translateY(-2px);
      }

      .btn-warning {
        background: linear-gradient(135deg, #2B579A 0%, #1E4175 100%);
        border: none;
        color: white;
        border-radius: 8px;
        font-weight: 600;
      }

      .btn-warning:hover {
        background: linear-gradient(135deg, #1E4175 0%, #163358 100%);
        color: white;
        transform: translateY(-2px);
      }

      .btn-info {
        background: linear-gradient(135deg, #87ceeb 0%, #6bb9d9 100%);
        border: none;
        color: #1a4a5a;
        border-radius: 8px;
        font-weight: 600;
      }

      .btn-secondary {
        background: linear-gradient(135deg, #95a5a6 0%, #7f8c8d 100%);
        border: none;
        border-radius: 8px;
        font-weight: 600;
      }

      .form-control, .form-select {
        border: 2px solid #c5d9c0;
        border-radius: 8px;
        padding: 0.6rem 1rem;
        transition: border-color 0.2s ease, box-shadow 0.2s ease;
        background-color: #ffffff;
      }

      .form-control:focus, .form-select:focus {
        border-color: #5F2987;
        box-shadow: 0 0 0 0.2rem rgba(95, 41, 135, 0.25);
      }

      .nav-tabs .nav-link {
        border-radius: 8px 8px 0 0;
        font-weight: 500;
        color: #5F2987;
        transition: all 0.2s ease;
      }

      .nav-tabs .nav-link.active {
        background-color: #5F2987;
        color: #ffffff;
        border-color: #5F2987;
      }

      .nav-tabs .nav-link:hover:not(.active) {
        background-color: #f0e6f5;
        color: #7B3BA3;
      }

      .dataTables_wrapper {
        font-family: 'Segoe UI', sans-serif !important;
        background-color: #ffffff !important;
      }

      .dataTables_wrapper .dataTable {
        background-color: #ffffff !important;
      }

      .dataTables_wrapper .dataTable thead th {
        background-color: #E2EFD9 !important;
        color: #2c3e50 !important;
        font-weight: 600;
      }

      .dataTables_wrapper .dataTable tbody tr {
        background-color: #ffffff !important;
      }

      .dataTables_wrapper .dataTable tbody tr td {
        background-color: #ffffff !important;
      }

      .dataTables_wrapper .dataTable tbody tr:hover td {
        background-color: #f8f9fa !important;
      }

      .dataTables_wrapper .dataTable tbody tr.odd {
        background-color: #ffffff !important;
      }

      .dataTables_wrapper .dataTable tbody tr.even {
        background-color: #ffffff !important;
      }

      .dataTables_wrapper .dataTable tbody tr.odd td,
      .dataTables_wrapper .dataTable tbody tr.even td {
        background-color: #ffffff !important;
      }

      table.dataTable tbody tr {
        background-color: #ffffff !important;
      }

      .accordion {
        border-radius: 12px;
        overflow: hidden;
      }

      .accordion-button {
        background: linear-gradient(135deg, #5F2987 0%, #7B3BA3 100%);
        color: white;
        font-weight: 600;
        padding: 1rem 1.25rem;
      }

      .accordion-button:not(.collapsed) {
        background: linear-gradient(135deg, #7B3BA3 0%, #5F2987 100%);
        color: white;
      }

      .accordion-button:focus {
        box-shadow: none;
        border-color: transparent;
      }

      .accordion-button::after {
        filter: brightness(0) invert(1);
      }

      .accordion-body {
        padding: 1.5rem;
        background-color: #ffffff;
      }

      .hero-section {
        background: linear-gradient(135deg, #5F2987 0%, #7B3BA3 50%, #9B5DC4 100%);
        border-radius: 15px;
        padding: 2rem;
        margin-bottom: 1.5rem;
        color: white;
      }

      .hero-section h2 {
        margin-bottom: 0.5rem;
        font-weight: 700;
      }

      .hero-section p {
        opacity: 0.9;
        margin-bottom: 0;
      }

      .checkbox-group label {
        font-weight: 500;
        color: #4a6a7a;
      }

      .progress {
        height: 8px;
        border-radius: 4px;
        background-color: #d9e6d5;
      }

      .progress-bar {
        background: linear-gradient(90deg, #5F2987 0%, #7B3BA3 100%);
        border-radius: 4px;
      }

      .eurostat-disabled {
        opacity: 0.5;
        text-decoration: line-through;
      }

      .fuente-badge {
        display: inline-block;
        padding: 2px 8px;
        border-radius: 12px;
        font-size: 0.75rem;
        font-weight: 600;
        margin-left: 5px;
      }

      .fuente-badge-bm { background-color: #87ceeb; color: #1a4a5a; }
      .fuente-badge-fmi { background-color: #f4c7ab; color: #5a3825; }
      .fuente-badge-omc { background-color: #a8d5ba; color: #2d5a3d; }
      .fuente-badge-bis { background-color: #d4a5c9; color: #4a2d45; }
      .fuente-badge-fred { background-color: #c9d4a5; color: #3d4a2d; }
      .fuente-badge-eurostat { background-color: #a5c9d4; color: #2d3d4a; }

      .resumen-fuentes {
        background-color: #ffffff;
        border-radius: 8px;
        padding: 1rem;
        margin-top: 1rem;
        border: 1px solid #c5d9c0;
      }

      .alert-eurostat {
        background-color: #fff3cd;
        border-color: #ffc107;
        color: #856404;
        border-radius: 8px;
        padding: 0.5rem 1rem;
        font-size: 0.85rem;
        margin-top: 0.5rem;
      }
    ")
  
  # ============================================================================
  # CARGA DE INDICADORES DESDE EXCEL
  # ============================================================================
  
  # Variable global para almacenar los indicadores del Excel
  indicadores_excel_cache <- NULL
  
  # Función para cargar indicadores desde el Excel
  cargar_indicadores_excel <- function(ruta_excel = NULL) {
    # Si ya está en caché, devolver
    if (!is.null(indicadores_excel_cache)) {
      return(indicadores_excel_cache)
    }
    
    # Buscar el archivo Excel en varias ubicaciones posibles
    rutas_posibles <- c(
      ruta_excel,
      "Indicadores_Dictamen_Economico.xlsx",
      "data/Indicadores_Dictamen_Economico.xlsx",
      "inst/extdata/Indicadores_Dictamen_Economico.xlsx",
      system.file("extdata", "Indicadores_Dictamen_Economico.xlsx", package = "dictamencoyuntura"),
      file.path(getwd(), "Indicadores_Dictamen_Economico.xlsx")
    )
    
    ruta_encontrada <- NULL
    for (ruta in rutas_posibles) {
      if (!is.null(ruta) && file.exists(ruta)) {
        ruta_encontrada <- ruta
        break
      }
    }
    
    if (is.null(ruta_encontrada)) {
      message("⚠ No se encontró el archivo Indicadores_Dictamen_Economico.xlsx")
      message("  Usando indicadores por defecto...")
      return(NULL)
    }
    
    message("📊 Cargando indicadores desde: ", ruta_encontrada)
    
    tryCatch({
      df <- readxl::read_excel(ruta_encontrada, sheet = 1)
      
      # Verificar si existen columnas opcionales
      tiene_escala <- "Escala" %in% names(df)
      tiene_decimales <- "Decimales" %in% names(df)
      tiene_nivel <- "Nivel" %in% names(df)
      
      # Crear el dataframe preservando el orden del Excel y TODAS las columnas originales
      indicadores_excel_cache <<- df |>
        dplyr::mutate(
          orden_excel = dplyr::row_number(),  # Preservar orden original del Excel
          # Columnas normalizadas para uso interno (con nombres sin tildes)
          Seccion = as.character(.data[["Sección"]]),
          Subcategoria = as.character(.data[["Subcategoría"]]),
          Codigo = as.character(.data[["Código"]]),
          Nombre_ES = as.character(.data[["Nombre corto"]]),  # Usar siempre este nombre
          Nombre_EN = as.character(.data[["Nombre base de datos"]]),
          Descripcion = as.character(.data[["Descripción"]]),
          Unidad = as.character(.data[["Unidad"]]),
          Fuente_original = as.character(.data[["Fuente"]]),  # Preservar fuente original
          Base_datos = as.character(.data[["Base de datos"]]),
          # Escala: si existe usarla, si no default "x1"
          Escala = if (tiene_escala) as.character(.data[["Escala"]]) else "x1",
          # Decimales: si existe usarla, si no default 1
          Decimales = if (tiene_decimales) as.integer(.data[["Decimales"]]) else 1L,
          # Nivel: si existe usarla, si no default 1 (sin sangría)
          Nivel = if (tiene_nivel) as.integer(.data[["Nivel"]]) else 1L,
          # Normalizar la columna Fuente para filtrado interno
          # FMI, FMI (WEO), FMI (BOP) -> FMI con la base de datos correspondiente
          Fuente = dplyr::case_when(
            grepl("^FMI", Fuente_original, ignore.case = TRUE) ~ "FMI",
            Fuente_original %in% c("BM", "Banco Mundial") ~ "BM",
            TRUE ~ Fuente_original
          )
        ) |>
        dplyr::filter(!is.na(Codigo) & Codigo != "")  # Filtrar filas sin código
      
      # Limpiar columna Escala (valores vacíos o NA -> "x1")
      indicadores_excel_cache$Escala[is.na(indicadores_excel_cache$Escala) | 
                                       indicadores_excel_cache$Escala == ""] <- "x1"
      
      # Limpiar columna Decimales (valores NA -> 1)
      indicadores_excel_cache$Decimales[is.na(indicadores_excel_cache$Decimales)] <- 1L
      
      # Limpiar columna Nivel (valores NA -> 1)
      indicadores_excel_cache$Nivel[is.na(indicadores_excel_cache$Nivel)] <- 1L
      
      message("✓ Cargados ", nrow(indicadores_excel_cache), " indicadores del Excel")
      if (tiene_escala) {
        escalas_usadas <- unique(indicadores_excel_cache$Escala[indicadores_excel_cache$Escala != "x1"])
        if (length(escalas_usadas) > 0) {
          message("  📐 Escalas detectadas: ", paste(escalas_usadas, collapse = ", "))
        }
      }
      if (tiene_decimales) {
        decimales_usados <- sort(unique(indicadores_excel_cache$Decimales))
        message("  🔢 Decimales configurados: ", paste(decimales_usados, collapse = ", "))
      }
      if (tiene_nivel) {
        niveles_usados <- sort(unique(indicadores_excel_cache$Nivel))
        message("  📑 Niveles de sangría: ", paste(niveles_usados, collapse = ", "))
      }
      
      # Mostrar resumen por fuente y base de datos
      resumen <- indicadores_excel_cache |>
        dplyr::group_by(Fuente, Base_datos) |>
        dplyr::summarise(n = dplyr::n(), .groups = "drop")
      
      for (i in seq_len(nrow(resumen))) {
        message(sprintf("  - %s (%s): %d indicadores", 
                        resumen$Fuente[i], resumen$Base_datos[i], resumen$n[i]))
      }
      
      return(indicadores_excel_cache)
    }, error = function(e) {
      message("Error leyendo Excel: ", e$message)
      return(NULL)
    })
  }
  
  # Función helper para aplicar escalas a los valores
  # Parsea strings como "x1", "x1e-6", "x100", etc. y devuelve el multiplicador
  aplicar_escala <- function(datos, columna_escala = "escala", columna_valor = "valor") {
    if (!(columna_escala %in% names(datos))) {
      return(datos)
    }
    
    datos |>
      dplyr::mutate(
        .multiplicador = dplyr::case_when(
          is.na(.data[[columna_escala]]) | .data[[columna_escala]] == "" | .data[[columna_escala]] == "x1" ~ 1,
          .data[[columna_escala]] == "x1e-3" ~ 1e-3,
          .data[[columna_escala]] == "x1e-6" ~ 1e-6,
          .data[[columna_escala]] == "x1e-9" ~ 1e-9,
          .data[[columna_escala]] == "x1e3" ~ 1e3,
          .data[[columna_escala]] == "x1e6" ~ 1e6,
          .data[[columna_escala]] == "x100" ~ 100,
          .data[[columna_escala]] == "x0.01" | .data[[columna_escala]] == "x1e-2" ~ 0.01,
          .data[[columna_escala]] == "x0.001" ~ 0.001,
          # Intentar parsear escalas numéricas genéricas (ej: "x0.5", "x2")
          grepl("^x[0-9e.+-]+$", .data[[columna_escala]]) ~ 
            suppressWarnings(as.numeric(sub("^x", "", .data[[columna_escala]]))),
          TRUE ~ 1
        ),
        # Aplicar multiplicador solo si no es NA
        !!columna_valor := dplyr::if_else(
          is.na(.multiplicador),
          .data[[columna_valor]],
          .data[[columna_valor]] * .multiplicador
        )
      ) |>
      dplyr::select(-.multiplicador)
  }
  
  # Función para obtener indicadores por fuente y base de datos
  obtener_indicadores_por_fuente <- function(fuente_patron, base_datos = NULL) {
    indicadores <- cargar_indicadores_excel()
    if (is.null(indicadores)) return(NULL)
    
    # Filtrar por fuente usando patrón
    resultado <- indicadores |>
      dplyr::filter(grepl(fuente_patron, Fuente, ignore.case = TRUE))
    
    if (!is.null(base_datos)) {
      resultado <- resultado |>
        dplyr::filter(Base_datos == base_datos)
    }
    
    # Mantener orden del Excel
    resultado <- resultado |>
      dplyr::arrange(orden_excel)
    
    return(resultado)
  }
  
  # Función para obtener códigos de indicadores por fuente
  obtener_codigos_indicadores <- function(fuente_patron, base_datos = NULL) {
    indicadores <- obtener_indicadores_por_fuente(fuente_patron, base_datos)
    if (is.null(indicadores) || nrow(indicadores) == 0) return(character(0))
    
    codigos <- indicadores$Codigo
    codigos <- codigos[!is.na(codigos) & codigos != ""]
    
    return(unique(codigos))
  }
  
  # Función para obtener el mapeo de indicadores (código -> nombre, unidad, descripción)
  obtener_mapeo_indicadores <- function(fuente_patron = NULL, base_datos = NULL) {
    indicadores <- if (!is.null(fuente_patron)) {
      obtener_indicadores_por_fuente(fuente_patron, base_datos)
    } else {
      cargar_indicadores_excel()
    }
    
    if (is.null(indicadores) || nrow(indicadores) == 0) return(list())
    
    mapeo <- list()
    for (i in seq_len(nrow(indicadores))) {
      codigo <- indicadores$Codigo[i]
      if (!is.na(codigo) && codigo != "") {
        mapeo[[codigo]] <- list(
          nombre = ifelse(is.na(indicadores$Nombre_ES[i]), codigo, indicadores$Nombre_ES[i]),
          unidad = ifelse(is.na(indicadores$Unidad[i]), "", indicadores$Unidad[i]),
          descripcion = ifelse(is.na(indicadores$Descripcion[i]), "", indicadores$Descripcion[i]),
          seccion = ifelse(is.na(indicadores$Seccion[i]), "", indicadores$Seccion[i]),
          subcategoria = ifelse(is.na(indicadores$Subcategoria[i]), "", indicadores$Subcategoria[i]),
          orden = indicadores$orden_excel[i]
        )
      }
    }
    
    return(mapeo)
  }
  
  # Función para crear dataframe de mapeo desde el Excel
  crear_mapeo_df <- function(fuente_patron = NULL, base_datos = NULL) {
    indicadores <- if (!is.null(fuente_patron)) {
      obtener_indicadores_por_fuente(fuente_patron, base_datos)
    } else {
      cargar_indicadores_excel()
    }
    
    if (is.null(indicadores) || nrow(indicadores) == 0) return(NULL)
    
    # Verificar si existen columnas opcionales
    tiene_decimales <- "Decimales" %in% names(indicadores)
    tiene_nivel <- "Nivel" %in% names(indicadores)
    
    resultado <- indicadores |>
      dplyr::select(
        indicador_codigo = Codigo,
        indicador_nombre = Nombre_ES,
        unidad_corta = Unidad,
        seccion = Seccion,
        subcategoria = Subcategoria,
        orden_excel,
        dplyr::any_of(c("Decimales", "Nivel"))
      ) |>
      dplyr::mutate(
        indicador_nombre = ifelse(is.na(indicador_nombre), indicador_codigo, indicador_nombre),
        unidad_corta = ifelse(is.na(unidad_corta), "", unidad_corta),
        unidad_larga = unidad_corta  # Por simplicidad
      )
    
    # Añadir columna decimales normalizada (minúsculas)
    if (tiene_decimales) {
      resultado$decimales <- indicadores$Decimales
    } else {
      resultado$decimales <- 1L
    }
    
    # Añadir columna nivel normalizada (minúsculas)
    if (tiene_nivel) {
      resultado$nivel <- indicadores$Nivel
    } else {
      resultado$nivel <- 1L
    }
    
    # Eliminar columnas mayúsculas si existen (para evitar duplicados)
    if ("Decimales" %in% names(resultado)) {
      resultado <- resultado |> dplyr::select(-Decimales)
    }
    if ("Nivel" %in% names(resultado)) {
      resultado <- resultado |> dplyr::select(-Nivel)
    }
    
    return(resultado)
  }
  
  # ============================================================================
  # INDICADORES DEL BANCO MUNDIAL (cargados desde Excel)
  # ============================================================================
  
  # Variable para compatibilidad - se inicializará desde el Excel
  indicadores_banco_mundial <- NULL
  
  # Función para obtener indicadores del Banco Mundial desde el Excel
  obtener_indicadores_bm <- function() {
    # Obtener indicadores BM del Excel (ya normalizado a "BM")
    indicadores <- cargar_indicadores_excel()
    if (is.null(indicadores)) {
      return(list())
    }
    
    # Filtrar indicadores del Banco Mundial (Fuente normalizada = "BM", base WDI)
    bm_indicadores <- indicadores |>
      dplyr::filter(Fuente == "BM" & Base_datos == "WDI") |>
      dplyr::arrange(orden_excel)
    
    if (nrow(bm_indicadores) == 0) return(list())
    
    return(bm_indicadores)
  }
  
  # ============================================================================
  # INDICADORES DEL FMI (cargados desde Excel)
  # ============================================================================
  
  indicadores_fmi <- NULL
  
  # Función para obtener indicadores del FMI WEO desde el Excel
  obtener_indicadores_fmi_weo <- function() {
    indicadores <- cargar_indicadores_excel()
    if (is.null(indicadores)) return(NULL)
    
    # Filtrar indicadores FMI con base WEO (Fuente ya normalizada a "FMI")
    fmi_indicadores <- indicadores |>
      dplyr::filter(Fuente == "FMI" & Base_datos == "WEO") |>
      dplyr::arrange(orden_excel)
    
    return(fmi_indicadores)
  }
  
  # Función para obtener indicadores del FMI BOP desde el Excel
  obtener_indicadores_fmi_bop <- function() {
    indicadores <- cargar_indicadores_excel()
    if (is.null(indicadores)) return(NULL)
    
    # Filtrar indicadores FMI con base BOP
    fmi_indicadores <- indicadores |>
      dplyr::filter(Fuente == "FMI" & Base_datos == "BOP") |>
      dplyr::arrange(orden_excel)
    
    return(fmi_indicadores)
  }
  
  # Función para obtener indicadores del FMI FM (Fiscal Monitor) desde el Excel
  obtener_indicadores_fmi_fm <- function() {
    indicadores <- cargar_indicadores_excel()
    if (is.null(indicadores)) return(NULL)
    
    # Filtrar indicadores FMI con base FM
    fmi_indicadores <- indicadores |>
      dplyr::filter(Fuente == "FMI" & Base_datos == "FM") |>
      dplyr::arrange(orden_excel)
    
    return(fmi_indicadores)
  }
  
  # Función para obtener indicadores del FMI FSI desde el Excel
  obtener_indicadores_fmi_fsi <- function() {
    indicadores <- cargar_indicadores_excel()
    if (is.null(indicadores)) return(NULL)
    
    # Filtrar indicadores FMI con base FSI
    fmi_indicadores <- indicadores |>
      dplyr::filter(Fuente == "FMI" & Base_datos == "FSI") |>
      dplyr::arrange(orden_excel)
    
    return(fmi_indicadores)
  }
  
  # ============================================================================
  # INDICADORES ADICIONALES DEL FMI (funciones de acceso por base de datos)
  # ============================================================================
  
  indicadores_fmi_sdmx <- NULL
  
  # Función para obtener indicadores de Eurostat desde el Excel
  obtener_indicadores_eurostat <- function() {
    indicadores <- cargar_indicadores_excel()
    if (is.null(indicadores)) return(NULL)
    
    eurostat_indicadores <- indicadores |>
      dplyr::filter(Fuente == "Eurostat") |>
      dplyr::arrange(orden_excel)
    
    return(eurostat_indicadores)
  }
  
  # Función para obtener indicadores del BIS desde el Excel
  obtener_indicadores_bis <- function() {
    indicadores <- cargar_indicadores_excel()
    if (is.null(indicadores)) return(NULL)
    
    bis_indicadores <- indicadores |>
      dplyr::filter(Fuente == "BIS") |>
      dplyr::arrange(orden_excel)
    
    return(bis_indicadores)
  }
  
  # Función para obtener indicadores de la OMC desde el Excel
  obtener_indicadores_omc <- function() {
    indicadores <- cargar_indicadores_excel()
    if (is.null(indicadores)) return(NULL)
    
    omc_indicadores <- indicadores |>
      dplyr::filter(Fuente == "OMC") |>
      dplyr::arrange(orden_excel)
    
    return(omc_indicadores)
  }
  
  # Mapeo ISO2 a ISO3
  mapeo_iso2_iso3 <- c(
    "ES" = "ESP", "DE" = "DEU", "FR" = "FRA", "IT" = "ITA", "GB" = "GBR",
    "US" = "USA", "JP" = "JPN", "CN" = "CHN", "BR" = "BRA", "MX" = "MEX",
    "AR" = "ARG", "CL" = "CHL", "CO" = "COL", "PE" = "PER", "VE" = "VEN",
    "EC" = "ECU", "BO" = "BOL", "PY" = "PRY", "UY" = "URY", "CR" = "CRI",
    "PA" = "PAN", "GT" = "GTM", "HN" = "HND", "NI" = "NIC", "SV" = "SLV",
    "DO" = "DOM", "CU" = "CUB", "PR" = "PRI", "PT" = "PRT", "GR" = "GRC",
    "NL" = "NLD", "BE" = "BEL", "AT" = "AUT", "CH" = "CHE", "SE" = "SWE",
    "NO" = "NOR", "DK" = "DNK", "FI" = "FIN", "IE" = "IRL", "PL" = "POL",
    "CZ" = "CZE", "HU" = "HUN", "RO" = "ROU", "BG" = "BGR", "HR" = "HRV",
    "SK" = "SVK", "SI" = "SVN", "EE" = "EST", "LV" = "LVA", "LT" = "LTU",
    "RU" = "RUS", "UA" = "UKR", "BY" = "BLR", "KZ" = "KAZ", "UZ" = "UZB",
    "IN" = "IND", "ID" = "IDN", "TH" = "THA", "VN" = "VNM", "MY" = "MYS",
    "PH" = "PHL", "SG" = "SGP", "KR" = "KOR", "TW" = "TWN", "HK" = "HKG",
    "AU" = "AUS", "NZ" = "NZL", "ZA" = "ZAF", "EG" = "EGY", "NG" = "NGA",
    "KE" = "KEN", "MA" = "MAR", "DZ" = "DZA", "TN" = "TUN", "SA" = "SAU",
    "AE" = "ARE", "IL" = "ISR", "TR" = "TUR", "IR" = "IRN", "IQ" = "IRQ",
    "PK" = "PAK", "BD" = "BGD", "LK" = "LKA", "MM" = "MMR", "KH" = "KHM"
  )
  
  # Lista de países de la Unión Europea (códigos ISO2)
  paises_ue <- c(
    "AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR",
    "DE", "GR", "HU", "IE", "IT", "LV", "LT", "LU", "MT", "NL",
    "PL", "PT", "RO", "SK", "SI", "ES", "SE"
  )
  
  # ============================================================================
  # FUNCIONES AUXILIARES
  # ============================================================================
  
  obtener_lista_paises <- function() {
    paises <- wbstats::wb_countries(lang = "es")
    
    paises |>
      dplyr::filter(region != "Agregados") |>
      dplyr::select(iso2c, iso3c, country) |>
      dplyr::mutate(
        country_es = country
      ) |>
      dplyr::select(iso2c, iso3c, country_es) |>
      dplyr::arrange(country_es)
  }
  
  # Función para formatear números con formato español
  formatear_numero_es <- function(x, decimales = 2) {
    if (is.na(x)) {
      return(" ")
    } else {
      format(round(x, decimales), nsmall = decimales, big.mark = ".", decimal.mark = ",", 
             trim = TRUE, scientific = FALSE)
    }
  }
  
  # ============================================================================
  # FUNCIÓN - Obtener codelists del FMI (nombres en inglés, descripciones)
  # ============================================================================
  
  # Variable global para almacenar los codelists (se carga una vez)
  codelists_fmi_cache <- NULL
  
  obtener_codelists_fmi <- function(forzar_recarga = FALSE) {
    # Si ya está en caché y no se fuerza recarga, devolver caché
    if (!is.null(codelists_fmi_cache) && !forzar_recarga) {
      return(codelists_fmi_cache)
    }
    
    # Verificar que imfapi está disponible
    if (!requireNamespace("imfapi", quietly = TRUE)) {
      message("Paquete imfapi no disponible para obtener codelists")
      return(NULL)
    }
    
    dataflows <- c("WEO", "FM", "BOP", "CPI", "FSIC")
    lista_dfs <- list()
    
    for (dataflow in dataflows) {
      tryCatch({
        message("Descargando codelist: ", dataflow)
        
        temp_df <- imfapi::imf_get_codelists(
          dimension_ids = "INDICATOR",
          dataflow_id = dataflow
        )
        
        if (!is.null(temp_df) && nrow(temp_df) > 0) {
          # Estandarizar nombres de columnas
          # imfapi devuelve: code, name, description (pueden variar)
          nombres_cols <- tolower(names(temp_df))
          names(temp_df) <- nombres_cols
          
          # Asegurar que existen las columnas necesarias
          if (!"code" %in% names(temp_df)) {
            if ("id" %in% names(temp_df)) names(temp_df)[names(temp_df) == "id"] <- "code"
          }
          if (!"name" %in% names(temp_df)) {
            if ("label" %in% names(temp_df)) names(temp_df)[names(temp_df) == "label"] <- "name"
          }
          if (!"description" %in% names(temp_df)) {
            temp_df$description <- NA_character_
          }
          
          temp_df$source_dataflow <- dataflow
          lista_dfs[[dataflow]] <- temp_df
        }
      }, error = function(e) {
        message("Error descargando codelist ", dataflow, ": ", e$message)
      })
    }
    
    if (length(lista_dfs) == 0) {
      return(NULL)
    }
    
    # Combinar todos los dataframes
    resultado <- dplyr::bind_rows(lista_dfs)
    
    # Seleccionar y renombrar columnas de interés
    cols_disponibles <- names(resultado)
    cols_seleccionar <- intersect(c("code", "name", "description", "source_dataflow"), cols_disponibles)
    
    resultado <- resultado |>
      dplyr::select(dplyr::all_of(cols_seleccionar)) |>
      dplyr::rename(
        indicador_codigo = code,
        name_en = name,
        description_en = description,
        source_db = source_dataflow
      ) |>
      dplyr::distinct(indicador_codigo, .keep_all = TRUE)  # Eliminar duplicados, mantener primera aparición
    
    # Guardar en caché
    codelists_fmi_cache <<- resultado
    
    message("✓ Codelists FMI cargados: ", nrow(resultado), " indicadores")
    return(resultado)
  }
  
  # Función para enriquecer datos con nombres en inglés
  enriquecer_con_nombres_ingles <- function(datos, codelists = NULL) {
    if (is.null(datos) || nrow(datos) == 0) return(datos)
    
    # Obtener codelists si no se proporcionan
    if (is.null(codelists)) {
      codelists <- obtener_codelists_fmi()
    }
    
    if (is.null(codelists) || nrow(codelists) == 0) {
      # Si no hay codelists, añadir columnas vacías
      datos$name_en <- NA_character_
      datos$description_en <- NA_character_
      datos$source_db <- NA_character_
      return(datos)
    }
    
    # Hacer join con los codelists
    datos <- datos |>
      dplyr::left_join(
        codelists |> dplyr::select(indicador_codigo, name_en, description_en, source_db),
        by = "indicador_codigo"
      )
    
    # Si el nombre en español está vacío o es igual al código, usar el nombre en inglés
    datos <- datos |>
      dplyr::mutate(
        indicador_nombre = dplyr::case_when(
          # Si el nombre es NA, vacío o igual al código, usar nombre en inglés
          is.na(indicador_nombre) | indicador_nombre == "" | indicador_nombre == indicador_codigo ~ 
            dplyr::coalesce(name_en, indicador_codigo),
          # Si no, mantener el nombre en español
          TRUE ~ indicador_nombre
        )
      )
    
    return(datos)
  }
  
  # ============================================================================
  # FUNCIÓN - Descarga Banco Mundial con reintentos
  # Descarga indicador por indicador para manejar errores individuales
  # ============================================================================
  
  descargar_datos_bm <- function(pais_codigo, fecha_inicio, fecha_fin, 
                                 max_reintentos = 2, usar_cache = TRUE) {
    
    # 1. Obtener indicadores del Banco Mundial desde el Excel
    bm_indicadores <- obtener_indicadores_bm()
    
    if (is.null(bm_indicadores) || nrow(bm_indicadores) == 0) {
      message("No se encontraron indicadores del Banco Mundial en el Excel")
      return(NULL)
    }
    
    # Crear mapeo desde el Excel
    mapeo_df <- bm_indicadores |>
      dplyr::select(
        indicador_codigo = Codigo,
        indicador_nombre = Nombre_ES,
        unidad_corta = Unidad,
        seccion = Seccion,
        subcategoria = Subcategoria,
        orden_excel
      ) |>
      dplyr::mutate(
        indicador_nombre = ifelse(is.na(indicador_nombre), indicador_codigo, indicador_nombre),
        unidad_corta = ifelse(is.na(unidad_corta), "", unidad_corta),
        unidad_larga = unidad_corta
      )
    
    # Añadir escala si existe en el Excel
    if ("Escala" %in% names(bm_indicadores)) {
      mapeo_df$escala <- bm_indicadores$Escala
    } else {
      mapeo_df$escala <- "x1"
    }
    
    # Añadir decimales si existe en el Excel
    if ("Decimales" %in% names(bm_indicadores)) {
      mapeo_df$decimales <- bm_indicadores$Decimales
    } else {
      mapeo_df$decimales <- 1L
    }
    
    codigos_indicadores <- mapeo_df$indicador_codigo
    n_indicadores <- length(codigos_indicadores)
    message(paste0("Descargando ", n_indicadores, " indicadores del Banco Mundial..."))
    
    # 2. Intentar primero descarga en lote (más rápido)
    datos_lote <- NULL
    descarga_lote_exitosa <- FALSE
    
    tryCatch({
      message("Intentando descarga en lote...")
      datos_lote <- wbstats::wb_data(
        country = pais_codigo,
        indicator = codigos_indicadores,
        start_date = lubridate::year(fecha_inicio),
        end_date = lubridate::year(fecha_fin),
        return_wide = FALSE
      )
      
      if (!is.null(datos_lote) && nrow(datos_lote) > 0) {
        message(paste0("✓ Descarga en lote exitosa: ", nrow(datos_lote), " registros"))
        descarga_lote_exitosa <- TRUE
      }
    }, error = function(e) {
      message("Descarga en lote falló: ", conditionMessage(e))
    })
    
    # 3. Si la descarga en lote falló, descargar uno por uno
    if (!descarga_lote_exitosa) {
      message("Descargando indicadores individualmente...")
      
      lista_resultados <- list()
      indicadores_fallidos <- c()
      indicadores_exitosos <- 0
      
      for (i in seq_along(codigos_indicadores)) {
        codigo <- codigos_indicadores[i]
        
        # Mostrar progreso cada 20 indicadores
        if (i %% 20 == 0 || i == n_indicadores) {
          message(paste0("  Progreso: ", i, "/", n_indicadores, 
                         " (", round(i/n_indicadores*100), "%) - ",
                         indicadores_exitosos, " exitosos"))
        }
        
        exito <- FALSE
        for (intento in 1:max_reintentos) {
          tryCatch({
            datos_ind <- wbstats::wb_data(
              country = pais_codigo,
              indicator = codigo,
              start_date = lubridate::year(fecha_inicio),
              end_date = lubridate::year(fecha_fin),
              return_wide = FALSE
            )
            
            if (!is.null(datos_ind) && nrow(datos_ind) > 0) {
              lista_resultados[[length(lista_resultados) + 1]] <- datos_ind
              indicadores_exitosos <- indicadores_exitosos + 1
              exito <- TRUE
            }
            break  # Salir del bucle de reintentos si tuvo éxito o no hay datos
          }, error = function(e2) {
            if (intento < max_reintentos) {
              Sys.sleep(0.5)  # Esperar antes de reintentar
            }
          })
        }
        
        if (!exito) {
          indicadores_fallidos <- c(indicadores_fallidos, codigo)
        }
        
        # Pequeña pausa cada 10 indicadores para no sobrecargar la API
        if (i %% 10 == 0) Sys.sleep(0.3)
      }
      
      # Reportar indicadores fallidos
      if (length(indicadores_fallidos) > 0) {
        message(paste0("⚠ ", length(indicadores_fallidos), " indicadores no encontrados o con error"))
        # Mostrar primeros 10 indicadores fallidos
        if (length(indicadores_fallidos) <= 10) {
          message("  Códigos: ", paste(indicadores_fallidos, collapse = ", "))
        } else {
          message("  Códigos: ", paste(head(indicadores_fallidos, 10), collapse = ", "), "...")
        }
      }
      
      # Combinar resultados individuales
      if (length(lista_resultados) > 0) {
        datos_lote <- dplyr::bind_rows(lista_resultados)
        message(paste0("✓ Banco Mundial: ", indicadores_exitosos, " indicadores, ", 
                       nrow(datos_lote), " registros obtenidos"))
      }
    }
    
    # 4. Verificar que tenemos datos
    if (is.null(datos_lote) || nrow(datos_lote) == 0) {
      message("No se pudieron obtener datos del Banco Mundial.")
      return(NULL)
    }
    
    # 5. Join con mapeo del Excel
    resultado <- datos_lote |>
      dplyr::select(
        country, 
        year = date, 
        iso2c = iso2c,
        indicador_codigo = indicator_id,
        valor = value
      ) |>
      dplyr::left_join(mapeo_df, by = "indicador_codigo") |>
      dplyr::filter(!is.na(valor)) |>
      dplyr::mutate(
        year = as.integer(year),
        fuente = "Banco Mundial",
        prioridad_fuente = 3L
      )
    
    # 6. Aplicar escala si existe
    if ("escala" %in% names(resultado)) {
      resultado <- aplicar_escala(resultado, "escala", "valor") |>
        dplyr::select(-escala)
    }
    
    return(resultado)
  }
  
  # ============================================================================
  # FUNCIÓN FMI UNIFICADA - Descarga de todas las bases de datos disponibles
  # Usa imfapi para WEO, FM, BOP, CPI
  # ============================================================================
  # ============================================================================
  # FUNCIÓN FMI UNIFICADA - Descarga de todas las bases de datos disponibles
  # Usa imfapi para WEO, FM, BOP, FSI
  # Lee indicadores SOLO del Excel
  # ============================================================================
  descargar_datos_fmi <- function(pais_codigo_iso2, fecha_inicio, fecha_fin) {
    
    # -------------------------------------------------------------------------
    # 1. Verificar e instalar imfapi si es necesario
    # -------------------------------------------------------------------------
    if (!requireNamespace("imfapi", quietly = TRUE)) {
      message("Instalando paquete imfapi...")
      tryCatch({
        install.packages("imfapi", repos = "https://cran.rstudio.com/")
      }, error = function(e) {
        message("No se pudo instalar imfapi: ", e$message)
        return(NULL)
      })
    }
    
    suppressPackageStartupMessages({
      if (!require("imfapi", quietly = TRUE)) {
        return(NULL)
      }
    })
    
    # -------------------------------------------------------------------------
    # 2. Preparar códigos de país
    # -------------------------------------------------------------------------
    paises <- obtener_lista_paises()
    match_idx <- which(paises$iso2c == pais_codigo_iso2)
    
    if (length(match_idx) == 0) {
      pais_codigo_iso3 <- mapeo_iso2_iso3[pais_codigo_iso2]
      if (is.na(pais_codigo_iso3)) {
        return(NULL)
      }
    } else {
      pais_codigo_iso3 <- paises$iso3c[match_idx]
    }
    
    anio_inicio <- as.character(lubridate::year(fecha_inicio))
    anio_fin <- as.character(lubridate::year(fecha_fin))
    
    # -------------------------------------------------------------------------
    # 3. Cargar indicadores del Excel y crear mapeos
    # -------------------------------------------------------------------------
    indicadores_weo <- obtener_indicadores_fmi_weo()
    indicadores_bop <- obtener_indicadores_fmi_bop()
    indicadores_fm <- obtener_indicadores_fmi_fm()
    indicadores_fsi <- obtener_indicadores_fmi_fsi()
    
    # Crear mapeo completo desde Excel (incluyendo escala y decimales)
    crear_mapeo_desde_excel <- function(indicadores_df) {
      if (is.null(indicadores_df) || nrow(indicadores_df) == 0) return(list())
      
      mapeo <- list()
      for (i in seq_len(nrow(indicadores_df))) {
        codigo <- indicadores_df$Codigo[i]
        if (!is.na(codigo) && codigo != "") {
          # Obtener escala si existe
          escala_valor <- if ("Escala" %in% names(indicadores_df)) {
            indicadores_df$Escala[i]
          } else {
            "x1"
          }
          if (is.na(escala_valor) || escala_valor == "") escala_valor <- "x1"
          
          # Obtener decimales si existe
          decimales_valor <- if ("Decimales" %in% names(indicadores_df)) {
            indicadores_df$Decimales[i]
          } else {
            1L
          }
          if (is.na(decimales_valor)) decimales_valor <- 1L
          
          mapeo[[codigo]] <- list(
            nombre = ifelse(is.na(indicadores_df$Nombre_ES[i]), codigo, indicadores_df$Nombre_ES[i]),
            unidad = ifelse(is.na(indicadores_df$Unidad[i]), "", indicadores_df$Unidad[i]),
            seccion = ifelse(is.na(indicadores_df$Seccion[i]), "", indicadores_df$Seccion[i]),
            subcategoria = ifelse(is.na(indicadores_df$Subcategoria[i]), "", indicadores_df$Subcategoria[i]),
            orden = indicadores_df$orden_excel[i],
            escala = escala_valor,
            decimales = decimales_valor
          )
        }
      }
      return(mapeo)
    }
    
    mapeo_weo <- crear_mapeo_desde_excel(indicadores_weo)
    mapeo_bop <- crear_mapeo_desde_excel(indicadores_bop)
    mapeo_fm <- crear_mapeo_desde_excel(indicadores_fm)
    mapeo_fsi <- crear_mapeo_desde_excel(indicadores_fsi)
    
    datos_lista <- list()
    
    # -------------------------------------------------------------------------
    # 4. Función auxiliar para procesar datos descargados
    # -------------------------------------------------------------------------
    procesar_datos_fmi <- function(datos_raw, mapeo, fuente_nombre, col_indicator = "INDICATOR") {
      if (is.null(datos_raw) || nrow(datos_raw) == 0) return(NULL)
      
      # Detectar columna de indicador
      if (!col_indicator %in% names(datos_raw)) {
        posibles_cols <- c("INDICATOR", "CONCEPT", "FSI_INDICATOR", "CLASSIFICATION")
        col_indicator <- intersect(posibles_cols, names(datos_raw))[1]
        if (is.na(col_indicator)) return(NULL)
      }
      
      # Filtrar solo los indicadores que están en el mapeo (del Excel)
      codigos_excel <- names(mapeo)
      
      datos_filtrados <- datos_raw |>
        dplyr::filter(.data[[col_indicator]] %in% codigos_excel)
      
      if (nrow(datos_filtrados) == 0) return(NULL)
      
      # Procesar datos
      resultado <- datos_filtrados |>
        dplyr::mutate(
          year = as.integer(substr(TIME_PERIOD, 1, 4)),
          indicador_codigo = .data[[col_indicator]],
          valor = as.numeric(OBS_VALUE)
        ) |>
        dplyr::filter(
          year >= as.integer(anio_inicio) & year <= as.integer(anio_fin),
          !is.na(valor)
        ) |>
        dplyr::rowwise() |>
        dplyr::mutate(
          indicador_nombre = mapeo[[indicador_codigo]]$nombre,
          unidad_corta = mapeo[[indicador_codigo]]$unidad,
          unidad_larga = mapeo[[indicador_codigo]]$unidad,
          seccion = mapeo[[indicador_codigo]]$seccion,
          subcategoria = mapeo[[indicador_codigo]]$subcategoria,
          orden_excel = mapeo[[indicador_codigo]]$orden,
          escala = mapeo[[indicador_codigo]]$escala,
          decimales = mapeo[[indicador_codigo]]$decimales
        ) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          country = pais_codigo_iso3,
          iso2c = pais_codigo_iso2,
          fuente = fuente_nombre,
          prioridad_fuente = 1L
        )
      
      # Aplicar escala
      resultado <- aplicar_escala(resultado, "escala", "valor")
      
      resultado <- resultado |>
        dplyr::select(country, year, iso2c, indicador_codigo, indicador_nombre,
                      unidad_corta, unidad_larga, valor, fuente, prioridad_fuente,
                      seccion, subcategoria, orden_excel, decimales)
      
      return(resultado)
    }
    
    # -------------------------------------------------------------------------
    # 5. Descargar WEO (World Economic Outlook)
    # -------------------------------------------------------------------------
    if (length(mapeo_weo) > 0) {
      message("  Descargando FMI WEO (", length(mapeo_weo), " indicadores)...")
      tryCatch({
        datos_weo <- suppressWarnings({
          imfapi::imf_get(
            dataflow_id = "WEO",
            dimensions = list(COUNTRY = pais_codigo_iso3, FREQUENCY = "A"),
            start_period = anio_inicio,
            end_period = anio_fin,
            progress = FALSE,
            max_tries = 2
          )
        })
        
        datos_procesados <- procesar_datos_fmi(datos_weo, mapeo_weo, "FMI (WEO)")
        if (!is.null(datos_procesados) && nrow(datos_procesados) > 0) {
          datos_lista[["WEO"]] <- datos_procesados
          message(paste0("  ✓ FMI (WEO): ", nrow(datos_procesados), " registros"))
        }
      }, error = function(e) {
        message("  ⚠ Error WEO: ", e$message)
      })
    }
    
    # -------------------------------------------------------------------------
    # 6. Descargar FM (Fiscal Monitor)
    # -------------------------------------------------------------------------
    if (length(mapeo_fm) > 0) {
      message("  Descargando FMI FM (", length(mapeo_fm), " indicadores)...")
      tryCatch({
        datos_fm <- suppressWarnings({
          imfapi::imf_get(
            dataflow_id = "FM",
            dimensions = list(COUNTRY = pais_codigo_iso3, FREQUENCY = "A"),
            start_period = anio_inicio,
            end_period = anio_fin,
            progress = FALSE,
            max_tries = 2
          )
        })
        
        datos_procesados <- procesar_datos_fmi(datos_fm, mapeo_fm, "FMI (FM)")
        if (!is.null(datos_procesados) && nrow(datos_procesados) > 0) {
          datos_lista[["FM"]] <- datos_procesados
          message(paste0("  ✓ FMI (FM): ", nrow(datos_procesados), " registros"))
        }
      }, error = function(e) {
        message("  ⚠ Error FM: ", e$message)
      })
    }
    
    # -------------------------------------------------------------------------
    # 7. Descargar BOP (Balance of Payments)
    # -------------------------------------------------------------------------
    if (length(mapeo_bop) > 0) {
      message("  Descargando FMI BOP (", length(mapeo_bop), " indicadores)...")
      tryCatch({
        datos_bop <- suppressWarnings({
          imfapi::imf_get(
            dataflow_id = "BOP",
            dimensions = list(COUNTRY = pais_codigo_iso3, FREQUENCY = "A"),
            start_period = anio_inicio,
            end_period = anio_fin,
            progress = FALSE,
            max_tries = 2
          )
        })
        
        datos_procesados <- procesar_datos_fmi(datos_bop, mapeo_bop, "FMI (BOP)", "INDICATOR")
        if (!is.null(datos_procesados) && nrow(datos_procesados) > 0) {
          datos_lista[["BOP"]] <- datos_procesados
          message(paste0("  ✓ FMI (BOP): ", nrow(datos_procesados), " registros"))
        }
      }, error = function(e) {
        message("  ⚠ Error BOP: ", e$message)
      })
    }
    
    # -------------------------------------------------------------------------
    # 8. Descargar FSI (Financial Soundness Indicators) - dataflow_id = "FSIC"
    # -------------------------------------------------------------------------
    if (length(mapeo_fsi) > 0) {
      message("  Descargando FMI FSI (", length(mapeo_fsi), " indicadores)...")
      tryCatch({
        datos_fsi <- suppressWarnings({
          imfapi::imf_get(
            dataflow_id = "FSIC",  # IMPORTANTE: El dataflow_id correcto es "FSIC"
            dimensions = list(COUNTRY = pais_codigo_iso3, FREQUENCY = "A"),
            start_period = anio_inicio,
            end_period = anio_fin,
            progress = FALSE,
            max_tries = 2
          )
        })
        
        # FSI puede usar diferentes nombres de columna para el indicador
        col_indicator <- if ("FSI_INDICATOR" %in% names(datos_fsi)) "FSI_INDICATOR" else "INDICATOR"
        
        datos_procesados <- procesar_datos_fmi(datos_fsi, mapeo_fsi, "FMI (FSI)", col_indicator)
        if (!is.null(datos_procesados) && nrow(datos_procesados) > 0) {
          datos_lista[["FSI"]] <- datos_procesados
          message(paste0("  ✓ FMI (FSI): ", nrow(datos_procesados), " registros"))
        }
      }, error = function(e) {
        message("  ⚠ Error FSI: ", e$message)
      })
    }
    
    # -------------------------------------------------------------------------
    # 9. Consolidar resultados
    # -------------------------------------------------------------------------
    if (length(datos_lista) == 0) {
      return(NULL)
    }
    
    resultado <- dplyr::bind_rows(datos_lista)
    
    # Eliminar duplicados (mismo indicador + año), manteniendo WEO primero
    resultado <- resultado |>
      dplyr::mutate(
        orden_fuente = dplyr::case_when(
          grepl("WEO", fuente) ~ 1,
          grepl("FM", fuente) ~ 2,
          grepl("BOP", fuente) ~ 3,
          grepl("FSI", fuente) ~ 4,
          TRUE ~ 5
        )
      ) |>
      dplyr::arrange(indicador_codigo, year, orden_fuente) |>
      dplyr::group_by(indicador_codigo, year) |>
      dplyr::slice(1) |>
      dplyr::ungroup() |>
      dplyr::select(-orden_fuente)
    
    return(resultado)
  }
  
  # BIS - Tipo de cambio efectivo real y nominal
  descargar_datos_bis <- function(pais_codigo_iso2, fecha_inicio, fecha_fin) {
    tryCatch({
      # --- 1. VALIDACIONES Y PAQUETES ---
      if (!requireNamespace("BIS", quietly = TRUE)) return(NULL)
      if (!requireNamespace("dplyr", quietly = TRUE)) return(NULL)
      if (!requireNamespace("stringr", quietly = TRUE)) return(NULL)
      if (!requireNamespace("lubridate", quietly = TRUE)) return(NULL)
      
      # --- 2. GESTIÓN FLEXIBLE DE FECHAS (NUEVO) ---
      # Esta función auxiliar permite pasar tanto 2019 como "2019-01-01"
      obtener_anio <- function(input) {
        if (is.numeric(input)) {
          return(input) # Si pones 2019, devuelve 2019
        } else {
          # Si pones fecha, extrae el año. tryCatch por si acaso falla el formato
          tryCatch(lubridate::year(input), error = function(e) as.numeric(substring(as.character(input), 1, 4)))
        }
      }
      
      year_start_val <- obtener_anio(fecha_inicio)
      year_end_val   <- obtener_anio(fecha_fin)
      
      # --- 3. MAPEO DE PAÍSES ---
      mapeo_bis <- c(
        "AE" = "AE: United Arab Emirates", "AR" = "AR: Argentina", "AT" = "AT: Austria",
        "AU" = "AU: Australia", "BA" = "BA: Bosnia and Herzegovina", "BE" = "BE: Belgium",
        "BG" = "BG: Bulgaria", "BR" = "BR: Brazil", "CA" = "CA: Canada",
        "CH" = "CH: Switzerland", "CL" = "CL: Chile", "CN" = "CN: China",
        "CO" = "CO: Colombia", "CY" = "CY: Cyprus", "CZ" = "CZ: Czechia",
        "DE" = "DE: Germany", "DK" = "DK: Denmark", "DZ" = "DZ: Algeria",
        "EE" = "EE: Estonia", "ES" = "ES: Spain", "FI" = "FI: Finland",
        "FR" = "FR: France", "GB" = "GB: United Kingdom", "GR" = "GR: Greece",
        "HK" = "HK: Hong Kong SAR", "HR" = "HR: Croatia", "HU" = "HU: Hungary",
        "ID" = "ID: Indonesia", "IE" = "IE: Ireland", "IL" = "IL: Israel",
        "IN" = "IN: India", "IS" = "IS: Iceland", "IT" = "IT: Italy",
        "JP" = "JP: Japan", "KR" = "KR: Korea", "LT" = "LT: Lithuania",
        "LU" = "LU: Luxembourg", "LV" = "LV: Latvia", "MA" = "MA: Morocco",
        "MK" = "MK: North Macedonia", "MT" = "MT: Malta", "MX" = "MX: Mexico",
        "MY" = "MY: Malaysia", "NL" = "NL: Netherlands", "NO" = "NO: Norway",
        "NZ" = "NZ: New Zealand", "PE" = "PE: Peru", "PH" = "PH: Philippines",
        "PL" = "PL: Poland", "PT" = "PT: Portugal", "RO" = "RO: Romania",
        "RS" = "RS: Serbia", "RU" = "RU: Russia", "SA" = "SA: Saudi Arabia",
        "SE" = "SE: Sweden", "SG" = "SG: Singapore", "SI" = "SI: Slovenia",
        "SK" = "SK: Slovakia", "TH" = "TH: Thailand", "TR" = "TR: Türkiye",
        "TW" = "TW: Chinese Taipei", "US" = "US: United States", "ZA" = "ZA: South Africa"
      )
      
      area_bis <- mapeo_bis[pais_codigo_iso2]
      if (is.na(area_bis)) {
        message("País no disponible en BIS")
        return(NULL)
      }
      
      # --- 4. DESCARGA ROBUSTA (LIBURL) ---
      options(download.file.method = "libcurl")
      old_timeout <- getOption("timeout")
      options(timeout = 600) 
      
      url_bis <- "https://data.bis.org/static/bulk/WS_EER_csv_flat.zip"
      temp_zip <- tempfile(fileext = ".zip")
      
      message("Descargando datos del BIS...")
      descarga_ok <- tryCatch({
        download.file(url_bis, destfile = temp_zip, mode = "wb", quiet = TRUE)
        TRUE
      }, error = function(e) { FALSE })
      
      options(timeout = old_timeout)
      
      if (!descarga_ok) return(NULL)
      
      eer_df <- tryCatch({ BIS::read_bis(temp_zip) }, error = function(e) { NULL })
      unlink(temp_zip)
      
      if (is.null(eer_df) || nrow(eer_df) == 0) {
        message("Archivo vacío o error de lectura.")
        return(NULL)
      }
      
      # --- 5. PROCESAMIENTO - Usar indicadores del Excel ---
      bis_indicadores <- obtener_indicadores_bis()
      
      if (is.null(bis_indicadores) || nrow(bis_indicadores) == 0) {
        message("No hay indicadores BIS en el Excel")
        return(NULL)
      }
      
      datos_lista <- list()
      
      # Procesar cada indicador del Excel
      for (i in seq_len(nrow(bis_indicadores))) {
        codigo <- bis_indicadores$Codigo[i]
        nombre <- bis_indicadores$Nombre_ES[i]
        unidad <- bis_indicadores$Unidad[i]
        seccion <- bis_indicadores$Seccion[i]
        subcategoria <- bis_indicadores$Subcategoria[i]
        orden <- bis_indicadores$orden_excel[i]
        
        # Determinar tipo (R=Real, N=Nominal)
        tipo_letra <- if (grepl("REER|real", codigo, ignore.case = TRUE)) "R" else "N"
        
        tryCatch({
          datos_ind <- eer_df |>
            dplyr::filter(
              ref_area == area_bis,
              stringr::str_sub(eer_basket, 1, 1) == "B",
              stringr::str_sub(eer_type, 1, 1) == tipo_letra,
              !is.na(time_period)
            ) |>
            dplyr::mutate(
              year = as.integer(stringr::str_sub(time_period, 1, 4)),
              valor = as.numeric(obs_value)
            ) |>
            dplyr::filter(year >= year_start_val & year <= year_end_val) |>
            dplyr::group_by(year) |>
            dplyr::summarise(valor = mean(valor, na.rm = TRUE), .groups = "drop") |>
            dplyr::mutate(
              country = pais_codigo_iso2,
              iso2c = pais_codigo_iso2,
              indicador_codigo = codigo,
              indicador_nombre = ifelse(is.na(nombre), codigo, nombre),
              unidad_corta = ifelse(is.na(unidad), "índice", unidad),
              unidad_larga = ifelse(is.na(unidad), "índice", unidad),
              fuente = "BIS",
              prioridad_fuente = 5L,
              seccion = ifelse(is.na(seccion), "Sector exterior", seccion),
              subcategoria = ifelse(is.na(subcategoria), "", subcategoria),
              orden_excel = orden
            )
          
          if (nrow(datos_ind) > 0) {
            datos_lista[[codigo]] <- datos_ind
          }
        }, error = function(e) NULL)
      }
      
      if (length(datos_lista) == 0) return(NULL)
      
      resultado <- dplyr::bind_rows(datos_lista) |>
        dplyr::filter(!is.na(valor))
      
      return(resultado)
      
    }, error = function(e) {
      message("Error en la función: ", e$message)
      return(NULL)
    })
  }
  
  # Eurostat - Lee indicadores del Excel
  descargar_datos_eurostat <- function(pais_codigo_iso2, fecha_inicio, fecha_fin) {
    tryCatch({
      if (!requireNamespace("eurostat", quietly = TRUE)) {
        return(NULL)
      }
      
      if (!(pais_codigo_iso2 %in% paises_ue)) {
        message("   ℹ País no es miembro de la UE")
        return(NULL)
      }
      
      # Obtener indicadores de Eurostat desde el Excel
      eurostat_indicadores <- obtener_indicadores_eurostat()
      if (is.null(eurostat_indicadores) || nrow(eurostat_indicadores) == 0) {
        message("   ℹ No hay indicadores de Eurostat en el Excel")
        return(NULL)
      }
      
      datos_lista <- list()
      anio_inicio <- lubridate::year(fecha_inicio)
      anio_fin <- lubridate::year(fecha_fin)
      
      # Mapeo de datasets a configuraciones de filtrado
      config_eurostat <- list(
        "nama_10_gdp" = list(
          filtros = function(df) df |> dplyr::filter(na_item == "B1GQ", unit == "CLV_PCH_PRE"),
          col_valor = "values"
        ),
        "une_rt_a" = list(
          filtros = function(df) df |> dplyr::filter(sex == "T", age == "Y15-74", unit == "PC_ACT"),
          col_valor = "values"
        ),
        "nama_10_lp_ulc" = list(
          filtros = function(df) df |> dplyr::filter(na_item == "D1_SAL_HW", nace_r2 == "TOTAL", unit == "PCH_PRE"),
          col_valor = "values"
        ),
        "gov_10_dd_edpt1" = list(
          filtros = function(df) df |> dplyr::filter(na_item == "GD", sector == "S13", unit == "PC_GDP"),
          col_valor = "values"
        )
      )
      
      for (i in seq_len(nrow(eurostat_indicadores))) {
        dataset_id <- eurostat_indicadores$Base_datos[i]
        codigo <- eurostat_indicadores$Codigo[i]
        nombre <- eurostat_indicadores$Nombre_ES[i]
        unidad <- eurostat_indicadores$Unidad[i]
        seccion <- eurostat_indicadores$Seccion[i]
        subcategoria <- eurostat_indicadores$Subcategoria[i]
        orden <- eurostat_indicadores$orden_excel[i]
        decimales <- if ("Decimales" %in% names(eurostat_indicadores)) eurostat_indicadores$Decimales[i] else 1L
        
        tryCatch({
          datos <- eurostat::get_eurostat(
            dataset_id,
            time_format = "num",
            filters = list(geo = pais_codigo_iso2)
          )
          
          if (!is.null(datos) && nrow(datos) > 0) {
            # Aplicar filtros específicos del dataset si existen
            if (dataset_id %in% names(config_eurostat)) {
              datos <- config_eurostat[[dataset_id]]$filtros(datos)
            }
            
            # Filtrar por años
            datos <- datos |>
              dplyr::filter(time >= anio_inicio & time <= anio_fin)
            
            if (nrow(datos) > 0) {
              datos_procesados <- datos |>
                dplyr::mutate(
                  year = as.integer(time),
                  indicador_codigo = codigo,
                  indicador_nombre = nombre,
                  unidad_corta = unidad,
                  unidad_larga = unidad,
                  valor = as.numeric(values),
                  country = pais_codigo_iso2,
                  iso2c = pais_codigo_iso2,
                  fuente = "Eurostat",
                  prioridad_fuente = 2L,
                  seccion = seccion,
                  subcategoria = subcategoria,
                  orden_excel = orden,
                  decimales = decimales
                ) |>
                dplyr::filter(!is.na(valor)) |>
                dplyr::select(country, year, iso2c, indicador_codigo, indicador_nombre,
                              unidad_corta, unidad_larga, valor, fuente, prioridad_fuente,
                              seccion, subcategoria, orden_excel, decimales)
              
              if (nrow(datos_procesados) > 0) {
                datos_lista[[codigo]] <- datos_procesados
              }
            }
          }
        }, error = function(e) {
          message("   ⚠ Eurostat ", dataset_id, ": ", e$message)
        })
      }
      
      if (length(datos_lista) == 0) return(NULL)
      
      resultado <- dplyr::bind_rows(datos_lista)
      return(resultado)
      
    }, error = function(e) {
      message("Error descargando datos de Eurostat: ", e$message)
      return(NULL)
    })
  }
  
  # OMC - Organización Mundial del Comercio (lee del Excel)
  descargar_datos_omc <- function(pais_codigo_iso2, fecha_inicio, fecha_fin) {
    tryCatch({
      if (!requireNamespace("wtor", quietly = TRUE)) {
        return(NULL)
      }
      
      # Obtener indicadores de OMC desde el Excel
      omc_indicadores <- obtener_indicadores_omc()
      if (is.null(omc_indicadores) || nrow(omc_indicadores) == 0) {
        message("   ℹ No hay indicadores de OMC en el Excel")
        return(NULL)
      }
      
      paises <- obtener_lista_paises()
      match_idx <- which(paises$iso2c == pais_codigo_iso2)
      
      if (length(match_idx) == 0) {
        pais_codigo_iso3 <- mapeo_iso2_iso3[pais_codigo_iso2]
        if (is.na(pais_codigo_iso3)) {
          return(NULL)
        }
      } else {
        pais_codigo_iso3 <- paises$iso3c[match_idx]
      }
      
      # Países UE usan código 918
      codigos_ue <- c(
        "AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN",
        "ESP", "FRA", "DEU", "ITA", "GRC", "HUN", "SWE", "IRL", "LVA",
        "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN"
      )
      
      if (pais_codigo_iso3 %in% codigos_ue) {
        codigo_pais <- "918"
      } else {
        tryCatch({
          economies_wto <- wtor::get_reporting_economies()
          codigo_pais <- economies_wto |>
            dplyr::filter(iso3A == pais_codigo_iso3) |>
            dplyr::pull(code)
          
          if (length(codigo_pais) == 0) {
            message("   ℹ País no encontrado en OMC")
            return(NULL)
          }
        }, error = function(e) {
          message("   ⚠ Error obteniendo código OMC: ", e$message)
          return(NULL)
        })
      }
      
      datos_lista <- list()
      anio_inicio <- lubridate::year(fecha_inicio)
      anio_fin <- lubridate::year(fecha_fin)
      
      for (i in seq_len(nrow(omc_indicadores))) {
        codigo <- omc_indicadores$Codigo[i]
        nombre <- omc_indicadores$Nombre_ES[i]
        unidad <- omc_indicadores$Unidad[i]
        seccion <- omc_indicadores$Seccion[i]
        subcategoria <- omc_indicadores$Subcategoria[i]
        orden <- omc_indicadores$orden_excel[i]
        decimales <- if ("Decimales" %in% names(omc_indicadores)) omc_indicadores$Decimales[i] else 1L
        
        tryCatch({
          datos <- wtor::get_timeseries_data(
            code = codigo,
            reporting_economies = codigo_pais,
            time_period = "all"
          )
          
          if (!is.null(datos) && nrow(datos) > 0 && "year" %in% names(datos) && "value" %in% names(datos)) {
            datos_procesados <- datos |>
              dplyr::mutate(year = as.integer(year)) |>
              dplyr::filter(year >= anio_inicio & year <= anio_fin) |>
              dplyr::mutate(
                indicador_codigo = codigo,
                indicador_nombre = nombre,
                unidad_corta = unidad,
                unidad_larga = unidad,
                valor = as.numeric(value),
                country = pais_codigo_iso2,
                iso2c = pais_codigo_iso2,
                fuente = "OMC",
                prioridad_fuente = 4L,
                seccion = seccion,
                subcategoria = subcategoria,
                orden_excel = orden,
                decimales = decimales
              ) |>
              dplyr::filter(!is.na(valor)) |>
              dplyr::select(country, year, iso2c, indicador_codigo, indicador_nombre,
                            unidad_corta, unidad_larga, valor, fuente, prioridad_fuente,
                            seccion, subcategoria, orden_excel, decimales)
            
            if (nrow(datos_procesados) > 0) {
              datos_lista[[codigo]] <- datos_procesados
            }
          }
        }, error = function(e) {
          message("   ⚠ OMC ", codigo, ": ", e$message)
        })
      }
      
      if (length(datos_lista) == 0) return(NULL)
      
      resultado <- dplyr::bind_rows(datos_lista)
      return(resultado)
      
    }, error = function(e) {
      message("Error descargando datos de OMC: ", e$message)
      return(NULL)
    })
  }
  
  # ============================================================================
  # FUNCIÓN COMBINADA
  # ============================================================================
  
  # Función de deduplicación por prioridad de fuente
  # Usa directamente indicador_nombre ya que los indicadores vienen del Excel
  deduplicar_por_prioridad <- function(datos) {
    if (is.null(datos) || nrow(datos) == 0) return(datos)
    
    # Si no existe prioridad_fuente, asignar valor alto (baja prioridad)
    if (!"prioridad_fuente" %in% names(datos)) {
      datos$prioridad_fuente <- 99
    }
    
    # Contar registros antes de deduplicar
    n_antes <- nrow(datos)
    
    # Deduplicar: mantener el registro con menor prioridad_fuente (mayor prioridad)
    # para cada combinación de indicador_nombre + year + unidad_corta
    datos_dedup <- datos |>
      dplyr::arrange(indicador_nombre, year, unidad_corta, prioridad_fuente) |>
      dplyr::group_by(indicador_nombre, year, unidad_corta) |>
      dplyr::slice(1) |>
      dplyr::ungroup()
    
    n_despues <- nrow(datos_dedup)
    n_eliminados <- n_antes - n_despues
    
    if (n_eliminados > 0) {
      message(paste0("   ℹ Deduplicación: ", n_eliminados, " registros duplicados eliminados"))
    }
    
    return(datos_dedup)
  }
  
  # Función para normalizar unidades y valores
  # Los valores se mantienen en la unidad original de la fuente
  normalizar_valores <- function(datos) {
    if (is.null(datos) || nrow(datos) == 0) return(datos)
    
    # NO se hacen conversiones de unidades
    # Los valores se mantienen exactamente como vienen de la fuente original
    # El usuario puede hacer las conversiones manualmente si lo necesita
    
    return(datos)
  }
  
  # ============================================================================
  # FUNCIÓN PARA DETECTAR CATEGORÍA DE INDICADOR
  # (usada tanto en descargar_datos_combinados como en exportar_a_excel)
  # ============================================================================
  
  detectar_categoria_indicador <- function(indicador_nombre, indicador_codigo) {
    nombre <- tolower(indicador_nombre)
    codigo <- tolower(indicador_codigo)
    
    # SECTOR REAL - sin ahorro ni inversión (ahora en sector exterior)
    if (grepl("pib|gdp|producción|producto interior|crecimiento económico", nombre) && 
        !grepl("per cápita|ppa|deflactor", nombre)) {
      return("Sector real")
    }
    if (grepl("consumo privado|consumo público|consumo hogares|consumo gobierno", nombre)) {
      return("Sector real")
    }
    if (grepl("formación bruta|fbcf|capital fijo", nombre)) {
      return("Sector real")
    }
    # Oferta (agricultura, industria, servicios)
    if (grepl("valor añadido|agricultura|industria|servicios|construcción|manufacturas|silvicultura", nombre) && 
        grepl("% pib|porcentaje", nombre, ignore.case = TRUE)) {
      return("Sector real")
    }
    # Exportaciones netas contribución al crecimiento
    if (grepl("exportaciones netas|contrib", nombre)) {
      return("Sector real")
    }
    if (codigo %in% c("ngdp_rpch", "ngdpd", "ngdp", "b1gq", "nx_rpch")) {
      return("Sector real")
    }
    
    # MERCADO LABORAL
    if (grepl("desempleo|unemployment|paro", nombre)) {
      return("Mercado laboral")
    }
    if (grepl("empleo|employment|ocupación|ocupados|población activa|fuerza laboral", nombre) && 
        !grepl("autoempleo", nombre)) {
      return("Mercado laboral")
    }
    if (grepl("clu|coste laboral|salario", nombre) && !grepl("impuesto", nombre)) {
      return("Mercado laboral")
    }
    if (grepl("productividad", nombre) && grepl("trabajo|hora|laboral", nombre)) {
      return("Mercado laboral")
    }
    if (codigo %in% c("lur", "le", "le_rpch", "lf", "lf_rpch", "sl.uem.totl.zs", "sl.emp.totl.sp.zs")) {
      return("Mercado laboral")
    }
    
    # SECTOR EXTERIOR - incluyendo ahorro e inversión
    if (grepl("ahorro nacional|inversión total|inversión doméstica", nombre)) {
      return("Sector exterior")
    }
    if (grepl("cuenta corriente|current account|balanza corriente", nombre)) {
      return("Sector exterior")
    }
    if (grepl("exportacion|importacion|export|import|comercio exterior", nombre)) {
      return("Sector exterior")
    }
    if (grepl("balanza de bienes|balanza comercial|trade balance", nombre)) {
      return("Sector exterior")
    }
    if (grepl("inversión directa|inversión cartera|cuenta financiera|cuenta capital", nombre)) {
      return("Sector exterior")
    }
    if (grepl("rentas primarias|rentas secundarias|remesas", nombre)) {
      return("Sector exterior")
    }
    if (grepl("tipo de cambio|exchange rate|reer|neer", nombre)) {
      return("Sector exterior")
    }
    if (grepl("arancel|tariff", nombre)) {
      return("Sector exterior")
    }
    if (grepl("reservas internacionales|reservas oficiales", nombre)) {
      return("Sector exterior")
    }
    if (grepl("posición de inversión|deuda externa|niip|pii neta", nombre)) {
      return("Sector exterior")
    }
    if (grepl("^b[a-z]{2,3}_|^cab|^bgs|^bxg|^bmg|^bfa|^bfd|^bfp|^bip|^bis_|^bra|^bniip|^d_ngdpd", codigo)) {
      return("Sector exterior")
    }
    if (codigo %in% c("bca_ngdpd", "tx_rpch", "tm_rpch", "reer_bis_broad", "neer_bis_broad", 
                      "nid_ngdp", "ngsd_ngdp")) {
      return("Sector exterior")
    }
    
    # SECTOR PÚBLICO
    if (grepl("deuda pública|deuda del gobierno|government debt|deuda bruta", nombre)) {
      return("Sector público")
    }
    if (grepl("saldo fiscal|saldo público|déficit|superávit|balance fiscal", nombre)) {
      return("Sector público")
    }
    if (grepl("saldo primario|saldo estructural|saldo cíclico", nombre)) {
      return("Sector público")
    }
    if (grepl("ingresos públicos|ingresos del gobierno|revenue", nombre) && 
        !grepl("per cápita", nombre)) {
      return("Sector público")
    }
    if (grepl("gastos públicos|gasto del gobierno|expenditure", nombre) && 
        grepl("% pib|gobierno", nombre)) {
      return("Sector público")
    }
    if (grepl("intereses de la deuda|pago de intereses", nombre)) {
      return("Sector público")
    }
    if (codigo %in% c("ggxwdg_ngdp", "ggxcnl_ngdp", "ggxonlb_ngdp", "ggr_ngdp", "ggx_ngdp", "ggsb")) {
      return("Sector público")
    }
    
    # PRECIOS E INDICADORES MONETARIOS
    if (grepl("variación interanual|ipc|cpi|hicp|precios al consumo", nombre)) {
      return("Indicadores monetarios y financieros")
    }
    if (grepl("deflactor", nombre)) {
      return("Indicadores monetarios y financieros")
    }
    if (grepl("tipo de interés|interest rate|tasa de interés|tipo interbancario", nombre)) {
      return("Indicadores monetarios y financieros")
    }
    if (grepl("masa monetaria|agregado monetario|m1|m2|m3|base monetaria", nombre)) {
      return("Indicadores monetarios y financieros")
    }
    if (grepl("crédito|credit|préstamos", nombre) && 
        grepl("sector privado|bancario|interno", nombre)) {
      return("Indicadores monetarios y financieros")
    }
    if (grepl("morosidad|non.?performing|npl|préstamos dudosos|préstamos morosos|ratio de préstamos", nombre)) {
      return("Indicadores monetarios y financieros")
    }
    if (grepl("roe|roa|rentabilidad bancaria|ratio de capital|solvencia bancaria|retorno sobre", nombre)) {
      return("Indicadores monetarios y financieros")
    }
    if (grepl("spread|diferencial|margen de intermediación|margen de interés", nombre)) {
      return("Indicadores monetarios y financieros")
    }
    # FSI indicators
    if (grepl("capital regulatorio|capital sobre|tier 1|tier 2|activos ponderados|ratio liquidez|depósitos clientes", nombre)) {
      return("Indicadores monetarios y financieros")
    }
    if (grepl("fsi|^fs[a-z]{2,}", codigo)) {
      return("Indicadores monetarios y financieros")
    }
    if (grepl("pcpi|cpi_|headline|core|^ipc", codigo)) {
      return("Indicadores monetarios y financieros")
    }
    if (codigo %in% c("pcpipch", "pcpiepch", "ngdp_d", "fpolm_pa", "fitb_pa", "fm2_xdc", 
                      "fsanl_pt", "fsera_pt", "fskrc_pt", "fskt1_pt", "fslal_pt", "fsctdl_pt", "fseroa_pt")) {
      return("Indicadores monetarios y financieros")
    }
    
    # PRO-MEMORIA
    if (grepl("población|population", nombre) && !grepl("activa|ocupada", nombre)) {
      return("Pro-memoria")
    }
    if (grepl("per cápita|ppa|paridad|purchasing power", nombre)) {
      return("Pro-memoria")
    }
    if (grepl("gini|desigualdad|inequality|pobreza|poverty", nombre)) {
      return("Pro-memoria")
    }
    if (grepl("esperanza de vida|life expectancy|mortalidad", nombre)) {
      return("Pro-memoria")
    }
    if (grepl("idh|desarrollo humano|human development", nombre)) {
      return("Pro-memoria")
    }
    if (codigo %in% c("lp", "ppppc", "si.pov.gini", "sp.pop.totl")) {
      return("Pro-memoria")
    }
    
    # Si no coincide con ninguna categoría
    return("Sin clasificar")
  }
  
  descargar_datos_combinados <- function(pais_codigo, fecha_inicio, fecha_fin,
                                         usar_fmi = TRUE,
                                         usar_bm = TRUE,
                                         usar_omc = TRUE,
                                         usar_bis = TRUE,
                                         usar_eurostat = TRUE,
                                         actualizar_progreso = NULL) {
    datos_lista <- list()
    resumen_fuentes <- list()
    
    # Función auxiliar para actualizar progreso
    progreso <- function(valor, detalle) {
      message(detalle)
      if (!is.null(actualizar_progreso)) {
        actualizar_progreso(valor, detalle)
      }
    }
    
    # Contar fuentes activas para calcular incrementos
    n_fuentes <- sum(c(usar_fmi, usar_eurostat, usar_bm, usar_omc, usar_bis))
    incremento <- if (n_fuentes > 0) 0.7 / n_fuentes else 0
    progreso_actual <- 0.1
    
    # 1. FMI (todas las bases de datos disponibles) - Prioridad 1
    if (usar_fmi) {
      progreso(progreso_actual, "Descargando FMI (WEO, FM, BOP, CPI)...")
      
      datos_fmi <- tryCatch({
        descargar_datos_fmi(pais_codigo, fecha_inicio, fecha_fin)
      }, error = function(e) {
        message("     ⚠ FMI: ", e$message)
        NULL
      })
      
      if (!is.null(datos_fmi) && nrow(datos_fmi) > 0) {
        datos_lista$fmi <- datos_fmi
        
        # Contar registros por base de datos para el resumen
        resumen_por_base <- datos_fmi |>
          dplyr::group_by(fuente) |>
          dplyr::summarise(n = dplyr::n(), .groups = "drop")
        
        for (i in seq_len(nrow(resumen_por_base))) {
          resumen_fuentes[[resumen_por_base$fuente[i]]] <- resumen_por_base$n[i]
        }
        
        message(paste0("✓ FMI completado: ", nrow(datos_fmi), " registros totales"))
      } else {
        message("   ℹ FMI: sin datos disponibles")
      }
      
      progreso_actual <- progreso_actual + incremento
    }
    
    # 2. Eurostat - Prioridad 2
    if (usar_eurostat) {
      progreso(progreso_actual, "Descargando Eurostat...")
      datos_eurostat <- descargar_datos_eurostat(pais_codigo, fecha_inicio, fecha_fin)
      if (!is.null(datos_eurostat) && nrow(datos_eurostat) > 0) {
        datos_lista$eurostat <- datos_eurostat
        resumen_fuentes$Eurostat <- nrow(datos_eurostat)
        message(paste0("✓ Eurostat: ", nrow(datos_eurostat), " registros"))
      } else {
        message("   ℹ Eurostat: sin datos (país no UE o error de conexión)")
      }
      progreso_actual <- progreso_actual + incremento
    }
    
    # 3. Banco Mundial - Prioridad 3
    if (usar_bm) {
      progreso(progreso_actual, "Descargando Banco Mundial (WDI)...")
      datos_bm <- descargar_datos_bm(pais_codigo, fecha_inicio, fecha_fin)
      if (!is.null(datos_bm) && nrow(datos_bm) > 0) {
        datos_lista$bm <- datos_bm
        resumen_fuentes$`Banco Mundial` <- nrow(datos_bm)
        message(paste0("✓ Banco Mundial: ", nrow(datos_bm), " registros"))
      }
      progreso_actual <- progreso_actual + incremento
    }
    
    # 5. OMC - Prioridad 4
    if (usar_omc) {
      progreso(progreso_actual, "Descargando OMC (aranceles)...")
      datos_omc <- descargar_datos_omc(pais_codigo, fecha_inicio, fecha_fin)
      if (!is.null(datos_omc) && nrow(datos_omc) > 0) {
        datos_lista$omc <- datos_omc
        resumen_fuentes$OMC <- nrow(datos_omc)
        message(paste0("✓ OMC: ", nrow(datos_omc), " registros"))
      }
      progreso_actual <- progreso_actual + incremento
    }
    
    # 6. BIS - Prioridad 5
    if (usar_bis) {
      progreso(progreso_actual, "Descargando BIS (tipos de cambio)...")
      datos_bis <- tryCatch({
        descargar_datos_bis(pais_codigo, fecha_inicio, fecha_fin)
      }, error = function(e) {
        message("   ⚠ BIS: ", e$message)
        NULL
      })
      if (!is.null(datos_bis) && nrow(datos_bis) > 0) {
        datos_lista$bis <- datos_bis
        resumen_fuentes$BIS <- nrow(datos_bis)
        message(paste0("✓ BIS: ", nrow(datos_bis), " registros"))
      }
      progreso_actual <- progreso_actual + incremento
    }
    
    if (length(datos_lista) == 0) {
      return(list(datos = NULL, resumen = resumen_fuentes))
    }
    
    # Cargar indicadores del Excel para enriquecer datos
    indicadores_excel <- cargar_indicadores_excel()
    
    # Asegurar que todas las columnas necesarias existen y añadir info del Excel
    datos_lista <- lapply(datos_lista, function(df) {
      if (is.null(df) || nrow(df) == 0) return(df)
      
      if ("year" %in% names(df)) {
        df$year <- as.integer(df$year)
      }
      if (!"prioridad_fuente" %in% names(df)) {
        df$prioridad_fuente <- 99
      }
      
      # Añadir columnas del Excel si no existen
      if (!is.null(indicadores_excel) && "indicador_codigo" %in% names(df)) {
        # Crear mapeo desde Excel
        excel_info <- indicadores_excel |>
          dplyr::select(Codigo, Seccion, Subcategoria, orden_excel, Nombre_ES, Unidad, 
                        dplyr::any_of(c("Decimales", "Nivel"))) |>
          dplyr::rename(indicador_codigo = Codigo)
        
        # Añadir info del Excel si falta
        if (!"seccion" %in% names(df)) {
          cols_join <- c("indicador_codigo", "seccion" = "Seccion", 
                         "subcategoria" = "Subcategoria", "orden_excel")
          df <- df |>
            dplyr::left_join(
              excel_info |> dplyr::select(indicador_codigo, seccion = Seccion, 
                                          subcategoria = Subcategoria, orden_excel),
              by = "indicador_codigo"
            )
        }
        
        # Añadir nivel desde Excel si existe y no está en df
        if (!"nivel" %in% names(df) && "Nivel" %in% names(excel_info)) {
          df <- df |>
            dplyr::left_join(
              excel_info |> dplyr::select(indicador_codigo, nivel = Nivel),
              by = "indicador_codigo"
            )
        }
        
        # Añadir decimales desde Excel si existe y no está en df
        if (!"decimales" %in% names(df) && "Decimales" %in% names(excel_info)) {
          df <- df |>
            dplyr::left_join(
              excel_info |> dplyr::select(indicador_codigo, decimales = Decimales),
              by = "indicador_codigo"
            )
        }
        
        # Actualizar nombre desde Excel si está disponible
        if ("indicador_nombre" %in% names(df)) {
          df <- df |>
            dplyr::left_join(
              excel_info |> dplyr::select(indicador_codigo, nombre_excel = Nombre_ES),
              by = "indicador_codigo"
            ) |>
            dplyr::mutate(
              indicador_nombre = dplyr::coalesce(nombre_excel, indicador_nombre)
            ) |>
            dplyr::select(-nombre_excel)
        }
      }
      
      # Asegurar que las columnas existen (valores por defecto si no)
      if (!"seccion" %in% names(df)) df$seccion <- NA_character_
      if (!"subcategoria" %in% names(df)) df$subcategoria <- NA_character_
      if (!"orden_excel" %in% names(df)) df$orden_excel <- NA_integer_
      if (!"nivel" %in% names(df)) df$nivel <- 1L
      if (!"decimales" %in% names(df)) df$decimales <- 1L
      
      return(df)
    })
    
    # Combinar todos los datos
    datos_combinados <- dplyr::bind_rows(datos_lista)
    n_total_antes <- nrow(datos_combinados)
    
    # Guardar valor original ANTES de cualquier transformación
    datos_combinados$valor_original <- datos_combinados$valor
    
    # Aplicar deduplicación por prioridad de fuente
    progreso(0.85, "Eliminando duplicados...")
    datos_deduplicados <- deduplicar_por_prioridad(datos_combinados)
    
    # Normalizar valores (ajustar unidades) - pero mantener valor_original
    progreso(0.90, "Normalizando valores...")
    datos_normalizados <- normalizar_valores(datos_deduplicados)
    
    # Enriquecer con nombres en inglés del FMI
    progreso(0.92, "Obteniendo nombres en inglés...")
    datos_normalizados <- enriquecer_con_nombres_ingles(datos_normalizados)
    
    # Usar sección del Excel como categoría si está disponible
    # Para los que no tienen sección, aplicar detección fila por fila
    datos_normalizados <- datos_normalizados |>
      dplyr::mutate(
        categoria = dplyr::if_else(
          !is.na(seccion) & seccion != "",
          seccion,
          purrr::map2_chr(indicador_nombre, indicador_codigo, detectar_categoria_indicador)
        )
      )
    
    # Ordenar por orden_excel para preservar el orden del archivo
    datos_normalizados <- datos_normalizados |>
      dplyr::arrange(orden_excel, indicador_codigo, year)
    
    n_total_despues <- nrow(datos_normalizados)
    n_indicadores <- length(unique(datos_normalizados$indicador_nombre))
    message(paste0("✅ Descarga completada: ", n_total_despues, " registros únicos de ", n_indicadores, " indicadores"))
    
    return(list(
      datos = datos_normalizados,
      resumen = resumen_fuentes
    ))
  }
  
  # ============================================================================
  # ORGANIZAR POR CATEGORÍA
  # ============================================================================
  
  organizar_por_categoria <- function(datos, anio_inicio = NULL, anio_fin = NULL) {
    if (is.null(datos) || nrow(datos) == 0) {
      return(list())
    }
    
    # Asegurar que year es integer
    datos$year <- as.integer(datos$year)
    
    if (is.null(anio_inicio)) anio_inicio <- min(datos$year, na.rm = TRUE)
    if (is.null(anio_fin)) anio_fin <- max(datos$year, na.rm = TRUE)
    todos_los_anios <- seq(anio_inicio, anio_fin)
    
    get_codigos <- function(lista) {
      unlist(lapply(lista, names), use.names = FALSE)
    }
    
    # Función para detectar indicadores por patrones regex
    detectar_categoria <- function(indicador_nombre, indicador_codigo) {
      nombre <- tolower(indicador_nombre)
      codigo <- tolower(indicador_codigo)
      
      # SECTOR REAL
      if (grepl("pib|gdp|producción|producto interior|crecimiento económico", nombre) && 
          !grepl("per cápita|ppa|deflactor", nombre)) {
        return("sector_real")
      }
      if (grepl("consumo privado|consumo público|consumo hogares|consumo gobierno", nombre)) {
        return("sector_real")
      }
      if (grepl("formación bruta|inversión total|fbcf|capital fijo", nombre)) {
        return("sector_real")
      }
      if (grepl("ahorro nacional|ahorro bruto", nombre)) {
        return("sector_real")
      }
      if (grepl("valor añadido|agricultura|industria|servicios|construcción", nombre) && 
          grepl("% pib|porcentaje", nombre, ignore.case = TRUE)) {
        return("sector_real")
      }
      if (codigo %in% c("ngdp_rpch", "ngdpd", "ngdp", "nid_ngdp", "ngsd_ngdp", "b1gq")) {
        return("sector_real")
      }
      
      # MERCADO LABORAL
      if (grepl("desempleo|unemployment|paro", nombre)) {
        return("mercado_laboral")
      }
      if (grepl("empleo|employment|ocupación|ocupados|población activa", nombre) && 
          !grepl("autoempleo", nombre)) {
        return("mercado_laboral")
      }
      if (grepl("clu|coste laboral|salario", nombre) && !grepl("impuesto", nombre)) {
        return("mercado_laboral")
      }
      if (grepl("productividad", nombre) && grepl("trabajo|hora|laboral", nombre)) {
        return("mercado_laboral")
      }
      if (codigo %in% c("lur", "le", "sl.uem.totl.zs")) {
        return("mercado_laboral")
      }
      
      # SECTOR EXTERIOR (incluye BOP)
      if (grepl("cuenta corriente|current account|balanza corriente", nombre)) {
        return("sector_exterior")
      }
      if (grepl("exportacion|importacion|export|import|comercio exterior", nombre)) {
        return("sector_exterior")
      }
      if (grepl("balanza de bienes|balanza comercial|trade balance", nombre)) {
        return("sector_exterior")
      }
      if (grepl("inversión directa|inversión cartera|cuenta financiera|cuenta capital", nombre)) {
        return("sector_exterior")
      }
      if (grepl("rentas primarias|rentas secundarias|remesas", nombre)) {
        return("sector_exterior")
      }
      if (grepl("tipo de cambio|exchange rate|reer|neer", nombre)) {
        return("sector_exterior")
      }
      if (grepl("arancel|tariff", nombre)) {
        return("sector_exterior")
      }
      if (grepl("reservas internacionales|reservas oficiales", nombre)) {
        return("sector_exterior")
      }
      # Códigos BOP comunes (CAB=Cuenta corriente, BGS=Bienes y servicios, BXG=Export bienes, etc.)
      if (grepl("^b[a-z]{2,3}_|^cab|^bgs|^bxg|^bmg|^bfa|^bfd|^bfp|^bip|^bis_|^bra", codigo)) {
        return("sector_exterior")
      }
      if (codigo %in% c("bca_ngdpd", "tx_rpch", "tm_rpch", "reer_bis_broad", "neer_bis_broad")) {
        return("sector_exterior")
      }
      
      # SECTOR PÚBLICO
      if (grepl("deuda pública|deuda del gobierno|government debt|deuda bruta", nombre)) {
        return("sector_publico")
      }
      if (grepl("saldo fiscal|saldo público|déficit|superávit|balance fiscal", nombre)) {
        return("sector_publico")
      }
      if (grepl("saldo primario|saldo estructural|saldo cíclico", nombre)) {
        return("sector_publico")
      }
      if (grepl("ingresos públicos|ingresos del gobierno|revenue", nombre) && 
          !grepl("per cápita", nombre)) {
        return("sector_publico")
      }
      if (grepl("gastos públicos|gasto del gobierno|expenditure", nombre) && 
          grepl("% pib|gobierno", nombre)) {
        return("sector_publico")
      }
      if (grepl("intereses de la deuda|pago de intereses", nombre)) {
        return("sector_publico")
      }
      if (codigo %in% c("ggxwdg_ngdp", "ggxcnl_ngdp", "ggxonlb_ngdp", "ggr_ngdp", "ggx_ngdp", "ggsb")) {
        return("sector_publico")
      }
      
      # PRECIOS, COSTES E INDICADORES MONETARIOS
      if (grepl("inflación|inflation|ipc|cpi|hicp|precios al consumo", nombre)) {
        return("precios_monetarios")
      }
      if (grepl("deflactor", nombre)) {
        return("precios_monetarios")
      }
      if (grepl("tipo de interés|interest rate|tasa de interés|tipo interbancario", nombre)) {
        return("precios_monetarios")
      }
      if (grepl("masa monetaria|agregado monetario|m1|m2|m3|base monetaria", nombre)) {
        return("precios_monetarios")
      }
      if (grepl("crédito|credit|préstamos", nombre) && 
          grepl("sector privado|bancario|interno", nombre)) {
        return("precios_monetarios")
      }
      if (grepl("morosidad|non.?performing|npl|préstamos dudosos", nombre)) {
        return("precios_monetarios")
      }
      if (grepl("roe|roa|rentabilidad bancaria|ratio de capital|solvencia bancaria", nombre)) {
        return("precios_monetarios")
      }
      if (grepl("spread|diferencial|margen de intermediación", nombre)) {
        return("precios_monetarios")
      }
      # Códigos IPC/CPI
      if (grepl("pcpi|cpi_|headline|core|^ipc", codigo)) {
        return("precios_monetarios")
      }
      if (codigo %in% c("pcpipch", "pcpiepch", "ngdp_d", "fpolm_pa", "fitb_pa", "fm2_xdc")) {
        return("precios_monetarios")
      }
      
      # PRO-MEMORIA
      if (grepl("población|population", nombre) && !grepl("activa|ocupada", nombre)) {
        return("pro_memoria")
      }
      if (grepl("per cápita|ppa|paridad|purchasing power", nombre)) {
        return("pro_memoria")
      }
      if (grepl("gini|desigualdad|inequality|pobreza|poverty", nombre)) {
        return("pro_memoria")
      }
      if (grepl("esperanza de vida|life expectancy|mortalidad", nombre)) {
        return("pro_memoria")
      }
      if (grepl("idh|desarrollo humano|human development", nombre)) {
        return("pro_memoria")
      }
      if (codigo %in% c("lp", "ppppc", "si.pov.gini", "sp.pop.totl")) {
        return("pro_memoria")
      }
      
      # Si no coincide con ninguna categoría
      return(NA)
    }
    
    # Aplicar detección de categoría a cada fila
    # Primero intentar usar la sección del Excel, si no existe usar detección automática
    datos <- datos |>
      dplyr::mutate(
        # Mapear secciones del Excel a nombres de categoría interna
        categoria_excel = dplyr::case_when(
          seccion == "Sector real" ~ "sector_real",
          seccion == "Mercado laboral" ~ "mercado_laboral",
          seccion == "Sector exterior" ~ "sector_exterior",
          seccion == "Sector público" ~ "sector_publico",
          seccion == "Indicadores monetarios y financieros" ~ "precios_monetarios",
          seccion == "Comercio y estructura económica" ~ "sector_exterior",  # Asignar a sector exterior
          seccion == "Pro-memoria" ~ "pro_memoria",
          TRUE ~ NA_character_
        )
      ) |>
      dplyr::rowwise() |>
      dplyr::mutate(
        # Usar sección del Excel si existe, sino detectar automáticamente
        categoria_detectada = dplyr::if_else(
          !is.na(categoria_excel),
          categoria_excel,
          detectar_categoria(indicador_nombre, indicador_codigo)
        )
      ) |>
      dplyr::ungroup() |>
      dplyr::select(-categoria_excel)
    
    # Función auxiliar para procesar cada categoría
    procesar_categoria <- function(datos_filtrados) {
      if (nrow(datos_filtrados) == 0) return(NULL)
      
      datos_cat <- datos_filtrados |>
        dplyr::group_by(year, indicador_nombre, unidad_corta, unidad_larga, fuente, subcategoria, orden_excel) |>
        dplyr::summarise(valor = dplyr::first(valor, na_rm = TRUE), .groups = "drop") |>
        dplyr::ungroup()
      
      if (nrow(datos_cat) == 0) return(NULL)
      
      indicadores_unicos <- datos_cat |>
        dplyr::select(indicador_nombre, unidad_corta, unidad_larga, fuente, subcategoria, orden_excel) |>
        dplyr::distinct()
      
      combinacion_completa <- tidyr::expand_grid(
        indicadores_unicos,
        year = todos_los_anios
      )
      
      datos_cat_completos <- combinacion_completa |>
        dplyr::left_join(datos_cat, by = c("indicador_nombre", "unidad_corta", "unidad_larga", "fuente", "subcategoria", "orden_excel", "year"))
      
      datos_cat_pivot <- datos_cat_completos |>
        dplyr::arrange(orden_excel) |>  # Ordenar por orden del Excel
        tidyr::pivot_wider(
          id_cols = c(indicador_nombre, unidad_corta, unidad_larga, fuente, subcategoria, orden_excel),
          names_from = year,
          values_from = valor,
          names_sort = TRUE,
          values_fn = list(valor = ~mean(.x, na.rm = TRUE))
        ) |>
        dplyr::arrange(orden_excel) |>  # Mantener orden del Excel
        dplyr::select(-orden_excel)  # Quitar columna orden_excel del resultado final
      
      return(datos_cat_pivot)
    }
    
    # Crear listas de datos por categoría
    datos_por_categoria <- list()
    
    # Categorías principales (las del Excel más las detectadas automáticamente)
    categorias <- c("sector_real", "mercado_laboral", "sector_exterior", 
                    "sector_publico", "precios_monetarios", "pro_memoria")
    
    for (cat in categorias) {
      datos_cat <- datos |> dplyr::filter(categoria_detectada == cat)
      resultado <- procesar_categoria(datos_cat)
      if (!is.null(resultado) && nrow(resultado) > 0) {
        datos_por_categoria[[cat]] <- resultado
      }
    }
    
    return(datos_por_categoria)
  }
  
  # ============================================================================
  # ORDEN DE INDICADORES (según dictamen de Narnia)
  # ============================================================================
  
  orden_indicadores <- c(
    # Sector Real - Producción y demanda
    "PIB real", "PIB nominal",
    "PIB per cápita", "Demanda doméstica",
    "Consumo privado", "Consumo público",
    "Formación bruta de capital", "Formación bruta de capital fijo", "Inversión total",
    "Exportaciones de bienes y servicios", "Exportaciones",
    "Importaciones de bienes y servicios", "Importaciones",
    "Volumen de exportaciones", "Volumen de importaciones",
    "Ahorro nacional bruto",
    # Sector Real - Oferta
    "Agricultura, valor añadido", "VAB Agricultura",
    "Industria, valor añadido", "VAB Industria",
    "Servicios, valor añadido", "VAB Servicios",
    "VAB Construcción", "VAB Comercio, transporte, hostelería",
    # Mercado laboral
    "Tasa de desempleo", "Desempleo juvenil", "Desempleo nacional",
    "Tasa de actividad", "Tasa de empleo",
    "Población activa", "Empleo", "Fuerza laboral",
    "CLU nominales", "Productividad/hora",
    # Sector exterior
    "Media simple del arancel NMF", "Media ponderada del arancel NMF",
    "Cuenta corriente", "Balanza cuenta corriente",
    "Inversión extranjera directa neta",
    "Deuda externa", "Deuda externa bruta", "Reservas internacionales",
    "Tipo de cambio oficial", "Tipo de cambio USD/EUR",
    "Tipo de cambio efectivo real", "Tipo de cambio efectivo nominal",
    "Posición de inversión internacional neta",
    "Ahorro nacional bruto", "Inversión total",
    # Sector público
    "Ingresos públicos", "Gastos públicos", "Gasto público",
    "Recaudación tributaria",
    "Saldo público", "Saldo fiscal", "Saldo primario", "Saldo estructural",
    "Deuda pública", "Deuda pública bruta", "Deuda pública neta",
    # Precios
    "Tasa de variación interanual del IPC promedio", 
    "Tasa de variación interanual del IPC al final del periodo",
    "HICP Inflación", "HICP subyacente", "Deflactor del PIB",
    # Monetarios
    "Masa monetaria (M2)", "Agregado monetario M1", "Agregado monetario M2",
    "Crédito al sector privado",
    "Tipo de interés de política monetaria", "Tipo de interés letras del Tesoro",
    "Tipo de interés de préstamos", "Tipo de interés de depósitos",
    "Tipo interbancario",
    # Pro-memoria
    "Población total", "Población", "Población urbana",
    "PIB per cápita (PPA)", "PIB (PPA)", "INB per cápita (PPA)",
    "Índice de Gini", "Esperanza de vida al nacer"
  )
  
  # Función para ordenar indicadores según orden predefinido
  ordenar_indicadores <- function(datos) {
    if (is.null(datos) || nrow(datos) == 0) return(datos)
    
    # Crear índice de orden
    datos$orden_idx <- match(datos$indicador_nombre, orden_indicadores)
    datos$orden_idx[is.na(datos$orden_idx)] <- 999  # Los no encontrados van al final
    
    # Ordenar
    datos <- datos |>
      dplyr::arrange(orden_idx) |>
      dplyr::select(-orden_idx)
    
    return(datos)
  }
  
  # ============================================================================
  # EXPORTAR A WORD
  # ============================================================================
  
  exportar_a_word <- function(datos_por_categoria, pais_nombre, fecha_inicio, fecha_fin, 
                              archivo_salida, plantilla_word = NULL) {
    
    # Colores definidos
    COLOR_MORADO <- "#5F2987"
    COLOR_VERDE_LINEA <- "#E2EFD9"  # Verde clarito para líneas divisorias
    COLOR_BLANCO <- "#FFFFFF"
    
    
    # Crear documento base (con o sin plantilla)
    if (!is.null(plantilla_word) && file.exists(plantilla_word)) {
      doc <- officer::read_docx(plantilla_word)
      message("Usando plantilla: ", plantilla_word)
      
      # Posicionarse en la 4ª página (después de las 3 primeras hojas de la plantilla)
      # Buscamos el bookmark "INICIO_DATOS" si existe, si no, vamos al final de la página 3
      tryCatch({
        doc <- officer::cursor_bookmark(doc, "INICIO_DATOS")
        message("Usando bookmark 'INICIO_DATOS' para insertar datos")
      }, error = function(e) {
        # Si no hay bookmark, ir al final del documento
        doc <<- officer::cursor_end(doc)
        message("No se encontró bookmark 'INICIO_DATOS', insertando al final")
      })
      
    } else {
      doc <- officer::read_docx()
      # Configurar página horizontal solo si no hay plantilla
      doc <- doc |> officer::body_set_default_section(
        officer::prop_section(
          page_size = officer::page_size(orient = "landscape"),
          page_margins = officer::page_mar(bottom = 0.5, top = 0.5, right = 0.5, left = 0.5,
                                           header = 0.3, footer = 0.3, gutter = 0)
        )
      )
    }
    
    # Definir propiedades de párrafo con keep_with_next para evitar saltos de página
    fp_par_keep <- officer::fp_par(
      keep_with_next = TRUE
    )
    
    # Definir propiedades de texto para títulos
    fp_titulo_principal <- officer::fp_text(
      font.size = 16,
      bold = TRUE,
      color = COLOR_MORADO,
      font.family = "Aptos"
    )
    
    fp_titulo_seccion <- officer::fp_text(
      font.size = 14,
      bold = TRUE,
      color = COLOR_MORADO,
      font.family = "Aptos"
    )
    
    fp_subtitulo <- officer::fp_text(
      font.size = 11,
      bold = TRUE,
      color = COLOR_MORADO,
      font.family = "Aptos"
    )
    
    fp_normal <- officer::fp_text(
      font.size = 10,
      color = "#333333",
      font.family = "Aptos"
    )
    
    # Mapeo de nombres de categoría interna a nombres del Excel
    # Los nombres de sección se tomarán directamente de los datos
    mapeo_categorias <- c(
      "sector_real" = "Sector real",
      "mercado_laboral" = "Mercado laboral",
      "sector_exterior" = "Sector exterior",
      "sector_publico" = "Sector público",
      "precios_monetarios" = "Indicadores monetarios y financieros",
      "pro_memoria" = "Pro-memoria"
    )
    
    # Mapeo alternativo para categorías que pueden venir de "Comercio y estructura económica"
    # Se mantendrá el nombre original del Excel que esté en los datos
    
    # Función para crear tabla con flextable
    crear_flextable <- function(datos_tabla, es_primera_tabla = FALSE) {
      
      # Eliminar columnas no deseadas (incluyendo subcategoria)
      if ("unidad_larga" %in% names(datos_tabla)) {
        datos_tabla <- datos_tabla |> dplyr::select(-unidad_larga)
      }
      if ("fuente" %in% names(datos_tabla)) {
        datos_tabla <- datos_tabla |> dplyr::select(-fuente)
      }
      if ("subcategoria" %in% names(datos_tabla)) {
        datos_tabla <- datos_tabla |> dplyr::select(-subcategoria)
      }
      
      # Guardar columna de decimales si existe antes de eliminarla
      decimales_por_fila <- NULL
      if ("decimales" %in% names(datos_tabla)) {
        decimales_por_fila <- datos_tabla$decimales
        datos_tabla <- datos_tabla |> dplyr::select(-decimales)
      }
      
      # Guardar columna de nivel si existe antes de eliminarla
      nivel_por_fila <- NULL
      if ("nivel" %in% names(datos_tabla)) {
        nivel_por_fila <- datos_tabla$nivel
        datos_tabla <- datos_tabla |> dplyr::select(-nivel)
      }
      
      cols_nombres <- names(datos_tabla)
      n_cols <- ncol(datos_tabla)
      
      # Formatear números en español (usando decimales específicos si existen)
      for (col in cols_nombres) {
        if (is.numeric(datos_tabla[[col]])) {
          if (!is.null(decimales_por_fila)) {
            # Usar decimales específicos por fila
            datos_tabla[[col]] <- mapply(function(x, d) {
              n_dec <- ifelse(is.na(d), 1, as.integer(d))
              formatear_numero_es(x, n_dec)
            }, datos_tabla[[col]], decimales_por_fila)
          } else {
            # Usar 1 decimal por defecto
            datos_tabla[[col]] <- sapply(datos_tabla[[col]], function(x) {
              formatear_numero_es(x, 1)
            })
          }
        } else {
          datos_tabla[[col]] <- ifelse(is.na(datos_tabla[[col]]) | datos_tabla[[col]] == "", 
                                       " ", as.character(datos_tabla[[col]]))
        }
      }
      
      # Formatear unidad corta entre paréntesis si no está vacía
      if ("unidad_corta" %in% cols_nombres) {
        datos_tabla$unidad_corta <- ifelse(
          datos_tabla$unidad_corta != " " & datos_tabla$unidad_corta != "",
          paste0("(", datos_tabla$unidad_corta, ")"), 
          " "
        )
      }
      
      # Renombrar columnas
      nombres_cols_tabla <- cols_nombres
      nombres_cols_tabla[nombres_cols_tabla == "indicador_nombre"] <- "Indicador"
      nombres_cols_tabla[nombres_cols_tabla == "unidad_corta"] <- " "
      names(datos_tabla) <- nombres_cols_tabla
      
      # Determinar índice de columna de unidad
      idx_unidad <- which(names(datos_tabla) == " ")
      
      # Crear flextable
      ft <- flextable::flextable(datos_tabla)
      
      # Aplicar fuente Aptos a toda la tabla
      ft <- ft |>
        flextable::font(fontname = "Aptos", part = "all") |>
        flextable::fontsize(size = 9, part = "body") |>
        flextable::fontsize(size = 10, part = "header")
      
      # Formato especial para columna de unidad: tamaño 8, cursiva, gris, alineado izquierda
      if (length(idx_unidad) > 0) {
        ft <- ft |>
          flextable::fontsize(size = 8, j = idx_unidad, part = "body") |>
          flextable::italic(j = idx_unidad, part = "body") |>
          flextable::color(color = "#969696", j = idx_unidad, part = "body") |>
          flextable::align(j = idx_unidad, align = "left", part = "body")
      }
      
      # Estilo del encabezado (morado con texto blanco)
      ft <- ft |>
        flextable::bg(bg = COLOR_MORADO, part = "header") |>
        flextable::color(color = COLOR_BLANCO, part = "header") |>
        flextable::bold(part = "header")
      
      # Alineación: primera columna a la izquierda
      ft <- ft |>
        flextable::align(j = 1, align = "left", part = "all")
      
      # Resto de columnas numéricas alineadas a la derecha (excluyendo unidad que ya está alineada)
      if (n_cols > 1) {
        # Columnas numéricas (excluyendo la de unidad)
        cols_numericas <- setdiff(2:n_cols, idx_unidad)
        if (length(cols_numericas) > 0) {
          ft <- ft |>
            flextable::align(j = cols_numericas, align = "right", part = "body") |>
            flextable::align(j = cols_numericas, align = "center", part = "header")
        }
        # La columna de unidad en el header también centrada
        if (length(idx_unidad) > 0) {
          ft <- ft |>
            flextable::align(j = idx_unidad, align = "center", part = "header")
        }
      }
      
      # Bordes: líneas horizontales verdes finas entre filas
      borde_verde <- officer::fp_border(color = COLOR_VERDE_LINEA, width = 0.75)
      borde_morado <- officer::fp_border(color = COLOR_MORADO, width = 1.5)
      
      ft <- ft |>
        flextable::border_remove() |>
        # Línea inferior del encabezado (morado, más grueso)
        flextable::hline(part = "header", border = borde_morado) |>
        # Líneas horizontales verdes entre todas las filas del cuerpo
        flextable::hline(part = "body", border = borde_verde) |>
        # Línea inferior de la tabla
        flextable::hline_bottom(part = "body", border = borde_verde)
      
      # Aplicar sangría según el nivel (si existe)
      # Nivel 1: sin sangría (0cm), Nivel 2: +0.5cm, Nivel 3: +1cm
      if (!is.null(nivel_por_fila)) {
        for (i in seq_along(nivel_por_fila)) {
          niv <- nivel_por_fila[i]
          if (!is.na(niv) && niv > 1) {
            # Calcular padding izquierdo en puntos (0.5cm = ~14 puntos por cada nivel adicional)
            padding_left <- (niv - 1) * 14
            ft <- ft |>
              flextable::padding(i = i, j = 1, padding.left = padding_left + 3, part = "body")
          }
        }
      }
      
      # Ajustar anchos de columna
      # Columna Indicador: 7cm (aproximadamente 2.76 pulgadas)
      # En landscape A4: ancho útil ≈ 27cm (29.7 - 2.7 de márgenes)
      # Resto: (27 - 7) / (n_cols - 1) cm por columna
      
      ancho_indicador <- 2.76  # 7cm en pulgadas
      
      ft <- ft |>
        flextable::width(j = 1, width = ancho_indicador)
      
      # Calcular ancho para el resto de columnas
      if (n_cols > 1) {
        # Ancho total disponible aproximado en pulgadas (landscape A4 con márgenes de 0.5")
        ancho_total <- 10.5  # pulgadas aproximadamente
        ancho_restante <- ancho_total - ancho_indicador
        ancho_por_columna <- ancho_restante / (n_cols - 1)
        
        ft <- ft |>
          flextable::width(j = 2:n_cols, width = ancho_por_columna)
      }
      
      # Padding
      ft <- ft |>
        flextable::padding(padding = 3, part = "all")
      
      # Mantener encabezado con el contenido (evitar que quede solo en página)
      ft <- ft |>
        flextable::paginate(init = TRUE, hdr_ftr = TRUE)
      
      return(ft)
    }
    
    # Procesar cada categoría usando las subcategorías del Excel
    for (cat_id in names(datos_por_categoria)) {
      datos_cat <- datos_por_categoria[[cat_id]]
      
      if (is.null(datos_cat) || nrow(datos_cat) == 0) next
      
      # Obtener nombre de sección del mapeo o usar el nombre de la categoría
      nombre_seccion <- mapeo_categorias[cat_id]
      if (is.na(nombre_seccion)) {
        nombre_seccion <- cat_id
      }
      
      # Añadir título de categoría/sección con formato morado (con keep_with_next)
      doc <- doc |>
        officer::body_add_fpar(
          officer::fpar(
            officer::ftext(nombre_seccion, prop = fp_titulo_seccion),
            fp_p = fp_par_keep
          )
        )
      
      # Verificar si hay columna de subcategoría
      if ("subcategoria" %in% names(datos_cat)) {
        # Obtener subcategorías únicas manteniendo el orden del Excel
        subcategorias_unicas <- unique(datos_cat$subcategoria)
        subcategorias_unicas <- subcategorias_unicas[!is.na(subcategorias_unicas) & subcategorias_unicas != ""]
        
        if (length(subcategorias_unicas) > 0) {
          es_primera_tabla <- TRUE
          
          for (subcat_nombre in subcategorias_unicas) {
            # Filtrar datos por subcategoría
            datos_subcat <- datos_cat |>
              dplyr::filter(subcategoria == subcat_nombre)
            
            if (nrow(datos_subcat) > 0) {
              # Añadir subtítulo con formato morado (con keep_with_next)
              doc <- doc |>
                officer::body_add_fpar(
                  officer::fpar(
                    officer::ftext(subcat_nombre, prop = fp_subtitulo),
                    fp_p = fp_par_keep
                  )
                )
              
              # Crear y añadir tabla con flextable (sin la columna subcategoria)
              ft <- crear_flextable(as.data.frame(datos_subcat), es_primera_tabla)
              doc <- doc |>
                flextable::body_add_flextable(ft)
              
              # Añadir nota sobre tipos de cambio si es la subcategoría correspondiente
              if (grepl("Competitividad|tipos de cambio", subcat_nombre, ignore.case = TRUE)) {
                nota_tc <- "Nota: Los tipos de cambio efectivos (TCER/TCEN) están denominados de tal manera que un aumento implica una apreciación. El tipo de cambio oficial frente al dólar está expresado en UML/USD, por lo que un aumento implica una depreciación de la moneda local frente al dólar estadounidense."
                doc <- doc |>
                  officer::body_add_fpar(
                    officer::fpar(
                      officer::ftext(nota_tc, 
                                     prop = officer::fp_text(font.size = 8, italic = TRUE, color = "#666666"))
                    )
                  )
              }
              
              doc <- doc |>
                officer::body_add_par("")
              
              es_primera_tabla <- FALSE
            }
          }
          
          # Añadir indicadores sin subcategoría si los hay
          datos_sin_subcat <- datos_cat |>
            dplyr::filter(is.na(subcategoria) | subcategoria == "")
          
          if (nrow(datos_sin_subcat) > 0) {
            doc <- doc |>
              officer::body_add_fpar(
                officer::fpar(
                  officer::ftext("Otros indicadores", prop = fp_subtitulo),
                  fp_p = fp_par_keep
                )
              )
            
            ft <- crear_flextable(as.data.frame(datos_sin_subcat), FALSE)
            doc <- doc |>
              flextable::body_add_flextable(ft) |>
              officer::body_add_par("")
          }
        } else {
          # Si no hay subcategorías válidas, mostrar todos los datos juntos
          ft <- crear_flextable(as.data.frame(datos_cat), TRUE)
          doc <- doc |>
            flextable::body_add_flextable(ft) |>
            officer::body_add_par("")
        }
      } else {
        # Si no existe columna subcategoria, mostrar todos los datos juntos
        ft <- crear_flextable(as.data.frame(datos_cat), TRUE)
        doc <- doc |>
          flextable::body_add_flextable(ft) |>
          officer::body_add_par("")
      }
    }
    
    print(doc, target = archivo_salida)
    return(archivo_salida)
  }
  
  # ============================================================================
  # EXPORTAR A EXCEL (con columna de fuente)
  # ============================================================================
  
  exportar_a_excel <- function(datos_por_categoria, datos_completos, pais_nombre,
                               pais_codigo, fecha_inicio, fecha_fin, archivo_salida) {
    
    wb <- openxlsx::createWorkbook()
    
    estilo_encabezado <- openxlsx::createStyle(
      fontSize = 11,
      fontName = "Segoe UI",
      fontColour = "#FFFFFF",
      fgFill = "#7FADCF",
      halign = "center",
      valign = "center",
      textDecoration = "bold",
      border = "TopBottomLeftRight",
      borderColour = "#5A9BC9"
    )
    
    estilo_datos <- openxlsx::createStyle(
      fontSize = 10,
      fontName = "Segoe UI",
      halign = "right",
      border = "TopBottomLeftRight",
      borderColour = "#D4E2EF",
      numFmt = "#.##0,00"
    )
    
    estilo_indicador <- openxlsx::createStyle(
      fontSize = 10,
      fontName = "Segoe UI",
      halign = "left",
      border = "TopBottomLeftRight",
      borderColour = "#D4E2EF"
    )
    
    estilo_titulo <- openxlsx::createStyle(
      fontSize = 14,
      fontName = "Segoe UI",
      fontColour = "#FFFFFF",
      fgFill = "#5A9BC9",
      halign = "center",
      textDecoration = "bold"
    )
    
    anio_inicio <- lubridate::year(fecha_inicio)
    anio_fin <- lubridate::year(fecha_fin)
    todos_los_anios <- as.character(seq(anio_inicio, anio_fin))
    
    openxlsx::addWorksheet(wb, "Información")
    
    info_data <- data.frame(
      Campo = c("País", "Código ISO", "Período inicio", "Período fin",
                "Fecha generación", "Fuentes de datos", "Orden de prioridad"),
      Valor = c(pais_nombre, pais_codigo,
                as.character(anio_inicio),
                as.character(anio_fin),
                format(Sys.Date(), "%d/%m/%Y"),
                "FMI (WEO, BOP, IFS, FSI, CPI, DOT, GFS), Eurostat, Banco Mundial (WDI), OMC, BIS",
                "1. FMI, 2. Eurostat, 3. Banco Mundial, 4. OMC, 5. BIS"),
      stringsAsFactors = FALSE
    )
    
    openxlsx::writeData(wb, "Información", info_data, startRow = 2, startCol = 2)
    openxlsx::addStyle(wb, "Información", estilo_encabezado, rows = 2, cols = 2:3, gridExpand = TRUE)
    openxlsx::setColWidths(wb, "Información", cols = 2:3, widths = c(25, 60))
    
    openxlsx::addWorksheet(wb, "Datos consolidados")
    
    if (!is.null(datos_completos) && nrow(datos_completos) > 0) {
      # Preparar datos con formato requerido incluyendo nombres en inglés:
      # Sección | Subcategoría | Indicador | Nombre (EN) | Descripción (EN) | Código | Unidad | Fuente | Base datos | Años...
      
      # Usar valor_original si existe, si no usar valor
      col_valor <- if ("valor_original" %in% names(datos_completos)) "valor_original" else "valor"
      
      # Verificar si ya existe la columna categoria
      tiene_categoria <- "categoria" %in% names(datos_completos)
      tiene_seccion <- "seccion" %in% names(datos_completos)
      tiene_subcategoria <- "subcategoria" %in% names(datos_completos)
      
      # Verificar si existen las columnas de nombres en inglés
      tiene_name_en <- "name_en" %in% names(datos_completos)
      tiene_description_en <- "description_en" %in% names(datos_completos)
      tiene_source_db <- "source_db" %in% names(datos_completos)
      tiene_orden_excel <- "orden_excel" %in% names(datos_completos)
      
      datos_para_excel <- datos_completos |>
        dplyr::mutate(
          year = as.integer(year),
          # Usar la categoría/sección del Excel si existe
          Seccion = dplyr::case_when(
            tiene_seccion & !is.na(seccion) & seccion != "" ~ seccion,
            tiene_categoria ~ categoria,
            TRUE ~ purrr::map2_chr(indicador_nombre, indicador_codigo, detectar_categoria_indicador)
          ),
          # Subcategoría del Excel
          Subcategoria = if (tiene_subcategoria) dplyr::coalesce(subcategoria, "") else "",
          # Código: si está vacío o es igual al nombre, poner NA
          Codigo = dplyr::case_when(
            is.na(indicador_codigo) | indicador_codigo == "" ~ NA_character_,
            indicador_codigo == indicador_nombre ~ NA_character_,
            TRUE ~ indicador_codigo
          ),
          # Unidad de medida
          Unidad = dplyr::case_when(
            !is.na(unidad_larga) & unidad_larga != "" ~ unidad_larga,
            !is.na(unidad_corta) & unidad_corta != "" ~ unidad_corta,
            TRUE ~ NA_character_
          ),
          # Columnas de nombres en inglés
          Nombre_EN = if (tiene_name_en) name_en else NA_character_,
          Descripcion_EN = if (tiene_description_en) description_en else NA_character_,
          Base_Datos = if (tiene_source_db) source_db else NA_character_,
          # Orden del Excel para mantener el orden original
          Orden_Excel = if (tiene_orden_excel) orden_excel else NA_integer_,
          # Valor original sin transformar
          valor_export = .data[[col_valor]]
        ) |>
        dplyr::select(year, Seccion, Subcategoria, Indicador = indicador_nombre, Nombre_EN, Descripcion_EN, 
                      Codigo, Unidad, Fuente = fuente, Base_Datos, Orden_Excel, valor_export) |>
        dplyr::group_by(year, Seccion, Subcategoria, Indicador, Nombre_EN, Descripcion_EN, Codigo, Unidad, Fuente, Base_Datos, Orden_Excel) |>
        dplyr::summarise(valor_export = dplyr::first(valor_export, na_rm = TRUE), .groups = "drop")
      
      # Crear combinación completa de indicadores y años
      indicadores_info <- datos_para_excel |>
        dplyr::select(Seccion, Subcategoria, Indicador, Nombre_EN, Descripcion_EN, Codigo, Unidad, Fuente, Base_Datos, Orden_Excel) |>
        dplyr::distinct()
      
      combinacion_completa <- tidyr::expand_grid(
        indicadores_info,
        year = as.integer(todos_los_anios)
      )
      
      # Unir y pivotar
      datos_para_excel <- combinacion_completa |>
        dplyr::left_join(datos_para_excel, 
                         by = c("Seccion", "Subcategoria", "Indicador", "Nombre_EN", "Descripcion_EN", 
                                "Codigo", "Unidad", "Fuente", "Base_Datos", "Orden_Excel", "year")) |>
        # Ordenar por orden del Excel primero
        dplyr::arrange(Orden_Excel, Indicador) |>
        tidyr::pivot_wider(
          id_cols = c(Seccion, Subcategoria, Indicador, Nombre_EN, Descripcion_EN, Codigo, Unidad, Fuente, Base_Datos, Orden_Excel),
          names_from = year,
          values_from = valor_export,
          names_sort = TRUE
        ) |>
        # Quitar columna de orden interno
        dplyr::select(-Orden_Excel) |>
        as.data.frame()
      
      # Reemplazar NA por texto vacío en columnas de texto
      datos_para_excel$Codigo <- ifelse(is.na(datos_para_excel$Codigo), "", datos_para_excel$Codigo)
      datos_para_excel$Unidad <- ifelse(is.na(datos_para_excel$Unidad), "", datos_para_excel$Unidad)
      datos_para_excel$Subcategoria <- ifelse(is.na(datos_para_excel$Subcategoria), "", datos_para_excel$Subcategoria)
      datos_para_excel$Nombre_EN <- ifelse(is.na(datos_para_excel$Nombre_EN), "", datos_para_excel$Nombre_EN)
      datos_para_excel$Descripcion_EN <- ifelse(is.na(datos_para_excel$Descripcion_EN), "", datos_para_excel$Descripcion_EN)
      datos_para_excel$Base_Datos <- ifelse(is.na(datos_para_excel$Base_Datos), "", datos_para_excel$Base_Datos)
      
      # Escribir datos
      openxlsx::writeData(wb, "Datos consolidados", datos_para_excel,
                          startRow = 1, startCol = 1, headerStyle = estilo_encabezado)
      
      n_filas <- nrow(datos_para_excel)
      n_cols <- ncol(datos_para_excel)
      n_cols_texto <- 8  # Sección, Indicador, Nombre_EN, Descripcion_EN, Código, Unidad, Fuente, Base_Datos
      
      if (n_filas > 0) {
        # Estilo para columnas de texto
        openxlsx::addStyle(wb, "Datos consolidados", estilo_indicador,
                           rows = 2:(n_filas + 1), cols = 1:n_cols_texto, gridExpand = TRUE)
        
        # Estilo para columnas de datos numéricos
        if (n_cols > n_cols_texto) {
          openxlsx::addStyle(wb, "Datos consolidados", estilo_datos,
                             rows = 2:(n_filas + 1),
                             cols = (n_cols_texto + 1):n_cols, gridExpand = TRUE)
        }
      }
      
      # Anchos de columna
      openxlsx::setColWidths(wb, "Datos consolidados", cols = 1, widths = 20)   # Sección
      openxlsx::setColWidths(wb, "Datos consolidados", cols = 2, widths = 45)   # Indicador
      openxlsx::setColWidths(wb, "Datos consolidados", cols = 3, widths = 50)   # Nombre_EN
      openxlsx::setColWidths(wb, "Datos consolidados", cols = 4, widths = 60)   # Descripcion_EN
      openxlsx::setColWidths(wb, "Datos consolidados", cols = 5, widths = 25)   # Código
      openxlsx::setColWidths(wb, "Datos consolidados", cols = 6, widths = 30)   # Unidad
      openxlsx::setColWidths(wb, "Datos consolidados", cols = 7, widths = 15)   # Fuente
      openxlsx::setColWidths(wb, "Datos consolidados", cols = 8, widths = 12)   # Base_Datos
      if (n_cols > n_cols_texto) {
        openxlsx::setColWidths(wb, "Datos consolidados", cols = (n_cols_texto + 1):n_cols, widths = 15)
      }
      openxlsx::freezePane(wb, "Datos consolidados", firstActiveRow = 2, firstActiveCol = (n_cols_texto + 1))
    }
    
    nombres_categorias <- c(
      "sector_real" = "Sector real",
      "mercado_laboral" = "Mercado laboral",
      "sector_exterior" = "Sector exterior",
      "sector_publico" = "Sector público",
      "precios_monetarios" = "Ind. monetarios y financieros",
      "pro_memoria" = "Pro-memoria"
    )
    
    for (cat_id in names(datos_por_categoria)) {
      cat_nombre <- nombres_categorias[cat_id]
      if (is.na(cat_nombre)) next
      
      datos_cat <- datos_por_categoria[[cat_id]]
      
      if (!is.null(datos_cat) && nrow(datos_cat) > 0) {
        datos_cat_df <- as.data.frame(datos_cat)
        
        if ("indicador_nombre" %in% names(datos_cat_df) && "unidad_larga" %in% names(datos_cat_df)) {
          datos_cat_df$indicador_completo <- ifelse(
            !is.na(datos_cat_df$unidad_larga) & datos_cat_df$unidad_larga != "",
            paste0(datos_cat_df$indicador_nombre, " (", datos_cat_df$unidad_larga, ")"),
            datos_cat_df$indicador_nombre
          )
          cols_a_eliminar <- intersect(names(datos_cat_df), c("indicador_nombre", "unidad_corta", "unidad_larga"))
          datos_cat_df <- datos_cat_df |>
            dplyr::select(-dplyr::all_of(cols_a_eliminar)) |>
            dplyr::select(indicador_completo, fuente, dplyr::everything())
          names(datos_cat_df)[names(datos_cat_df) == "indicador_completo"] <- "Indicador"
          names(datos_cat_df)[names(datos_cat_df) == "fuente"] <- "Fuente"
        }
        
        for (col in names(datos_cat_df)) {
          if (is.numeric(datos_cat_df[[col]])) {
            datos_cat_df[[col]] <- sapply(datos_cat_df[[col]], function(x) {
              if (is.na(x)) " " else round(x, 2)
            })
          }
        }
        
        openxlsx::addWorksheet(wb, cat_nombre)
        
        openxlsx::writeData(wb, cat_nombre, cat_nombre, startRow = 1, startCol = 1)
        openxlsx::mergeCells(wb, cat_nombre, rows = 1, cols = 1:ncol(datos_cat_df))
        openxlsx::addStyle(wb, cat_nombre, estilo_titulo, rows = 1, cols = 1)
        
        openxlsx::writeData(wb, cat_nombre, datos_cat_df, startRow = 3, startCol = 1,
                            headerStyle = estilo_encabezado)
        
        n_filas <- nrow(datos_cat_df)
        n_cols <- ncol(datos_cat_df)
        
        if (n_filas > 0) {
          openxlsx::addStyle(wb, cat_nombre, estilo_indicador,
                             rows = 4:(n_filas + 3), cols = 1, gridExpand = TRUE)
          
          openxlsx::addStyle(wb, cat_nombre, estilo_indicador,
                             rows = 4:(n_filas + 3), cols = 2, gridExpand = TRUE)
          
          if (n_cols > 2) {
            openxlsx::addStyle(wb, cat_nombre, estilo_datos,
                               rows = 4:(n_filas + 3),
                               cols = 3:n_cols, gridExpand = TRUE)
          }
        }
        
        openxlsx::setColWidths(wb, cat_nombre, cols = 1, widths = 50)
        openxlsx::setColWidths(wb, cat_nombre, cols = 2, widths = 15)
        if (n_cols > 2) {
          openxlsx::setColWidths(wb, cat_nombre, cols = 3:n_cols, widths = 12)
        }
        openxlsx::freezePane(wb, cat_nombre, firstActiveRow = 4, firstActiveCol = 3)
      }
    }
    
    openxlsx::saveWorkbook(wb, archivo_salida, overwrite = TRUE)
    return(archivo_salida)
  }
  
  # ============================================================================
  # INTERFAZ DE USUARIO (UI)
  # ============================================================================
  
  ui <- page_navbar(
    title = span(
      icon("chart-line"), " ",
      "Dictamen de coyuntura"
    ),
    theme = tema_pastel,
    fillable = FALSE,
    
    # Panel principal - Descarga de datos
    nav_panel(
      title = "Descarga de datos",
      icon = icon("download"),
      
      shinyjs::useShinyjs(),
      
      div(
        class = "container-fluid py-3",
        
        # Hero section con bandera
        div(
          class = "hero-section mb-4",
          layout_columns(
            col_widths = c(9, 3),
            div(
              h2(icon("globe"), " Sistema de descarga de indicadores"),
              p("Descargue datos macroeconómicos de múltiples fuentes internacionales para preparar dictámenes económicos.")
            ),
            div(
              class = "text-center",
              uiOutput("bandera_pais")
            )
          )
        ),
        
        # Panel de configuración
        card(
          class = "mb-4",
          card_header(
            class = "d-flex align-items-center",
            icon("sliders"), " ", "Configuración"
          ),
          card_body(
            layout_columns(
              col_widths = c(4, 2, 2, 4),
              
              div(
                selectInput(
                  "pais",
                  label = tags$span(icon("flag"), " País:"),
                  choices = NULL,
                  selected = NULL,
                  width = "100%"
                ),
                uiOutput("enlace_fmi_pais")
              ),
              
              numericInput(
                "fecha_inicio",
                label = tags$span(icon("calendar"), " Año inicio:"),
                value = lubridate::year(Sys.Date() %m-% months(6)) - 6,
                min = 1990,
                max = lubridate::year(Sys.Date()) + 5,
                step = 1,
                width = "100%"
              ),
              
              numericInput(
                "fecha_fin",
                label = tags$span(icon("calendar-check"), " Año fin:"),
                value = lubridate::year(Sys.Date() %m-% months(6)) + 2,
                min = 1990,
                max = lubridate::year(Sys.Date()) + 10,
                step = 1,
                width = "100%"
              ),
              
              div(
                tags$label(
                  class = "form-label",
                  icon("database"), " Fuentes de datos:"
                ),
                div(
                  class = "d-flex gap-2 mb-2",
                  actionButton("btn_seleccionar_todas", "Todas", class = "btn-sm btn-secondary"),
                  actionButton("btn_deseleccionar_todas", "Ninguna", class = "btn-sm btn-secondary")
                ),
                checkboxGroupInput(
                  "fuentes_datos",
                  label = NULL,
                  choices = c(
                    "FMI (WEO, FM, BOP, FSI)" = "fmi",
                    "Banco Mundial (WDI)" = "bm",
                    "Eurostat (UE)" = "eurostat",
                    "OMC" = "omc",
                    "BIS" = "bis"
                  ),
                  selected = c("fmi", "bm", "eurostat", "omc", "bis"),
                  inline = FALSE,
                  width = "100%"
                ),
                tags$small(
                  class = "text-muted",
                  "Solo se descargan indicadores del Excel para las fuentes seleccionadas."
                ),
                uiOutput("alerta_eurostat")
              )
            ),
            
            div(
              class = "text-center mt-3",
              actionButton(
                "btn_descargar",
                label = tagList(icon("cloud-download-alt"), " Descargar datos"),
                class = "btn-primary btn-lg px-5"
              )
            )
          )
        ),
        
        # Resumen de descarga
        uiOutput("resumen_descarga"),
        
        # Vista previa de datos (colapsable)
        bslib::accordion(
          id = "accordion_vista_previa",
          class = "mb-4",
          bslib::accordion_panel(
            title = tagList(icon("table"), " Vista previa de datos"),
            value = "vista_previa",
            icon = NULL,
            
            navset_tab(
              nav_panel("Sector real", DT::DTOutput("tabla_sector_real")),
              nav_panel("Mercado laboral", DT::DTOutput("tabla_mercado_laboral")),
              nav_panel("Sector exterior", 
                        div(
                          DT::DTOutput("tabla_sector_exterior"),
                          tags$p(
                            class = "text-muted small mt-2 fst-italic",
                            tags$strong("Nota:"), 
                            " Los tipos de cambio efectivos (TCER/TCEN) están denominados de tal manera que un aumento implica una apreciación. El tipo de cambio oficial frente al dólar está expresado en UML/USD, por lo que un aumento implica una depreciación de la moneda local frente al dólar estadounidense."
                          )
                        )
              ),
              nav_panel("Sector público", DT::DTOutput("tabla_sector_publico")),
              nav_panel("Indicadores monetarios y financieros", DT::DTOutput("tabla_precios_monetarios")),
              nav_panel("Pro-memoria", DT::DTOutput("tabla_promemoria"))
            )
          )
        ),
        
        # Botones de exportación
        layout_columns(
          col_widths = c(6, 6),
          class = "mb-4",
          
          shinyjs::disabled(
            downloadButton("btn_exportar_word",
                           label = span(class = "btn-text", icon("file-word"), " Exportar a Word"),
                           class = "btn-warning btn-lg w-100 py-3 btn-export"
            )
          ),
          
          shinyjs::disabled(
            downloadButton("btn_exportar_excel",
                           label = tagList(icon("file-excel"), " Exportar a Excel"),
                           class = "btn-success btn-lg w-100 py-3")
          )
        )
      )
    ),
    
    # Panel de indicadores - Muestra indicadores del Excel
    nav_panel(
      title = "Indicadores",
      icon = icon("list"),
      
      div(
        class = "container-fluid py-3",
        
        card(
          card_header(
            class = "bg-primary text-white",
            icon("database"), " Indicadores disponibles en el archivo Excel"
          ),
          card_body(
            p(class = "text-muted", 
              "Esta tabla muestra todos los indicadores configurados en el archivo ", 
              tags$code("Indicadores_Dictamen_Economico.xlsx"), 
              ". Solo se descargarán datos para los indicadores incluidos en este archivo."),
            
            # Filtros por fuente
            layout_columns(
              col_widths = c(3, 3, 3, 3),
              selectInput(
                "filtro_fuente_indicadores",
                "Filtrar por fuente:",
                choices = c("Todas" = "", "FMI", "BM", "Eurostat", "OMC", "BIS"),
                selected = ""
              ),
              selectInput(
                "filtro_seccion_indicadores", 
                "Filtrar por sección:",
                choices = c("Todas" = ""),
                selected = ""
              ),
              div(),
              div(
                class = "text-end pt-4",
                textOutput("resumen_indicadores_excel")
              )
            ),
            
            hr(),
            
            # Tabla de indicadores
            DT::DTOutput("tabla_indicadores_excel")
          )
        ),
        
        # Resumen por fuente (con enlaces a las fuentes)
        layout_columns(
          col_widths = c(2, 2, 2, 2, 2, 2),
          class = "mt-3",
          
          tags$a(
            href = "https://data.imf.org/", target = "_blank",
            style = "text-decoration: none;",
            value_box(
              title = "FMI",
              value = textOutput("n_fmi"),
              showcase = icon("university"),
              theme = "warning"
            )
          ),
          tags$a(
            href = "https://databank.worldbank.org/source/world-development-indicators", target = "_blank",
            style = "text-decoration: none;",
            value_box(
              title = "Banco Mundial",
              value = textOutput("n_bm"),
              showcase = icon("globe"),
              theme = "info"
            )
          ),
          tags$a(
            href = "https://ec.europa.eu/eurostat/data/database", target = "_blank",
            style = "text-decoration: none;",
            value_box(
              title = "Eurostat",
              value = textOutput("n_eurostat"),
              showcase = icon("flag"),
              theme = "primary"
            )
          ),
          tags$a(
            href = "https://stats.wto.org/", target = "_blank",
            style = "text-decoration: none;",
            value_box(
              title = "OMC",
              value = textOutput("n_omc"),
              showcase = icon("balance-scale"),
              theme = "success"
            )
          ),
          tags$a(
            href = "https://www.bis.org/statistics/index.htm", target = "_blank",
            style = "text-decoration: none;",
            value_box(
              title = "BIS",
              value = textOutput("n_bis"),
              showcase = icon("chart-line"),
              theme = "secondary"
            )
          ),
          value_box(
            title = "Total",
            value = textOutput("n_total"),
            showcase = icon("database"),
            theme = "dark"
          )
        )#,
        # value_box(
        #   title = "Total indicadores",
        #   value = textOutput("n_total"),
        #   showcase = icon("database"),
        #   theme = "dark"
        # )
      )
    ),
    
    # Panel de ayuda
    nav_spacer(),
    nav_panel(
      title = "Ayuda",
      icon = icon("question-circle"),
      
      div(
        class = "container py-4",
        
        card(
          card_header(icon("book"), " Guía de uso"),
          card_body(
            tags$ol(
              tags$li("Selecciona el país de interés en el menú desplegable."),
              tags$li("Define el período de análisis (año inicial y año final)."),
              tags$li("Marca las fuentes de datos que deseas utilizar."),
              tags$li("Haz clic en 'Descargar datos' para obtener la información."),
              tags$li("Revisa los datos en las diferentes pestañas organizadas por categoría."),
              tags$li("Exporta los resultados a Word o Excel según tus necesidades.")
            ),
            
            hr(),
            
            h5(icon("database"), " Fuentes de datos disponibles:"),
            
            tags$ul(
              tags$li(tags$strong("FMI:"), " World Economic Outlook (WEO), Fiscal Monitor (FM), Balance of Payments (BOP), Consumer Price Index (CPI)"),
              tags$li(tags$strong("Eurostat:"), " Datos estadísticos de la Unión Europea (solo países UE)"),
              tags$li(tags$strong("Banco Mundial:"), " World Development Indicators (WDI) - amplia cobertura de países"),
              tags$li(tags$strong("OMC:"), " Datos arancelarios de la Organización Mundial del Comercio"),
              tags$li(tags$strong("BIS:"), " Tipos de cambio efectivos del Bank for International Settlements")
            ),
            
            hr(),
            
            # NUEVA SECCIÓN: Notas para la preparación del examen
            h5(icon("file-word"), " Notas para la preparación del ejercicio con el documento Word:"),
            
            div(
              class = "alert alert-info",
              style = "background-color: #e8f4fc; border-color: #2B579A; border-left: 4px solid #2B579A; color: #000000;",
              
              tags$p(
                style = "color: #000000;",
                tags$strong(icon("exclamation-triangle"), " Limitaciones de la descarga automática:"),
                " Los datos se descargan de forma automática desde diversas fuentes internacionales. ",
                "Es imprescindible realizar una ", tags$strong("revisión exhaustiva"), 
                " de la coherencia de los datos antes de utilizar el documento para preparar el ejercicio.",
                " Algunas variables pueden requerir ajustes manuales, especialmente:"
              ),
              
              tags$ul(
                style = "color: #000000;",
                tags$li("Conversión de unidades (p. ej., expresar magnitudes en millones)."),
                tags$li("Verificación de la consistencia temporal de las series."),
                tags$li("Corrección de posibles valores atípicos o errores de fuente.")
              ),
              
              hr(),
              
              tags$p(
                style = "color: #000000;",
                tags$strong(icon("filter"), " Selección de variables:"),
                " El documento descargado incluye un amplio conjunto de indicadores. ",
                "Para preparar el ejercicio, es necesario ", tags$strong("seleccionar las variables más relevantes"),
                " para el análisis del país y eliminar aquellas que no aporten información significativa."
              ),
              
              hr(),
              
              tags$p(
                style = "color: #000000;",
                tags$strong(icon("exchange-alt"), " Tipos de cambio:"),
                " En el caso de las variables relativas al tipo de cambio, se debe revisar si ",
                "un ", tags$strong("aumento del índice representa una apreciación o depreciación"),
                " de la moneda local. Esto depende de cómo esté definido el indicador en cada fuente:"
              ),
              
              tags$ul(
                style = "color: #000000;",
                tags$li(tags$strong("Tipo de cambio nominal (UML/USD):"), " Un aumento indica depreciación de la moneda local."),
                tags$li(tags$strong("REER/NEER (índices):"), " Un aumento generalmente indica apreciación real/nominal.")
              ),
              
              hr(),
              
              tags$p(
                style = "color: #000000;",
                tags$strong(icon("chart-area"), " Complementar con gráficos del FMI:"),
                " Se recomienda encarecidamente consultar la ", 
                tags$strong("última consulta del Artículo IV del FMI"), 
                " para el país seleccionado. Estos informes contienen gráficos y análisis ",
                "que complementan las tablas de datos y facilitan la comprensión de la situación económica."
              ),
              
              tags$p(
                style = "margin-top: 0.5rem;",
                tags$a(
                  href = "https://www.imf.org/en/Publications/SPROLLs/Article-iv-staff-reports",
                  target = "_blank",
                  class = "btn btn-sm",
                  style = "background-color: transparent; color: #000000; border: 2px solid #5F2987; transition: all 0.3s ease;",
                  onmouseover = "this.style.backgroundColor='#5F2987'; this.style.color='#E2EFD9'; this.style.fontWeight='bold';",
                  onmouseout = "this.style.backgroundColor='transparent'; this.style.color='#000000'; this.style.fontWeight='normal';",
                  icon("external-link-alt"), " Consultas Artículo IV del FMI"
                )
              ),
              
              hr(),
              
              tags$p(
                style = "color: #000000;",
                tags$strong(icon("edit"), " Adaptar las preguntas:"),
                " Una vez seleccionados los datos y realizado un análisis preliminar de la situación económica del país, ",
                "es necesario ", tags$strong("adaptar las preguntas del ejercicio"), 
                " a las particularidades del país elegido, considerando:"
              ),
              
              tags$ul(
                style = "color: #000000;",
                tags$li("Los principales desequilibrios macroeconómicos identificados."),
                tags$li("El contexto económico y geopolítico relevante."),
                tags$li("Las recomendaciones de política económica más pertinentes.")
              )
            )
          )
        )
      )
    )
  )
  
  # ============================================================================
  # SERVIDOR
  # ============================================================================
  
  server <- function(input, output, session) {
    
    # Valores reactivos
    datos_descargados <- reactiveVal(NULL)
    datos_por_categoria <- reactiveVal(NULL)
    pais_seleccionado <- reactiveVal("")
    bandera_actual <- reactiveVal("")
    resumen_fuentes <- reactiveVal(NULL)
    es_pais_ue <- reactiveVal(TRUE)
    iso3_actual <- reactiveVal("")  # Código ISO3 del país actual
    
    # =========================================================================
    # PESTAÑA DE INDICADORES - Outputs para la tabla de indicadores del Excel
    # =========================================================================
    
    # Cargar indicadores del Excel al iniciar y crear reactivo
    indicadores_excel_rv <- reactive({
      cargar_indicadores_excel()
    })
    
    # Actualizar filtro de secciones cuando se cargan los indicadores
    observe({
      indicadores <- indicadores_excel_rv()
      if (!is.null(indicadores)) {
        secciones <- c("Todas" = "", sort(unique(indicadores$Seccion)))
        updateSelectInput(session, "filtro_seccion_indicadores", choices = secciones)
      }
    })
    
    # Tabla de indicadores del Excel
    output$tabla_indicadores_excel <- DT::renderDT({
      indicadores <- indicadores_excel_rv()
      if (is.null(indicadores)) return(NULL)
      
      # Aplicar filtros
      if (!is.null(input$filtro_fuente_indicadores) && input$filtro_fuente_indicadores != "") {
        indicadores <- indicadores |> dplyr::filter(Fuente == input$filtro_fuente_indicadores)
      }
      if (!is.null(input$filtro_seccion_indicadores) && input$filtro_seccion_indicadores != "") {
        indicadores <- indicadores |> dplyr::filter(Seccion == input$filtro_seccion_indicadores)
      }
      
      # Seleccionar columnas para mostrar
      tabla <- indicadores |>
        dplyr::select(
          Sección = Seccion,
          Subcategoría = Subcategoria,
          Código = Codigo,
          `Nombre corto` = Nombre_ES,
          Unidad,
          Fuente,
          `Base de datos` = Base_datos
        )
      
      DT::datatable(
        tabla,
        options = list(
          pageLength = 25,
          dom = 'frtip',
          language = list(
            search = "Buscar:",
            lengthMenu = "Mostrar _MENU_ indicadores",
            info = "Mostrando _START_ a _END_ de _TOTAL_ indicadores",
            paginate = list(previous = "Anterior", `next` = "Siguiente")
          ),
          scrollX = TRUE
        ),
        rownames = FALSE,
        filter = "top",
        class = "compact stripe hover"
      )
    })
    
    # Resumen de indicadores
    output$resumen_indicadores_excel <- renderText({
      indicadores <- indicadores_excel_rv()
      if (is.null(indicadores)) return("No se pudo cargar el archivo Excel")
      paste0(nrow(indicadores), " indicadores configurados")
    })
    
    # Contadores por fuente
    output$n_fmi <- renderText({
      indicadores <- indicadores_excel_rv()
      if (is.null(indicadores)) return("0")
      sum(indicadores$Fuente == "FMI", na.rm = TRUE)
    })
    
    output$n_bm <- renderText({
      indicadores <- indicadores_excel_rv()
      if (is.null(indicadores)) return("0")
      sum(indicadores$Fuente == "BM", na.rm = TRUE)
    })
    
    output$n_eurostat <- renderText({
      indicadores <- indicadores_excel_rv()
      if (is.null(indicadores)) return("0")
      sum(indicadores$Fuente == "Eurostat", na.rm = TRUE)
    })
    
    output$n_omc <- renderText({
      indicadores <- indicadores_excel_rv()
      if (is.null(indicadores)) return("0")
      sum(indicadores$Fuente == "OMC", na.rm = TRUE)
    })
    
    output$n_bis <- renderText({
      indicadores <- indicadores_excel_rv()
      if (is.null(indicadores)) return("0")
      sum(indicadores$Fuente == "BIS", na.rm = TRUE)
    })
    
    output$n_total <- renderText({
      indicadores <- indicadores_excel_rv()
      if (is.null(indicadores)) return("0")
      nrow(indicadores)
    })
    
    # =========================================================================
    # FIN PESTAÑA DE INDICADORES
    # =========================================================================
    
    # Cargar lista de países al iniciar
    observe({
      paises <- obtener_lista_paises()
      paises_lista <- setNames(paises$iso2c, paises$country_es)
      
      updateSelectInput(
        session,
        "pais",
        choices = paises_lista,
        selected = "ES"
      )
    })
    
    # Habilitar/deshabilitar botones según haya datos
    observe({
      if (is.null(datos_por_categoria()) || length(datos_por_categoria()) == 0) {
        shinyjs::disable("btn_exportar_word")
        shinyjs::disable("btn_exportar_excel")
      } else {
        shinyjs::enable("btn_exportar_word")
        shinyjs::enable("btn_exportar_excel")
      }
    })
    
    # Observar cambios en el país seleccionado y actualizar la bandera
    observeEvent(input$pais, {
      req(input$pais)
      iso2 <- input$pais
      bandera_actual(paste0("https://flagcdn.com/w160/", tolower(iso2), ".png"))
      
      # Obtener y guardar código ISO3
      pais_iso3 <- if (iso2 %in% names(mapeo_iso2_iso3)) {
        mapeo_iso2_iso3[iso2]
      } else {
        tryCatch(countrycode::countrycode(iso2, "iso2c", "iso3c"), error = function(e) NULL)
      }
      iso3_actual(pais_iso3)
      
      # Verificar si el país es de la UE para Eurostat
      pais_en_ue <- iso2 %in% paises_ue
      es_pais_ue(pais_en_ue)
      
      if (!pais_en_ue) {
        # Deseleccionar Eurostat
        fuentes_actuales <- input$fuentes_datos
        fuentes_sin_eurostat <- setdiff(fuentes_actuales, "eurostat")
        updateCheckboxGroupInput(session, "fuentes_datos", selected = fuentes_sin_eurostat)
        # Deshabilitar con un pequeño delay
        shinyjs::delay(100, shinyjs::disable(selector = "input[value='eurostat']"))
      } else {
        # Habilitar Eurostat
        shinyjs::enable(selector = "input[value='eurostat']")
        fuentes_actuales <- input$fuentes_datos
        if (!("eurostat" %in% fuentes_actuales)) {
          updateCheckboxGroupInput(session, "fuentes_datos", selected = c(fuentes_actuales, "eurostat"))
        }
      }
    })
    
    # Alerta de Eurostat
    output$alerta_eurostat <- renderUI({
      if (!es_pais_ue()) {
        div(
          class = "alert alert-warning mt-2 py-1",
          style = "font-size: 0.85rem;",
          icon("info-circle"), " Eurostat solo disponible para países de la UE"
        )
      }
    })
    
    # Botones seleccionar/deseleccionar todas
    observeEvent(input$btn_seleccionar_todas, {
      fuentes_base <- c("fmi", "bm", "omc", "bis")
      if (es_pais_ue()) {
        fuentes_base <- c(fuentes_base, "eurostat")
      }
      updateCheckboxGroupInput(session, "fuentes_datos", selected = fuentes_base)
    })
    
    observeEvent(input$btn_deseleccionar_todas, {
      updateCheckboxGroupInput(session, "fuentes_datos", selected = character(0))
    })
    
    # Renderizar la bandera como enlace a la página del país en el FMI
    output$bandera_pais <- renderUI({
      req(bandera_actual())
      
      iso3 <- iso3_actual()
      
      # Si no hay ISO3, mostrar solo la imagen sin enlace
      if (is.null(iso3) || iso3 == "") {
        return(
          tags$img(
            src = bandera_actual(),
            height = "80px",
            style = "border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.2);"
          )
        )
      }
      
      # URL de la página del país en el FMI
      url_destino <- paste0("https://www.imf.org/en/countries/", tolower(iso3))
      
      tags$a(
        href = url_destino,
        target = "_blank",
        title = "Abrir página del país en el FMI",
        style = "cursor: pointer; display: inline-block; transition: transform 0.2s ease;",
        onmouseover = "this.style.transform='scale(1.05)';",
        onmouseout = "this.style.transform='scale(1)';",
        tags$img(
          src = bandera_actual(),
          height = "80px",
          style = "border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.2);"
        )
      )
    })
    
    # Renderizar enlace a la página del país en el FMI
    output$enlace_fmi_pais <- renderUI({
      req(input$pais)
      iso2 <- input$pais
      
      # Obtener código ISO3 en minúsculas
      paises <- obtener_lista_paises()
      match_idx <- which(paises$iso2c == iso2)
      
      if (length(match_idx) > 0) {
        iso3 <- tolower(paises$iso3c[match_idx])
      } else if (iso2 %in% names(mapeo_iso2_iso3)) {
        iso3 <- tolower(mapeo_iso2_iso3[iso2])
      } else {
        iso3 <- tryCatch(
          tolower(countrycode::countrycode(iso2, "iso2c", "iso3c")),
          error = function(e) NULL
        )
      }
      
      if (!is.null(iso3) && nchar(iso3) == 3) {
        url_fmi <- paste0("https://www.imf.org/en/countries/", iso3)
        tags$a(
          href = url_fmi,
          target = "_blank",
          class = "small",
          style = "color: #5F2987; text-decoration: none; display: inline-block; margin-top: 5px;",
          onmouseover = "this.style.color='#5F2987'; this.style.textDecoration='underline';",
          onmouseout = "this.style.color='#5F2987'; this.style.textDecoration='none';",
          icon("external-link-alt"), " Ver país en el FMI"
        )
      }
    })
    
    # Resumen de descarga
    output$resumen_descarga <- renderUI({
      req(resumen_fuentes(), pais_seleccionado(), datos_descargados())
      resumen <- resumen_fuentes()
      datos <- datos_descargados()
      
      if (length(resumen) == 0) return(NULL)
      
      total <- sum(unlist(resumen))
      # Formatear el total con separador de miles español
      total_formateado <- format(total, big.mark = ".", decimal.mark = ",")
      
      # Contar indicadores únicos
      n_indicadores <- if (!is.null(datos) && nrow(datos) > 0) {
        length(unique(datos$indicador_nombre))
      } else {
        0
      }
      
      # Obtener nombre del país y años
      nombre_pais <- pais_seleccionado()
      anio_inicio <- input$fecha_inicio
      anio_fin <- input$fecha_fin
      
      div(
        class = "resumen-fuentes mb-4",
        h5(
          icon("check-circle", class = "text-success"), 
          paste0(" Datos descargados para ", nombre_pais, " (", anio_inicio, "-", anio_fin, "): ", 
                 total_formateado, " registros de ", n_indicadores, " indicadores")
        ),
        div(
          class = "d-flex flex-wrap gap-2 mt-2",
          lapply(names(resumen), function(fuente) {
            registros_fuente <- format(resumen[[fuente]], big.mark = ".", decimal.mark = ",")
            tags$span(
              class = "badge bg-primary",
              fuente, ": ", registros_fuente
            )
          })
        )
      )
    })
    
    # Botón de descarga
    observeEvent(input$btn_descargar, {
      shinyjs::disable("btn_descargar")
      on.exit({ shinyjs::enable("btn_descargar") })
      
      req(input$pais, input$fecha_inicio, input$fecha_fin)
      
      # Validación de fechas
      if (input$fecha_inicio >= input$fecha_fin) {
        showNotification(
          "El año de inicio debe ser anterior al año fin",
          type = "error",
          duration = 5
        )
        return()
      }
      
      if (input$fecha_fin - input$fecha_inicio > 50) {
        showNotification(
          "El rango máximo es de 50 años",
          type = "warning",
          duration = 5
        )
        return()
      }
      
      if (length(input$fuentes_datos) == 0) {
        showNotification(
          "Debe seleccionar al menos una fuente de datos",
          type = "error",
          duration = 5
        )
        return()
      }
      
      withProgress(message = 'Descargando datos...', value = 0, {
        
        paises <- obtener_lista_paises()
        nombre_pais <- paises$country_es[paises$iso2c == input$pais]
        pais_seleccionado(nombre_pais)
        
        incProgress(0.05, detail = "Preparando conexiones...")
        
        usar_bm <- "bm" %in% input$fuentes_datos
        usar_fmi <- "fmi" %in% input$fuentes_datos
        usar_omc <- "omc" %in% input$fuentes_datos
        usar_bis <- "bis" %in% input$fuentes_datos
        usar_eurostat <- "eurostat" %in% input$fuentes_datos && es_pais_ue()
        
        # Contar fuentes activas para calcular incrementos
        n_fuentes <- sum(c(usar_fmi, usar_eurostat, usar_bm, usar_omc, usar_bis))
        incremento_por_fuente <- if (n_fuentes > 0) 0.70 / n_fuentes else 0
        progreso_acumulado <- 0.05
        
        # Función callback para actualizar el progreso
        actualizar_progreso <- function(valor, detalle) {
          # Incrementar progreso según la fuente que se descarga
          progreso_acumulado <<- progreso_acumulado + incremento_por_fuente
          incProgress(incremento_por_fuente, detail = detalle)
        }
        
        resultado <- descargar_datos_combinados(
          input$pais,
          as.Date(paste0(input$fecha_inicio, "-01-01")),
          as.Date(paste0(input$fecha_fin, "-12-31")),
          usar_bm = usar_bm,
          usar_fmi = usar_fmi,
          usar_omc = usar_omc,
          usar_bis = usar_bis,
          usar_eurostat = usar_eurostat,
          actualizar_progreso = actualizar_progreso
        )
        
        # Asegurar que llegamos a 0.75 (3/4) después de las fuentes
        progreso_restante <- 0.75 - progreso_acumulado
        if (progreso_restante > 0) {
          incProgress(progreso_restante, detail = "Procesando datos...")
        }
        
        if (is.null(resultado$datos) || nrow(resultado$datos) == 0) {
          showNotification(
            "No se pudieron descargar datos. Verifique su conexión a Internet e intente de nuevo.",
            type = "error",
            duration = 8
          )
          return()
        }
        
        datos_descargados(resultado$datos)
        resumen_fuentes(resultado$resumen)
        
        incProgress(0.15, detail = "Organizando por categorías...")
        
        datos_cat <- organizar_por_categoria(resultado$datos, input$fecha_inicio, input$fecha_fin)
        datos_por_categoria(datos_cat)
        
        incProgress(0.10, detail = "¡Completado!")
      })
      
      showNotification(
        tagList(icon("check"), " Datos descargados correctamente"),
        type = "message",
        duration = 3
      )
    })
    
    # Función auxiliar para renderizar tablas DT (con columna de fuente)
    renderizar_tabla <- function(datos) {
      if (is.null(datos) || nrow(datos) == 0) {
        return(NULL)
      }
      
      datos_df <- as.data.frame(datos)
      
      if ("indicador_nombre" %in% names(datos_df) && "unidad_larga" %in% names(datos_df)) {
        datos_df$Indicador <- ifelse(
          !is.na(datos_df$unidad_larga) & datos_df$unidad_larga != "",
          paste0(datos_df$indicador_nombre, " (", datos_df$unidad_larga, ")"),
          datos_df$indicador_nombre
        )
        cols_a_eliminar <- intersect(names(datos_df), c("indicador_nombre", "unidad_corta", "unidad_larga"))
        datos_df <- datos_df |>
          dplyr::select(-dplyr::all_of(cols_a_eliminar))
        
        # Incluir columna de fuente en la tabla
        if ("fuente" %in% names(datos_df)) {
          datos_df <- datos_df |>
            dplyr::rename(Fuente = fuente) |>
            dplyr::select(Indicador, Fuente, dplyr::everything())
        } else {
          datos_df <- datos_df |>
            dplyr::select(Indicador, dplyr::everything())
        }
      }
      
      cols_numericas <- which(sapply(datos_df, is.numeric))
      
      # Formatear números con formato español
      for (col in cols_numericas) {
        datos_df[[col]] <- sapply(datos_df[[col]], function(x) {
          formatear_numero_es(x, 2)
        })
      }
      
      DT::datatable(
        datos_df,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = 'Bfrtip',
          language = list(
            url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'
          )
        ),
        rownames = FALSE,
        class = "table table-hover"
      )
    }
    
    # Tablas de vista previa
    output$tabla_sector_real <- DT::renderDT({
      req(datos_por_categoria())
      if ("sector_real" %in% names(datos_por_categoria())) {
        renderizar_tabla(datos_por_categoria()$sector_real)
      }
    })
    
    output$tabla_mercado_laboral <- DT::renderDT({
      req(datos_por_categoria())
      if ("mercado_laboral" %in% names(datos_por_categoria())) {
        renderizar_tabla(datos_por_categoria()$mercado_laboral)
      }
    })
    
    output$tabla_sector_exterior <- DT::renderDT({
      req(datos_por_categoria())
      if ("sector_exterior" %in% names(datos_por_categoria())) {
        renderizar_tabla(datos_por_categoria()$sector_exterior)
      }
    })
    
    output$tabla_sector_publico <- DT::renderDT({
      req(datos_por_categoria())
      if ("sector_publico" %in% names(datos_por_categoria())) {
        renderizar_tabla(datos_por_categoria()$sector_publico)
      }
    })
    
    output$tabla_precios_monetarios <- DT::renderDT({
      req(datos_por_categoria())
      if ("precios_monetarios" %in% names(datos_por_categoria())) {
        renderizar_tabla(datos_por_categoria()$precios_monetarios)
      }
    })
    
    output$tabla_promemoria <- DT::renderDT({
      req(datos_por_categoria())
      if ("pro_memoria" %in% names(datos_por_categoria())) {
        renderizar_tabla(datos_por_categoria()$pro_memoria)
      }
    })
    
    # Exportar a Word
    output$btn_exportar_word <- downloadHandler(
      filename = function() {
        paste0("Dictamen_", input$pais, "_", format(Sys.Date(), "%Y%m%d"), ".docx")
      },
      content = function(file) {
        shinyjs::disable("btn_exportar_word")
        on.exit({ shinyjs::enable("btn_exportar_word") })
        
        req(datos_por_categoria(), pais_seleccionado())
        
        # Crear archivo en directorio de salida
        archivo_destino <- file.path(output_dir, basename(file))
        
        # Ruta a la plantilla (ajustar según tu estructura)
        plantilla_path <- NULL
        if (file.exists("templates/Plantilla_Ejercicios_Dictamen.dotx")) {
          plantilla_path <- "templates/Plantilla_Ejercicios_Dictamen.dotx"
        } else if (file.exists("inst/templates/Plantilla_Ejercicios_Dictamen.dotx")) {
          # Si es un paquete R
          plantilla_path <- "inst/templates/Plantilla_Ejercicios_Dictamen.dotx"
        }
        
        tryCatch({
          exportar_a_word(
            datos_por_categoria(),
            pais_seleccionado(),
            as.Date(paste0(input$fecha_inicio, "-01-01")),
            as.Date(paste0(input$fecha_fin, "-12-31")),
            archivo_destino,
            plantilla_word = plantilla_path  # <-- NUEVO PARÁMETRO
          )
          
          # Copiar al archivo temporal para descarga
          file.copy(archivo_destino, file)
          
          showNotification(
            tagList(icon("check"), " Documento Word guardado en ", output_dir),
            type = "message",
            duration = 3
          )
        }, error = function(e) {
          showNotification(
            paste("Error al exportar:", e$message),
            type = "error",
            duration = 5
          )
        })
      }
    )
    
    # Exportar a Excel
    output$btn_exportar_excel <- downloadHandler(
      filename = function() {
        paste0("Dictamen_", input$pais, "_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
      },
      content = function(file) {
        shinyjs::disable("btn_exportar_excel")
        on.exit({ shinyjs::enable("btn_exportar_excel") })
        
        req(datos_por_categoria(), datos_descargados(), pais_seleccionado())
        
        # Crear archivo en directorio de salida
        archivo_destino <- file.path(output_dir, basename(file))
        
        tryCatch({
          exportar_a_excel(
            datos_por_categoria(),
            datos_descargados(),
            pais_seleccionado(),
            input$pais,
            as.Date(paste0(input$fecha_inicio, "-01-01")),
            as.Date(paste0(input$fecha_fin, "-12-31")),
            archivo_destino
          )
          
          # Copiar al archivo temporal para descarga
          file.copy(archivo_destino, file)
          
          showNotification(
            tagList(icon("check"), " Archivo Excel guardado en ", output_dir),
            type = "message",
            duration = 3
          )
        }, error = function(e) {
          showNotification(
            paste("Error al exportar:", e$message),
            type = "error",
            duration = 5
          )
        })
      }
    )
  }
  
  # ============================================================================
  # EJECUTAR APLICACIÓN
  # ============================================================================
  
  shinyApp(ui = ui, server = server, ...)
}