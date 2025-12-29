# ============================================================================
# APLICACIÓN SHINY PARA DESCARGA DE DATOS MACROECONÓMICOS
# Preparación de Dictámenes Económicos
# ============================================================================
# 
# Esta aplicación permite descargar datos macroeconómicos de países,
# organizados en las 6 categorías estándar: Sector real, Sector exterior, 
# Sector público, Precios y costes, Indicadores monetarios y financieros,
# y Pro-memoria.
#
# Fecha: Diciembre 2025
# ============================================================================

# INSTALACIÓN DE PAQUETES (ejecutar solo la primera vez)
# ============================================================================
paquetes_necesarios <- c(
  "shiny",
  "shinydashboard",
  "WDI",           # Banco Mundial
  "dplyr",
  "tidyr",
  "officer",       # Para documentos Word
  "openxlsx",      # Para archivos Excel
  "DT",
  "zoo",
  "lubridate"
)

# Instalar paquetes que falten
paquetes_faltantes <- paquetes_necesarios[!(paquetes_necesarios %in% installed.packages()[,"Package"])]
if(length(paquetes_faltantes) > 0) {
  install.packages(paquetes_faltantes, repos = "http://cran.rstudio.com/")
}

# Cargar librerías
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(WDI)
  library(dplyr)
  library(tidyr)
  library(officer)
  library(openxlsx)
  library(DT)
  library(zoo)
  library(lubridate)
})

# ============================================================================
# MAPEO DE PAÍSES A ESPAÑOL
# ============================================================================

# Diccionario de nombres de países en español (los más comunes)
diccionario_paises_es <- c(
  "Afghanistan" = "Afganistán",
  "Albania" = "Albania",
  "Algeria" = "Argelia",
  "Andorra" = "Andorra",
  "Angola" = "Angola",
  "Argentina" = "Argentina",
  "Armenia" = "Armenia",
  "Australia" = "Australia",
  "Austria" = "Austria",
  "Azerbaijan" = "Azerbaiyán",
  "Bahamas, The" = "Bahamas",
  "Bahrain" = "Baréin",
  "Bangladesh" = "Bangladés",
  "Barbados" = "Barbados",
  "Belarus" = "Bielorrusia",
  "Belgium" = "Bélgica",
  "Belize" = "Belice",
  "Benin" = "Benín",
  "Bhutan" = "Bután",
  "Bolivia" = "Bolivia",
  "Bosnia and Herzegovina" = "Bosnia y Herzegovina",
  "Botswana" = "Botsuana",
  "Brazil" = "Brasil",
  "Brunei Darussalam" = "Brunéi",
  "Bulgaria" = "Bulgaria",
  "Burkina Faso" = "Burkina Faso",
  "Burundi" = "Burundi",
  "Cabo Verde" = "Cabo Verde",
  "Cambodia" = "Camboya",
  "Cameroon" = "Camerún",
  "Canada" = "Canadá",
  "Central African Republic" = "República Centroafricana",
  "Chad" = "Chad",
  "Chile" = "Chile",
  "China" = "China",
  "Colombia" = "Colombia",
  "Comoros" = "Comoras",
  "Congo, Dem. Rep." = "República Democrática del Congo",
  "Congo, Rep." = "República del Congo",
  "Costa Rica" = "Costa Rica",
  "Cote d'Ivoire" = "Costa de Marfil",
  "Croatia" = "Croacia",
  "Cuba" = "Cuba",
  "Cyprus" = "Chipre",
  "Czech Republic" = "República Checa",
  "Czechia" = "Chequia",
  "Denmark" = "Dinamarca",
  "Djibouti" = "Yibuti",
  "Dominica" = "Dominica",
  "Dominican Republic" = "República Dominicana",
  "Ecuador" = "Ecuador",
  "Egypt, Arab Rep." = "Egipto",
  "El Salvador" = "El Salvador",
  "Equatorial Guinea" = "Guinea Ecuatorial",
  "Eritrea" = "Eritrea",
  "Estonia" = "Estonia",
  "Eswatini" = "Esuatini",
  "Ethiopia" = "Etiopía",
  "Fiji" = "Fiyi",
  "Finland" = "Finlandia",
  "France" = "Francia",
  "Gabon" = "Gabón",
  "Gambia, The" = "Gambia",
  "Georgia" = "Georgia",
  "Germany" = "Alemania",
  "Ghana" = "Ghana",
  "Greece" = "Grecia",
  "Grenada" = "Granada",
  "Guatemala" = "Guatemala",
  "Guinea" = "Guinea",
  "Guinea-Bissau" = "Guinea-Bisáu",
  "Guyana" = "Guyana",
  "Haiti" = "Haití",
  "Honduras" = "Honduras",
  "Hong Kong SAR, China" = "Hong Kong",
  "Hungary" = "Hungría",
  "Iceland" = "Islandia",
  "India" = "India",
  "Indonesia" = "Indonesia",
  "Iran, Islamic Rep." = "Irán",
  "Iraq" = "Irak",
  "Ireland" = "Irlanda",
  "Israel" = "Israel",
  "Italy" = "Italia",
  "Jamaica" = "Jamaica",
  "Japan" = "Japón",
  "Jordan" = "Jordania",
  "Kazakhstan" = "Kazajistán",
  "Kenya" = "Kenia",
  "Kiribati" = "Kiribati",
  "Korea, Dem. People's Rep." = "Corea del Norte",
  "Korea, Rep." = "Corea del Sur",
  "Kosovo" = "Kosovo",
  "Kuwait" = "Kuwait",
  "Kyrgyz Republic" = "Kirguistán",
  "Lao PDR" = "Laos",
  "Latvia" = "Letonia",
  "Lebanon" = "Líbano",
  "Lesotho" = "Lesoto",
  "Liberia" = "Liberia",
  "Libya" = "Libia",
  "Liechtenstein" = "Liechtenstein",
  "Lithuania" = "Lituania",
  "Luxembourg" = "Luxemburgo",
  "Macao SAR, China" = "Macao",
  "Madagascar" = "Madagascar",
  "Malawi" = "Malaui",
  "Malaysia" = "Malasia",
  "Maldives" = "Maldivas",
  "Mali" = "Malí",
  "Malta" = "Malta",
  "Marshall Islands" = "Islas Marshall",
  "Mauritania" = "Mauritania",
  "Mauritius" = "Mauricio",
  "Mexico" = "México",
  "Micronesia, Fed. Sts." = "Micronesia",
  "Moldova" = "Moldavia",
  "Monaco" = "Mónaco",
  "Mongolia" = "Mongolia",
  "Montenegro" = "Montenegro",
  "Morocco" = "Marruecos",
  "Mozambique" = "Mozambique",
  "Myanmar" = "Birmania",
  "Namibia" = "Namibia",
  "Nauru" = "Nauru",
  "Nepal" = "Nepal",
  "Netherlands" = "Países Bajos",
  "New Zealand" = "Nueva Zelanda",
  "Nicaragua" = "Nicaragua",
  "Niger" = "Níger",
  "Nigeria" = "Nigeria",
  "North Macedonia" = "Macedonia del Norte",
  "Norway" = "Noruega",
  "Oman" = "Omán",
  "Pakistan" = "Pakistán",
  "Palau" = "Palaos",
  "Panama" = "Panamá",
  "Papua New Guinea" = "Papúa Nueva Guinea",
  "Paraguay" = "Paraguay",
  "Peru" = "Perú",
  "Philippines" = "Filipinas",
  "Poland" = "Polonia",
  "Portugal" = "Portugal",
  "Puerto Rico" = "Puerto Rico",
  "Qatar" = "Catar",
  "Romania" = "Rumanía",
  "Russian Federation" = "Rusia",
  "Rwanda" = "Ruanda",
  "Samoa" = "Samoa",
  "San Marino" = "San Marino",
  "Sao Tome and Principe" = "Santo Tomé y Príncipe",
  "Saudi Arabia" = "Arabia Saudita",
  "Senegal" = "Senegal",
  "Serbia" = "Serbia",
  "Seychelles" = "Seychelles",
  "Sierra Leone" = "Sierra Leona",
  "Singapore" = "Singapur",
  "Slovak Republic" = "Eslovaquia",
  "Slovenia" = "Eslovenia",
  "Solomon Islands" = "Islas Salomón",
  "Somalia" = "Somalia",
  "South Africa" = "Sudáfrica",
  "South Sudan" = "Sudán del Sur",
  "Spain" = "España",
  "Sri Lanka" = "Sri Lanka",
  "St. Kitts and Nevis" = "San Cristóbal y Nieves",
  "St. Lucia" = "Santa Lucía",
  "St. Vincent and the Grenadines" = "San Vicente y las Granadinas",
  "Sudan" = "Sudán",
  "Suriname" = "Surinam",
  "Sweden" = "Suecia",
  "Switzerland" = "Suiza",
  "Syrian Arab Republic" = "Siria",
  "Tajikistan" = "Tayikistán",
  "Tanzania" = "Tanzania",
  "Thailand" = "Tailandia",
  "Timor-Leste" = "Timor Oriental",
  "Togo" = "Togo",
  "Tonga" = "Tonga",
  "Trinidad and Tobago" = "Trinidad y Tobago",
  "Tunisia" = "Túnez",
  "Turkey" = "Turquía",
  "Turkmenistan" = "Turkmenistán",
  "Tuvalu" = "Tuvalu",
  "Uganda" = "Uganda",
  "Ukraine" = "Ucrania",
  "United Arab Emirates" = "Emiratos Árabes Unidos",
  "United Kingdom" = "Reino Unido",
  "United States" = "Estados Unidos",
  "Uruguay" = "Uruguay",
  "Uzbekistan" = "Uzbekistán",
  "Vanuatu" = "Vanuatu",
  "Venezuela, RB" = "Venezuela",
  "Vietnam" = "Vietnam",
  "West Bank and Gaza" = "Cisjordania y Gaza",
  "Yemen, Rep." = "Yemen",
  "Zambia" = "Zambia",
  "Zimbabwe" = "Zimbabue"
)

# ============================================================================
# MAPEO DE INDICADORES POR CATEGORÍA (CÓDIGOS CORRECTOS DEL BANCO MUNDIAL)
# ============================================================================

indicadores_banco_mundial <- list(
  sector_real = c(
    "NY.GDP.MKTP.KD.ZG" = "PIB real (variación %)",
    "NY.GDP.MKTP.CD" = "PIB nominal (USD corrientes)",
    "NY.GDP.PCAP.CD" = "PIB per cápita (USD)",
    "NE.CON.PRVT.ZS" = "Consumo privado (% PIB)",
    "NE.CON.GOVT.ZS" = "Consumo público (% PIB)",
    "NE.GDI.TOTL.ZS" = "Formación bruta capital (% PIB)",
    "NV.AGR.TOTL.ZS" = "Agricultura, valor añadido (% PIB)",
    "NV.IND.TOTL.ZS" = "Industria, valor añadido (% PIB)",
    "NV.SRV.TOTL.ZS" = "Servicios, valor añadido (% PIB)",
    "SL.UEM.TOTL.ZS" = "Desempleo (% población activa)",
    "SL.TLF.CACT.ZS" = "Población activa (% población total)"
  ),
  
  sector_exterior = c(
    "BN.CAB.XOKA.GD.ZS" = "Cuenta corriente (% PIB)",
    "BX.KLT.DINV.WD.GD.ZS" = "IED neta (% PIB)",
    "NE.EXP.GNFS.ZS" = "Exportaciones bienes y servicios (% PIB)",
    "NE.IMP.GNFS.ZS" = "Importaciones bienes y servicios (% PIB)",
    "DT.DOD.DECT.GN.ZS" = "Deuda externa (% INB)",
    "FI.RES.TOTL.CD" = "Reservas internacionales (USD)",
    "PX.REX.REER" = "Tipo cambio efectivo real (índice)"
  ),
  
  sector_publico = c(
    "GC.BAL.CASH.GD.ZS" = "Saldo efectivo gobierno (% PIB)",
    "GC.DOD.TOTL.GD.ZS" = "Deuda pública (% PIB)",
    "GC.REV.XGRT.GD.ZS" = "Ingresos públicos (% PIB)",
    "GC.XPN.TOTL.GD.ZS" = "Gasto público (% PIB)",
    "GC.TAX.TOTL.GD.ZS" = "Recaudación tributaria (% PIB)"
  ),
  
  precios_costes = c(
    "FP.CPI.TOTL.ZG" = "IPC (variación anual %)",
    "NY.GDP.DEFL.KD.ZG" = "Deflactor PIB (variación anual %)",
    "SL.EMP.TOTL.SP.ZS" = "Ratio empleo-población (%)"
  ),
  
  indicadores_monetarios = c(
    "FM.LBL.BMNY.GD.ZS" = "Dinero en sentido amplio (% PIB)",
    "FR.INR.RINR" = "Tipo de interés real (%)",
    "FR.INR.LEND" = "Tipo de interés préstamos (%)",
    "FS.AST.PRVT.GD.ZS" = "Crédito interno sector privado (% PIB)",
    "FB.BNK.CAPA.ZS" = "Ratio de adecuación capital bancario (%)"
  ),
  
  pro_memoria = c(
    "SP.POP.TOTL" = "Población total",
    "SP.URB.TOTL.IN.ZS" = "Población urbana (% total)",
    "SE.ADT.LITR.ZS" = "Tasa alfabetización adultos (%)",
    "SI.POV.GINI" = "Índice Gini",
    "EN.ATM.CO2E.PC" = "Emisiones CO2 per cápita (ton métricas)"
  )
)

# ============================================================================
# FUNCIONES AUXILIARES
# ============================================================================

# Función para obtener lista de países en español
obtener_lista_paises <- function() {
  paises <- WDI::WDI_data$country
  
  paises |>
    dplyr::filter(region != "Aggregates") |>
    dplyr::select(iso2c, country) |>
    dplyr::mutate(
      # Traducir usando el diccionario, si no existe mantener el nombre original
      country_es = ifelse(
        country %in% names(diccionario_paises_es),
        diccionario_paises_es[country],
        country
      )
    ) |>
    dplyr::select(iso2c, country_es) |>
    dplyr::arrange(country_es)
}

# Función para descargar datos del Banco Mundial
descargar_datos_bm <- function(pais_codigo, fecha_inicio, fecha_fin) {
  
  # Obtener SOLO los códigos de los indicadores
  codigos_indicadores <- names(unlist(indicadores_banco_mundial))
  
  tryCatch({
    datos <- WDI::WDI(
      country = pais_codigo,
      indicator = codigos_indicadores,
      start = lubridate::year(fecha_inicio),
      end = lubridate::year(fecha_fin),
      extra = TRUE
    )
    
    if (is.null(datos) || nrow(datos) == 0) {
      return(NULL)
    }
    
    # Crear mapeo de código a descripción
    mapeo_descripciones <- unlist(indicadores_banco_mundial)
    
    # Limpiar y transformar datos
    datos |>
      dplyr::select(country, year, iso2c, dplyr::all_of(codigos_indicadores)) |>
      tidyr::pivot_longer(
        cols = dplyr::all_of(codigos_indicadores),
        names_to = "indicador_codigo",
        values_to = "valor"
      ) |>
      dplyr::mutate(
        indicador_nombre = mapeo_descripciones[indicador_codigo],
        fuente = "Banco Mundial"
      ) |>
      dplyr::filter(!is.na(valor))
    
  }, error = function(e) {
    message("Error al descargar datos: ", e$message)
    return(NULL)
  })
}

# Función para organizar datos por categoría
organizar_por_categoria <- function(datos) {
  
  if (is.null(datos) || nrow(datos) == 0) {
    return(list())
  }
  
  # Mapear cada indicador a su categoría
  mapeo_categorias <- list()
  
  for (cat_nombre in names(indicadores_banco_mundial)) {
    indicadores_cat <- names(indicadores_banco_mundial[[cat_nombre]])
    mapeo_categorias[[cat_nombre]] <- indicadores_cat
  }
  
  # Crear lista de datasets por categoría
  datos_por_categoria <- list()
  
  for (cat_nombre in names(mapeo_categorias)) {
    datos_cat <- datos |>
      dplyr::filter(indicador_codigo %in% mapeo_categorias[[cat_nombre]]) |>
      dplyr::select(year, indicador_nombre, valor) |>
      tidyr::pivot_wider(
        names_from = year,
        values_from = valor,
        names_sort = TRUE
      )
    
    if (nrow(datos_cat) > 0) {
      datos_por_categoria[[cat_nombre]] <- datos_cat
    }
  }
  
  return(datos_por_categoria)
}

# Función para exportar a Word
exportar_a_word <- function(datos_por_categoria, pais_nombre, fecha_inicio, fecha_fin, archivo_salida) {
  
  # Crear documento
  doc <- officer::read_docx()
  
  # Título principal
  doc <- doc |>
    officer::body_add_par(
      paste0("INDICADORES ECONÓMICOS Y SOCIALES - ", toupper(pais_nombre)),
      style = "heading 1"
    ) |>
    officer::body_add_par(
      paste0("Período: ", format(fecha_inicio, "%Y"), " - ", format(fecha_fin, "%Y")),
      style = "Normal"
    ) |>
    officer::body_add_par("", style = "Normal") |>
    officer::body_add_par(
      paste0("Fecha de generación: ", format(Sys.Date(), "%d/%m/%Y")),
      style = "Normal"
    ) |>
    officer::body_add_par("Fuente: Banco Mundial (World Development Indicators)", style = "Normal") |>
    officer::body_add_par("", style = "Normal")
  
  # Nombres de categorías para los títulos
  nombres_categorias <- c(
    "sector_real" = "SECTOR REAL",
    "sector_exterior" = "SECTOR EXTERIOR",
    "sector_publico" = "SECTOR PÚBLICO",
    "precios_costes" = "PRECIOS Y COSTES",
    "indicadores_monetarios" = "INDICADORES MONETARIOS Y FINANCIEROS",
    "pro_memoria" = "PRO-MEMORIA"
  )
  
  # Añadir cada categoría
  for (cat_id in names(datos_por_categoria)) {
    
    cat_nombre <- nombres_categorias[cat_id]
    datos_cat <- datos_por_categoria[[cat_id]]
    
    if (!is.null(datos_cat) && nrow(datos_cat) > 0) {
      # Título de categoría
      doc <- doc |>
        officer::body_add_par(cat_nombre, style = "heading 2") |>
        officer::body_add_par("", style = "Normal")
      
      # Añadir tabla usando officer
      doc <- doc |>
        officer::body_add_table(
          value = datos_cat,
          style = "Light Grid Accent 1",
          first_row = TRUE
        ) |>
        officer::body_add_par("", style = "Normal")
    }
  }
  
  # Guardar documento
  print(doc, target = archivo_salida)
  
  return(archivo_salida)
}

# Función para exportar a Excel
exportar_a_excel <- function(datos_por_categoria, datos_completos, pais_nombre, 
                             pais_codigo, fecha_inicio, fecha_fin, archivo_salida) {
  
  # Crear workbook
  wb <- openxlsx::createWorkbook()
  
  # Estilo para encabezados (tonos pastel profesionales)
  estilo_encabezado <- openxlsx::createStyle(
    fontSize = 11,
    fontName = "Segoe UI",
    fontColour = "#FFFFFF",
    fgFill = "#8FA7C3",
    halign = "center",
    valign = "center",
    textDecoration = "bold",
    border = "TopBottomLeftRight"
  )
  
  # Estilo para datos
  estilo_datos <- openxlsx::createStyle(
    fontSize = 10,
    fontName = "Segoe UI",
    halign = "right",
    border = "TopBottomLeftRight",
    numFmt = "#,##0.00"
  )
  
  # Estilo para indicadores
  estilo_indicador <- openxlsx::createStyle(
    fontSize = 10,
    fontName = "Segoe UI",
    halign = "left",
    border = "TopBottomLeftRight"
  )
  
  # HOJA 1: Información
  openxlsx::addWorksheet(wb, "Información")
  
  info_data <- data.frame(
    Campo = c("País", "Código ISO", "Período inicio", "Período fin", 
              "Fecha generación", "Fuentes de datos"),
    Valor = c(pais_nombre, pais_codigo, 
              format(fecha_inicio, "%Y"), 
              format(fecha_fin, "%Y"),
              format(Sys.Date(), "%d/%m/%Y"),
              "Banco Mundial (World Development Indicators)")
  )
  
  openxlsx::writeData(wb, "Información", info_data, startRow = 2, startCol = 2)
  openxlsx::addStyle(wb, "Información", estilo_encabezado, rows = 2, cols = 2:3, gridExpand = TRUE)
  openxlsx::setColWidths(wb, "Información", cols = 2:3, widths = c(25, 50))
  
  # HOJA 2: Datos Consolidados
  openxlsx::addWorksheet(wb, "Datos Consolidados")
  
  if (!is.null(datos_completos) && nrow(datos_completos) > 0) {
    datos_para_excel <- datos_completos |>
      dplyr::select(year, indicador_nombre, valor, fuente) |>
      tidyr::pivot_wider(
        names_from = year,
        values_from = valor,
        names_sort = TRUE
      ) |>
      dplyr::arrange(indicador_nombre)
    
    openxlsx::writeData(wb, "Datos Consolidados", datos_para_excel, 
                        startRow = 1, startCol = 1, headerStyle = estilo_encabezado)
    
    openxlsx::addStyle(wb, "Datos Consolidados", estilo_indicador, 
                       rows = 2:(nrow(datos_para_excel) + 1), cols = 1:2, gridExpand = TRUE)
    
    if (ncol(datos_para_excel) > 2) {
      openxlsx::addStyle(wb, "Datos Consolidados", estilo_datos,
                         rows = 2:(nrow(datos_para_excel) + 1), 
                         cols = 3:ncol(datos_para_excel), gridExpand = TRUE)
    }
    
    openxlsx::setColWidths(wb, "Datos Consolidados", cols = 1, widths = 45)
    openxlsx::setColWidths(wb, "Datos Consolidados", cols = 2, widths = 20)
    openxlsx::freezePane(wb, "Datos Consolidados", firstRow = TRUE, firstCol = TRUE)
  }
  
  # HOJAS 3-8: Una hoja por cada categoría
  nombres_categorias <- c(
    "sector_real" = "Sector real",
    "sector_exterior" = "Sector exterior",
    "sector_publico" = "Sector público",
    "precios_costes" = "Precios y costes",
    "indicadores_monetarios" = "Indicadores monetarios",
    "pro_memoria" = "Pro-memoria"
  )
  
  for (cat_id in names(datos_por_categoria)) {
    
    cat_nombre <- nombres_categorias[cat_id]
    datos_cat <- datos_por_categoria[[cat_id]]
    
    if (!is.null(datos_cat) && nrow(datos_cat) > 0) {
      openxlsx::addWorksheet(wb, cat_nombre)
      
      openxlsx::writeData(wb, cat_nombre, cat_nombre, startRow = 1, startCol = 1)
      openxlsx::mergeCells(wb, cat_nombre, rows = 1, cols = 1:ncol(datos_cat))
      
      titulo_style <- openxlsx::createStyle(
        fontSize = 14,
        fontName = "Segoe UI",
        fontColour = "#FFFFFF",
        fgFill = "#8FA7C3",
        halign = "center",
        textDecoration = "bold"
      )
      openxlsx::addStyle(wb, cat_nombre, titulo_style, rows = 1, cols = 1)
      
      openxlsx::writeData(wb, cat_nombre, datos_cat, startRow = 3, startCol = 1, 
                          headerStyle = estilo_encabezado)
      
      openxlsx::addStyle(wb, cat_nombre, estilo_indicador,
                         rows = 4:(nrow(datos_cat) + 3), cols = 1, gridExpand = TRUE)
      
      if (ncol(datos_cat) > 1) {
        openxlsx::addStyle(wb, cat_nombre, estilo_datos,
                           rows = 4:(nrow(datos_cat) + 3), 
                           cols = 2:ncol(datos_cat), gridExpand = TRUE)
      }
      
      openxlsx::setColWidths(wb, cat_nombre, cols = 1, widths = 45)
      if (ncol(datos_cat) > 1) {
        openxlsx::setColWidths(wb, cat_nombre, cols = 2:ncol(datos_cat), widths = 12)
      }
      
      openxlsx::freezePane(wb, cat_nombre, firstRow = 4, firstCol = TRUE)
    }
  }
  
  # Guardar archivo
  openxlsx::saveWorkbook(wb, archivo_salida, overwrite = TRUE)
  
  return(archivo_salida)
}

# ============================================================================
# INTERFAZ DE USUARIO (UI) - ESTÉTICA MEJORADA CON TONOS PASTEL
# ============================================================================

ui <- shinydashboard::dashboardPage(
  
  shinydashboard::dashboardHeader(
    title = "Análisis Macroeconómico",
    titleWidth = 350
  ),
  
  shinydashboard::dashboardSidebar(
    width = 280,
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem("Descarga de datos", tabName = "descarga", icon = shiny::icon("download")),
      shinydashboard::menuItem("Ayuda", tabName = "ayuda", icon = shiny::icon("question-circle"))
    )
  ),
  
  shinydashboard::dashboardBody(
    
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("
        @import url('https://fonts.googleapis.com/css2?family=Nunito:wght@300;400;600;700&display=swap');
        
        body, .content-wrapper, .main-sidebar, .main-header {
          font-family: 'Segoe UI', 'Nunito', -apple-system, BlinkMacSystemFont, sans-serif !important;
        }
        
        .content-wrapper { 
          background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%);
        }
        
        .box { 
          border-top-color: #8FA7C3;
          box-shadow: 0 2px 8px rgba(0,0,0,0.08);
          border-radius: 8px;
        }
        
        .box-header {
          background: linear-gradient(135deg, #8FA7C3 0%, #A8C0D8 100%);
          color: white;
          border-radius: 8px 8px 0 0;
        }
        
        .box-title {
          font-weight: 600;
        }
        
        .btn-primary { 
          background: linear-gradient(135deg, #8FA7C3 0%, #7B98B8 100%);
          border-color: #7B98B8;
          font-weight: 600;
          border-radius: 6px;
          transition: all 0.3s ease;
        }
        
        .btn-primary:hover { 
          background: linear-gradient(135deg, #7B98B8 0%, #6B88A8 100%);
          border-color: #6B88A8;
          transform: translateY(-2px);
          box-shadow: 0 4px 12px rgba(0,0,0,0.15);
        }
        
        .btn-warning {
          background: linear-gradient(135deg, #F4C7AB 0%, #E8B598 100%);
          border-color: #E8B598;
          color: #5a3825;
          font-weight: 600;
          border-radius: 6px;
        }
        
        .btn-warning:hover {
          background: linear-gradient(135deg, #E8B598 0%, #DCa385 100%);
          border-color: #DCa385;
          transform: translateY(-2px);
        }
        
        .btn-success {
          background: linear-gradient(135deg, #A8D5BA 0%, #95C9A7 100%);
          border-color: #95C9A7;
          color: #2d5a3d;
          font-weight: 600;
          border-radius: 6px;
        }
        
        .btn-success:hover {
          background: linear-gradient(135deg, #95C9A7 0%, #82BD94 100%);
          border-color: #82BD94;
          transform: translateY(-2px);
        }
        
        .form-control {
          border: 2px solid #D4E2EF;
          border-radius: 6px;
          font-family: 'Segoe UI', sans-serif;
        }
        
        .form-control:focus {
          border-color: #8FA7C3;
          box-shadow: 0 0 0 0.2rem rgba(143, 167, 195, 0.25);
        }
        
        .main-sidebar {
          background: linear-gradient(180deg, #5C7A99 0%, #4A6580 100%);
        }
        
        .sidebar-menu > li > a {
          color: #E8F1F8;
          font-weight: 500;
        }
        
        .sidebar-menu > li.active > a {
          background-color: #8FA7C3;
          color: white;
          border-left: 4px solid #F4C7AB;
        }
        
        .main-header .navbar {
          background: linear-gradient(135deg, #5C7A99 0%, #6B88A8 100%);
        }
        
        .main-header .logo {
          background: linear-gradient(135deg, #4A6580 0%, #5C7A99 100%);
          color: white;
          font-weight: 700;
        }
        
        .nav-tabs-custom > .nav-tabs > li.active {
          border-top-color: #8FA7C3;
        }
        
        .nav-tabs-custom > .nav-tabs > li.active > a {
          color: #5C7A99;
          font-weight: 600;
        }
        
        .dataTables_wrapper .dataTables_paginate .paginate_button.current {
          background: #8FA7C3 !important;
          border-color: #8FA7C3 !important;
          color: white !important;
        }
        
        pre {
          background-color: #F8FAFB;
          border: 1px solid #D4E2EF;
          border-radius: 6px;
          font-family: 'Segoe UI', monospace;
        }
      "))
    ),
    
    shinydashboard::tabItems(
      
      # TAB 1: Descarga de datos
      shinydashboard::tabItem(
        tabName = "descarga",
        
        shiny::fluidRow(
          shinydashboard::box(
            title = "Configuración de la descarga",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            shiny::fluidRow(
              shiny::column(
                width = 4,
                shiny::selectInput(
                  "pais",
                  "Seleccionar país:",
                  choices = NULL,
                  selected = NULL
                )
              ),
              
              shiny::column(
                width = 4,
                shiny::numericInput(
                  "fecha_inicio",
                  "Año inicial:",
                  value = lubridate::year(Sys.Date()) - 6,
                  min = 2000,
                  max = lubridate::year(Sys.Date()) + 10,
                  step = 1
                )
              ),
              
              shiny::column(
                width = 4,
                shiny::numericInput(
                  "fecha_fin",
                  "Año final:",
                  value = lubridate::year(Sys.Date()) + 2,
                  min = 2000,
                  max = lubridate::year(Sys.Date()) + 10,
                  step = 1
                )
              )
            ),
            
            shiny::fluidRow(
              shiny::column(
                width = 12,
                shiny::actionButton(
                  "btn_descargar",
                  "Descargar Datos",
                  icon = shiny::icon("cloud-download-alt"),
                  class = "btn-primary btn-lg",
                  width = "100%"
                )
              )
            )
          )
        ),
        
        shiny::fluidRow(
          shinydashboard::box(
            title = "Estado de la descarga",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            shiny::verbatimTextOutput("status_descarga")
          )
        ),
        
        shiny::fluidRow(
          shinydashboard::box(
            title = "Vista previa de datos",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            collapsed = TRUE,
            
            shiny::tabsetPanel(
              id = "tabs_categorias",
              shiny::tabPanel("Sector real", DT::DTOutput("tabla_sector_real")),
              shiny::tabPanel("Sector exterior", DT::DTOutput("tabla_sector_exterior")),
              shiny::tabPanel("Sector público", DT::DTOutput("tabla_sector_publico")),
              shiny::tabPanel("Precios y costes", DT::DTOutput("tabla_precios")),
              shiny::tabPanel("Indicadores monetarios", DT::DTOutput("tabla_monetarios")),
              shiny::tabPanel("Pro-memoria", DT::DTOutput("tabla_promemoria"))
            )
          )
        ),
        
        shiny::fluidRow(
          shiny::column(
            width = 6,
            shinydashboard::box(
              title = "Exportar a Word",
              status = "warning",
              solidHeader = TRUE,
              width = NULL,
              shiny::downloadButton("btn_exportar_word", "Descargar Word (.docx)", 
                                    class = "btn-warning", width = "100%")
            )
          ),
          
          shiny::column(
            width = 6,
            shinydashboard::box(
              title = "Exportar a Excel",
              status = "success",
              solidHeader = TRUE,
              width = NULL,
              shiny::downloadButton("btn_exportar_excel", "Descargar Excel (.xlsx)", 
                                    class = "btn-success", width = "100%")
            )
          )
        )
      ),
      
      # TAB 2: Ayuda
      shinydashboard::tabItem(
        tabName = "ayuda",
        
        shiny::fluidRow(
          shinydashboard::box(
            title = "Guía de Uso",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            shiny::h3("Aplicación de descarga de datos macroeconómicos"),
            shiny::p("Esta aplicación facilita la preparación de dictámenes económicos, 
              organizando automáticamente los datos en las categorías estándar 
              utilizadas en los análisis macroeconómicos."),
            
            shiny::h4("Instrucciones de uso:"),
            shiny::tags$ol(
              shiny::tags$li("Seleccione un país de la lista desplegable."),
              shiny::tags$li("Indique el período de análisis (año inicial y año final) - Por defecto: 6 años antes del actual hasta 2 años después."),
              shiny::tags$li("Haga clic en 'Descargar datos' para obtener la información."),
              shiny::tags$li("Revise los datos en las pestañas de vista previa."),
              shiny::tags$li("Exporte los datos a Word o Excel según necesite.")
            ),
            
            shiny::h4("Categorías de datos:"),
            shiny::tags$ul(
              shiny::tags$li(shiny::strong("Sector real:"), "PIB, consumo, inversión, empleo, estructura sectorial."),
              shiny::tags$li(shiny::strong("Sector exterior:"), "Balanza de pagos, comercio exterior, deuda externa, reservas."),
              shiny::tags$li(shiny::strong("Sector público:"), "Déficit, deuda pública, ingresos y gastos del gobierno."),
              shiny::tags$li(shiny::strong("Precios y costes:"), "IPC, deflactor del PIB, salarios."),
              shiny::tags$li(shiny::strong("Indicadores monetarios:"), "Masa monetaria, tipos de interés, crédito."),
              shiny::tags$li(shiny::strong("Pro-memoria:"), "Población, datos sociales, desarrollo humano.")
            ),
            
            shiny::h4("Fuente de datos:"),
            shiny::p(shiny::strong("Banco Mundial - World Development Indicators (WDI)"), 
                     " es la fuente principal de datos macroeconómicos, con cobertura de más de 200 países."),
            
            shiny::h4("Formatos de exportación:"),
            shiny::tags$ul(
              shiny::tags$li(shiny::strong("Word (.docx):"), "Documento con tablas organizadas por categorías."),
              shiny::tags$li(shiny::strong("Excel (.xlsx):"), "Archivo con múltiples hojas: información del país, 
                      datos consolidados, y una hoja por cada categoría.")
            ),
            
            shiny::h4("Notas importantes:"),
            shiny::tags$ul(
              shiny::tags$li("Los datos se actualizan periódicamente en el Banco Mundial."),
              shiny::tags$li("Algunos indicadores pueden no estar disponibles para todos los países."),
              shiny::tags$li("Las proyecciones (años futuros) pueden no estar disponibles."),
              shiny::tags$li("Los valores NA (no disponibles) se excluyen automáticamente.")
            ),
            
            shiny::hr(),
            
            shiny::p(shiny::em("Preparación de Dictámenes Económicos - 2025"))
          )
        )
      )
    )
  )
)

# ============================================================================
# SERVIDOR (SERVER)
# ============================================================================

server <- function(input, output, session) {
  
  # Variables reactivas
  datos_descargados <- shiny::reactiveVal(NULL)
  datos_por_categoria <- shiny::reactiveVal(list())
  pais_seleccionado <- shiny::reactiveVal("")
  
  # Cargar lista de países al iniciar
  shiny::observe({
    paises <- obtener_lista_paises()
    paises_lista <- setNames(paises$iso2c, paises$country_es)
    
    shiny::updateSelectInput(
      session,
      "pais",
      choices = paises_lista,
      selected = "ES"
    )
  })
  
  # Estado de descarga
  output$status_descarga <- shiny::renderText({
    if (is.null(datos_descargados())) {
      "Esperando descarga de datos..."
    } else {
      n_indicadores <- length(unique(datos_descargados()$indicador_codigo))
      n_registros <- nrow(datos_descargados())
      
      paste0("✓ Datos descargados correctamente para: ", pais_seleccionado(), 
             "\n✓ Período: ", input$fecha_inicio, " - ", input$fecha_fin,
             "\n✓ Indicadores descargados: ", n_indicadores,
             "\n✓ Registros totales: ", n_registros,
             "\n\nPuede revisar los datos en la vista previa y exportarlos.")
    }
  })
  
  # Botón de descarga
  shiny::observeEvent(input$btn_descargar, {
    
    shiny::req(input$pais, input$fecha_inicio, input$fecha_fin)
    
    if (input$fecha_inicio >= input$fecha_fin) {
      shiny::showNotification(
        "El año de inicio debe ser anterior al año fin",
        type = "error",
        duration = 5
      )
      return()
    }
    
    shiny::withProgress(message = 'Descargando datos del Banco Mundial...', value = 0, {
      
      paises <- obtener_lista_paises()
      nombre_pais <- paises$country_es[paises$iso2c == input$pais]
      pais_seleccionado(nombre_pais)
      
      shiny::incProgress(0.2, detail = "Conectando con la API...")
      
      datos_bm <- descargar_datos_bm(
        input$pais, 
        as.Date(paste0(input$fecha_inicio, "-01-01")),
        as.Date(paste0(input$fecha_fin, "-12-31"))
      )
      
      shiny::incProgress(0.5, detail = "Procesando datos...")
      
      if (is.null(datos_bm) || nrow(datos_bm) == 0) {
        shiny::showNotification(
          "No se pudieron descargar datos para el país y período seleccionado. Intente con otro país o período.",
          type = "error",
          duration = 8
        )
        return()
      }
      
      datos_descargados(datos_bm)
      
      shiny::incProgress(0.7, detail = "Organizando por categorías...")
      
      datos_cat <- organizar_por_categoria(datos_bm)
      datos_por_categoria(datos_cat)
      
      shiny::incProgress(1, detail = "¡Completado!")
    })
    
    shiny::showNotification(
      "✓ Datos descargados y organizados correctamente",
      type = "message",
      duration = 3
    )
  })
  
  # Función auxiliar para renderizar tablas DT
  renderizar_tabla <- function(datos) {
    if (is.null(datos) || nrow(datos) == 0) {
      return(NULL)
    }
    
    cols_numericas <- which(sapply(datos, is.numeric))
    
    dt <- DT::datatable(
      datos,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        dom = 'Bfrtip',
        language = list(
          url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'
        )
      ),
      rownames = FALSE
    )
    
    if (length(cols_numericas) > 0) {
      dt <- DT::formatRound(dt, columns = cols_numericas, digits = 2)
    }
    
    return(dt)
  }
  
  # Tablas de vista previa
  output$tabla_sector_real <- DT::renderDT({
    shiny::req(datos_por_categoria())
    if ("sector_real" %in% names(datos_por_categoria())) {
      renderizar_tabla(datos_por_categoria()$sector_real)
    }
  })
  
  output$tabla_sector_exterior <- DT::renderDT({
    shiny::req(datos_por_categoria())
    if ("sector_exterior" %in% names(datos_por_categoria())) {
      renderizar_tabla(datos_por_categoria()$sector_exterior)
    }
  })
  
  output$tabla_sector_publico <- DT::renderDT({
    shiny::req(datos_por_categoria())
    if ("sector_publico" %in% names(datos_por_categoria())) {
      renderizar_tabla(datos_por_categoria()$sector_publico)
    }
  })
  
  output$tabla_precios <- DT::renderDT({
    shiny::req(datos_por_categoria())
    if ("precios_costes" %in% names(datos_por_categoria())) {
      renderizar_tabla(datos_por_categoria()$precios_costes)
    }
  })
  
  output$tabla_monetarios <- DT::renderDT({
    shiny::req(datos_por_categoria())
    if ("indicadores_monetarios" %in% names(datos_por_categoria())) {
      renderizar_tabla(datos_por_categoria()$indicadores_monetarios)
    }
  })
  
  output$tabla_promemoria <- DT::renderDT({
    shiny::req(datos_por_categoria())
    if ("pro_memoria" %in% names(datos_por_categoria())) {
      renderizar_tabla(datos_por_categoria()$pro_memoria)
    }
  })
  
  # Exportar a Word
  output$btn_exportar_word <- shiny::downloadHandler(
    filename = function() {
      paste0("Dictamen_", input$pais, "_", format(Sys.Date(), "%Y%m%d"), ".docx")
    },
    content = function(file) {
      shiny::req(datos_por_categoria(), pais_seleccionado())
      
      tryCatch({
        exportar_a_word(
          datos_por_categoria(),
          pais_seleccionado(),
          as.Date(paste0(input$fecha_inicio, "-01-01")),
          as.Date(paste0(input$fecha_fin, "-12-31")),
          file
        )
        
        shiny::showNotification(
          "✓ Documento Word exportado correctamente",
          type = "message",
          duration = 3
        )
      }, error = function(e) {
        shiny::showNotification(
          paste("Error al exportar a Word:", e$message),
          type = "error",
          duration = 5
        )
      })
    }
  )
  
  # Exportar a Excel
  output$btn_exportar_excel <- shiny::downloadHandler(
    filename = function() {
      paste0("Dictamen_", input$pais, "_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      shiny::req(datos_por_categoria(), datos_descargados(), pais_seleccionado())
      
      tryCatch({
        exportar_a_excel(
          datos_por_categoria(),
          datos_descargados(),
          pais_seleccionado(),
          input$pais,
          as.Date(paste0(input$fecha_inicio, "-01-01")),
          as.Date(paste0(input$fecha_fin, "-12-31")),
          file
        )
        
        shiny::showNotification(
          "✓ Archivo Excel exportado correctamente",
          type = "message",
          duration = 3
        )
      }, error = function(e) {
        shiny::showNotification(
          paste("Error al exportar a Excel:", e$message),
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

shiny::shinyApp(ui = ui, server = server)