# ============================================================================
# APLICACIÓN SHINY PARA DESCARGA DE DATOS MACROECONÓMICOS
# Preparación de Dictámenes Económicos - Versión 8.0
# ============================================================================
#
# FUENTES DE DATOS:
# 1. FMI (WEO, FM, BOP, CPI) - Usando paquete imfapi (API SDMX 3.0)
# 2. Eurostat
# 3. OCDE (NUEVO v8.0)
# 4. Banco Mundial (World Development Indicators)
# 5. OMC (Organización Mundial del Comercio)
# 6. BIS (Bank for International Settlements)
# 7. DBnomics (NUEVO v8.0)
#
# CAMBIOS v8.0:
# - CORREGIDO: Indicadores BOP (cuenta corriente, inversión directa, etc.)
#              ahora aparecen correctamente en tabla y Word
# - CORREGIDO: Indicadores CPI se muestran con nombres descriptivos
# - NUEVO: Fuente OCDE para países miembros (38 países)
# - NUEVO: Fuente DBnomics como agregador de múltiples fuentes
# - MEJORADO: Categorización ampliada para incluir nuevos indicadores
# - MEJORADO: Sistema de deduplicación actualizado con prioridades
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
    "WDI",
    "dplyr",
    "tidyr",
    "purrr",
    "officer",
    "openxlsx",
    "DT",
    "lubridate",
    "stringr",
    "httr",
    "readxl",
    "countrycode",
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
    library(WDI)
    library(dplyr)
    library(tidyr)
    library(officer)
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
    bg = "#f8f9fa",
    fg = "#2c3e50",
    primary = "#7fadcf",
    secondary = "#95a5a6",
    success = "#a8d5ba",
    info = "#87ceeb",
    warning = "#f4c7ab",
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
        background: linear-gradient(135deg, #7fadcf 0%, #5a9bc9 100%) !important;
        box-shadow: 0 2px 10px rgba(0,0,0,0.1);
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
      }

      .card:hover {
        transform: translateY(-2px);
        box-shadow: 0 6px 20px rgba(0,0,0,0.12);
      }

      .card-header {
        background: linear-gradient(135deg, #7fadcf 0%, #a8c8de 100%);
        color: white;
        border-radius: 12px 12px 0 0 !important;
        font-weight: 600;
        padding: 1rem 1.25rem;
      }

      .btn-primary {
        background: linear-gradient(135deg, #7fadcf 0%, #5a9bc9 100%);
        border: none;
        border-radius: 8px;
        font-weight: 600;
        padding: 0.6rem 1.5rem;
        transition: all 0.3s ease;
      }

      .btn-primary:hover {
        background: linear-gradient(135deg, #5a9bc9 0%, #4a8bb9 100%);
        transform: translateY(-2px);
        box-shadow: 0 4px 12px rgba(90, 155, 201, 0.4);
      }

      .btn-success {
        background: linear-gradient(135deg, #a8d5ba 0%, #8bc9a0 100%);
        border: none;
        color: #2d5a3d;
        border-radius: 8px;
        font-weight: 600;
      }

      .btn-success:hover {
        background: linear-gradient(135deg, #8bc9a0 0%, #6eb886 100%);
        color: #2d5a3d;
        transform: translateY(-2px);
      }

      .btn-warning {
        background: linear-gradient(135deg, #f4c7ab 0%, #e8b598 100%);
        border: none;
        color: #5a3825;
        border-radius: 8px;
        font-weight: 600;
      }

      .btn-warning:hover {
        background: linear-gradient(135deg, #e8b598 0%, #dca385 100%);
        color: #5a3825;
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
        border: 2px solid #d4e2ef;
        border-radius: 8px;
        padding: 0.6rem 1rem;
        transition: border-color 0.2s ease, box-shadow 0.2s ease;
      }

      .form-control:focus, .form-select:focus {
        border-color: #7fadcf;
        box-shadow: 0 0 0 0.2rem rgba(127, 173, 207, 0.25);
      }

      .nav-tabs .nav-link {
        border-radius: 8px 8px 0 0;
        font-weight: 500;
        color: #5a7a8a;
        transition: all 0.2s ease;
      }

      .nav-tabs .nav-link.active {
        background-color: #7fadcf;
        color: white;
        border-color: #7fadcf;
      }

      .nav-tabs .nav-link:hover:not(.active) {
        background-color: #e8f1f8;
        color: #5a9bc9;
      }

      .dataTables_wrapper {
        font-family: 'Segoe UI', sans-serif !important;
      }

      .accordion {
        border-radius: 12px;
        overflow: hidden;
      }

      .accordion-button {
        background: linear-gradient(135deg, #7fadcf 0%, #a8c8de 100%);
        color: white;
        font-weight: 600;
        padding: 1rem 1.25rem;
      }

      .accordion-button:not(.collapsed) {
        background: linear-gradient(135deg, #5a9bc9 0%, #7fadcf 100%);
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
      }

      .hero-section {
        background: linear-gradient(135deg, #7fadcf 0%, #a8c8de 50%, #d4e8f0 100%);
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
        background-color: #e8f1f8;
      }

      .progress-bar {
        background: linear-gradient(90deg, #7fadcf 0%, #5a9bc9 100%);
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
        background-color: #f0f7fc;
        border-radius: 8px;
        padding: 1rem;
        margin-top: 1rem;
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
  # DICCIONARIO DE PAÍSES
  # ============================================================================
  
  diccionario_paises_es <- c(
    "Afghanistan" = "Afganistán", "Albania" = "Albania", "Algeria" = "Argelia",
    "Andorra" = "Andorra", "Angola" = "Angola", "Argentina" = "Argentina",
    "Armenia" = "Armenia", "Australia" = "Australia", "Austria" = "Austria",
    "Azerbaijan" = "Azerbaiyán", "Bahamas, The" = "Bahamas", "Bahrain" = "Baréin",
    "Bangladesh" = "Bangladés", "Barbados" = "Barbados", "Belarus" = "Bielorrusia",
    "Belgium" = "Bélgica", "Belize" = "Belice", "Benin" = "Benín",
    "Bhutan" = "Bután", "Bolivia" = "Bolivia", "Bosnia and Herzegovina" = "Bosnia y Herzegovina",
    "Botswana" = "Botsuana", "Brazil" = "Brasil", "Brunei Darussalam" = "Brunéi",
    "Bulgaria" = "Bulgaria", "Burkina Faso" = "Burkina Faso", "Burundi" = "Burundi",
    "Cabo Verde" = "Cabo Verde", "Cambodia" = "Camboya", "Cameroon" = "Camerún",
    "Canada" = "Canadá", "Central African Republic" = "República Centroafricana",
    "Chad" = "Chad", "Chile" = "Chile", "China" = "China", "Colombia" = "Colombia",
    "Comoros" = "Comoras", "Congo, Dem. Rep." = "República Democrática del Congo",
    "Congo, Rep." = "República del Congo", "Costa Rica" = "Costa Rica",
    "Cote d'Ivoire" = "Costa de Marfil", "Croatia" = "Croacia", "Cuba" = "Cuba",
    "Cyprus" = "Chipre", "Czech Republic" = "República Checa", "Czechia" = "Chequia",
    "Denmark" = "Dinamarca", "Djibouti" = "Yibuti", "Dominica" = "Dominica",
    "Dominican Republic" = "República Dominicana", "Ecuador" = "Ecuador",
    "Egypt, Arab Rep." = "Egipto", "El Salvador" = "El Salvador",
    "Equatorial Guinea" = "Guinea Ecuatorial", "Eritrea" = "Eritrea",
    "Estonia" = "Estonia", "Eswatini" = "Esuatini", "Ethiopia" = "Etiopía",
    "Fiji" = "Fiyi", "Finland" = "Finlandia", "France" = "Francia",
    "Gabon" = "Gabón", "Gambia, The" = "Gambia", "Georgia" = "Georgia",
    "Germany" = "Alemania", "Ghana" = "Ghana", "Greece" = "Grecia",
    "Grenada" = "Granada", "Guatemala" = "Guatemala", "Guinea" = "Guinea",
    "Guinea-Bissau" = "Guinea-Bisáu", "Guyana" = "Guyana", "Haiti" = "Haití",
    "Honduras" = "Honduras", "Hong Kong SAR, China" = "Hong Kong",
    "Hungary" = "Hungría", "Iceland" = "Islandia", "India" = "India",
    "Indonesia" = "Indonesia", "Iran, Islamic Rep." = "Irán", "Iraq" = "Irak",
    "Ireland" = "Irlanda", "Israel" = "Israel", "Italy" = "Italia",
    "Jamaica" = "Jamaica", "Japan" = "Japón", "Jordan" = "Jordania",
    "Kazakhstan" = "Kazajistán", "Kenya" = "Kenia", "Kiribati" = "Kiribati",
    "Korea, Dem. People's Rep." = "Corea del Norte", "Korea, Rep." = "Corea del Sur",
    "Kosovo" = "Kosovo", "Kuwait" = "Kuwait", "Kyrgyz Republic" = "Kirguistán",
    "Lao PDR" = "Laos", "Latvia" = "Letonia", "Lebanon" = "Líbano",
    "Lesotho" = "Lesoto", "Liberia" = "Liberia", "Libya" = "Libia",
    "Liechtenstein" = "Liechtenstein", "Lithuania" = "Lituania",
    "Luxembourg" = "Luxemburgo", "Macao SAR, China" = "Macao",
    "Madagascar" = "Madagascar", "Malawi" = "Malaui", "Malaysia" = "Malasia",
    "Maldives" = "Maldivas", "Mali" = "Malí", "Malta" = "Malta",
    "Marshall Islands" = "Islas Marshall", "Mauritania" = "Mauritania",
    "Mauritius" = "Mauricio", "Mexico" = "México",
    "Micronesia, Fed. Sts." = "Micronesia", "Moldova" = "Moldavia",
    "Monaco" = "Mónaco", "Mongolia" = "Mongolia", "Montenegro" = "Montenegro",
    "Morocco" = "Marruecos", "Mozambique" = "Mozambique", "Myanmar" = "Birmania",
    "Namibia" = "Namibia", "Nauru" = "Nauru", "Nepal" = "Nepal",
    "Netherlands" = "Países Bajos", "New Zealand" = "Nueva Zelanda",
    "Nicaragua" = "Nicaragua", "Niger" = "Níger", "Nigeria" = "Nigeria",
    "North Macedonia" = "Macedonia del Norte", "Norway" = "Noruega",
    "Oman" = "Omán", "Pakistan" = "Pakistán", "Palau" = "Palaos",
    "Panama" = "Panamá", "Papua New Guinea" = "Papúa Nueva Guinea",
    "Paraguay" = "Paraguay", "Peru" = "Perú", "Philippines" = "Filipinas",
    "Poland" = "Polonia", "Portugal" = "Portugal", "Puerto Rico" = "Puerto Rico",
    "Qatar" = "Catar", "Romania" = "Rumanía", "Russian Federation" = "Rusia",
    "Rwanda" = "Ruanda", "Samoa" = "Samoa", "San Marino" = "San Marino",
    "Sao Tome and Principe" = "Santo Tomé y Príncipe", "Saudi Arabia" = "Arabia Saudita",
    "Senegal" = "Senegal", "Serbia" = "Serbia", "Seychelles" = "Seychelles",
    "Sierra Leone" = "Sierra Leona", "Singapore" = "Singapur",
    "Slovak Republic" = "Eslovaquia", "Slovenia" = "Eslovenia",
    "Solomon Islands" = "Islas Salomón", "Somalia" = "Somalia",
    "South Africa" = "Sudáfrica", "South Sudan" = "Sudán del Sur",
    "Spain" = "España", "Sri Lanka" = "Sri Lanka",
    "St. Kitts and Nevis" = "San Cristóbal y Nieves", "St. Lucia" = "Santa Lucía",
    "St. Vincent and the Grenadines" = "San Vicente y las Granadinas",
    "Sudan" = "Sudán", "Suriname" = "Surinam", "Sweden" = "Suecia",
    "Switzerland" = "Suiza", "Syrian Arab Republic" = "Siria",
    "Tajikistan" = "Tayikistán", "Tanzania" = "Tanzania", "Thailand" = "Tailandia",
    "Timor-Leste" = "Timor Oriental", "Togo" = "Togo", "Tonga" = "Tonga",
    "Trinidad and Tobago" = "Trinidad y Tobago", "Tunisia" = "Túnez",
    "Turkey" = "Turquía", "Turkmenistan" = "Turkmenistán", "Tuvalu" = "Tuvalu",
    "Uganda" = "Uganda", "Ukraine" = "Ucrania",
    "United Arab Emirates" = "Emiratos Árabes Unidos", "United Kingdom" = "Reino Unido",
    "United States" = "Estados Unidos", "Uruguay" = "Uruguay",
    "Uzbekistan" = "Uzbekistán", "Vanuatu" = "Vanuatu", "Venezuela, RB" = "Venezuela",
    "Vietnam" = "Vietnam", "West Bank and Gaza" = "Cisjordania y Gaza",
    "Yemen, Rep." = "Yemen", "Zambia" = "Zambia", "Zimbabwe" = "Zimbabue"
  )
  
  # Lista de países UE
  paises_ue <- c("AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR",
                 "DE", "GR", "HU", "IE", "IT", "LV", "LT", "LU", "MT", "NL",
                 "PL", "PT", "RO", "SK", "SI", "ES", "SE")
  
  # ============================================================================
  # INDICADORES DEL BANCO MUNDIAL
  # ============================================================================
  
  indicadores_banco_mundial <- list(
    sector_real = list(
      "NY.GDP.MKTP.KD.ZG" = c("PIB real", "var. %", "Variación porcentual"),
      "NY.GDP.MKTP.CD" = c("PIB nominal", "mill. USD", "Millones de dólares (USD)"),
      "NY.GDP.PCAP.CD" = c("PIB per cápita", "USD", "Dólares (USD)"),
      "NE.CON.PRVT.ZS" = c("Consumo privado", "% PIB", "Porcentaje del PIB"),
      "NE.CON.GOVT.ZS" = c("Consumo público", "% PIB", "Porcentaje del PIB"),
      "NE.GDI.TOTL.ZS" = c("Formación bruta de capital", "% PIB", "Porcentaje del PIB"),
      "NV.AGR.TOTL.ZS" = c("Agricultura, valor añadido", "% PIB", "Porcentaje del PIB"),
      "NV.IND.TOTL.ZS" = c("Industria, valor añadido", "% PIB", "Porcentaje del PIB"),
      "NV.SRV.TOTL.ZS" = c("Servicios, valor añadido", "% PIB", "Porcentaje del PIB")
    ),
    
    mercado_laboral = list(
      "SL.UEM.TOTL.ZS" = c("Tasa de desempleo", "%", "Porcentaje"),
      "SL.TLF.CACT.ZS" = c("Tasa de actividad", "%", "Porcentaje"),
      "SL.EMP.TOTL.SP.ZS" = c("Ratio empleo-población", "%", "Porcentaje"),
      "SL.TLF.TOTL.IN" = c("Población activa", "mill.", "Millones de personas"),
      "SL.UEM.TOTL.NE.ZS" = c("Desempleo nacional", "%", "Porcentaje")
    ),
    
    sector_exterior = list(
      "BN.CAB.XOKA.GD.ZS" = c("Cuenta corriente", "% PIB", "Porcentaje del PIB"),
      "BX.KLT.DINV.WD.GD.ZS" = c("Inversión extranjera directa neta", "% PIB", "Porcentaje del PIB"),
      "NE.EXP.GNFS.ZS" = c("Exportaciones de bienes y servicios", "% PIB", "Porcentaje del PIB"),
      "NE.IMP.GNFS.ZS" = c("Importaciones de bienes y servicios", "% PIB", "Porcentaje del PIB"),
      "DT.DOD.DECT.GN.ZS" = c("Deuda externa", "% INB", "Porcentaje del INB"),
      "FI.RES.TOTL.CD" = c("Reservas internacionales", "mill. USD", "Millones de dólares (USD)"),
      "PA.NUS.FCRF" = c("Tipo de cambio oficial", "UML/USD", "Unidades de moneda local por USD")
    ),
    
    sector_publico = list(
      "GC.DOD.TOTL.GD.ZS" = c("Deuda pública", "% PIB", "Porcentaje del PIB"),
      "GC.REV.XGRT.GD.ZS" = c("Ingresos públicos", "% PIB", "Porcentaje del PIB"),
      "GC.XPN.TOTL.GD.ZS" = c("Gasto público", "% PIB", "Porcentaje del PIB"),
      "GC.TAX.TOTL.GD.ZS" = c("Recaudación tributaria", "% PIB", "Porcentaje del PIB"),
      "GC.NFN.TOTL.GD.ZS" = c("Saldo fiscal", "% PIB", "Porcentaje del PIB")
    ),
    
    precios_costes = list(
      "FP.CPI.TOTL.ZG" = c("Inflación (IPC)", "var. %", "Variación porcentual"),
      "NY.GDP.DEFL.KD.ZG" = c("Deflactor del PIB", "var. %", "Variación porcentual")
    ),
    
    indicadores_monetarios = list(
      "FM.LBL.BMNY.GD.ZS" = c("Masa monetaria (M2)", "% PIB", "Porcentaje del PIB"),
      "FS.AST.PRVT.GD.ZS" = c("Crédito al sector privado", "% PIB", "Porcentaje del PIB"),
      "FR.INR.DPST" = c("Tipo de interés de depósitos", "%", "Porcentaje")
    ),
    
    pro_memoria = list(
      "SP.POP.TOTL" = c("Población total", "mill.", "Millones de personas"),
      "SP.URB.TOTL.IN.ZS" = c("Población urbana", "% total", "Porcentaje del total"),
      "NY.GNP.PCAP.PP.CD" = c("INB per cápita (PPA)", "USD int.", "Dólares internacionales"),
      "SI.POV.GINI" = c("Índice de Gini", "", "Índice (0-100)"),
      "SP.DYN.LE00.IN" = c("Esperanza de vida al nacer", "años", "Años")
    )
  )
  
  # ============================================================================
  # INDICADORES DEL FMI (World Economic Outlook)
  # ============================================================================
  
  indicadores_fmi <- list(
    sector_real = list(
      "NGDP_RPCH" = c("PIB real", "var. %", "Variación porcentual"),
      "NGDPD" = c("PIB nominal", "mm. USD", "Miles de millones de dólares (USD)"),
      "NGDPDPC" = c("PIB per cápita", "USD", "Dólares (USD)"),
      "NID_NGDP" = c("Inversión total", "% PIB", "Porcentaje del PIB"),
      "NGSD_NGDP" = c("Ahorro nacional bruto", "% PIB", "Porcentaje del PIB")
    ),
    
    mercado_laboral = list(
      "LUR" = c("Tasa de desempleo", "%", "Porcentaje"),
      "LE" = c("Empleo", "mill.", "Millones de personas")
    ),
    
    sector_exterior = list(
      "BCA_NGDPD" = c("Cuenta corriente", "% PIB", "Porcentaje del PIB"),
      "TX_RPCH" = c("Volumen de exportaciones", "var. %", "Variación porcentual"),
      "TM_RPCH" = c("Volumen de importaciones", "var. %", "Variación porcentual")
    ),
    
    sector_publico = list(
      "GGXWDG_NGDP" = c("Deuda pública bruta", "% PIB", "Porcentaje del PIB"),
      "GGXCNL_NGDP" = c("Saldo primario", "% PIB", "Porcentaje del PIB"),
      "GGXONLB_NGDP" = c("Saldo público", "% PIB", "Porcentaje del PIB"),
      "GGR_NGDP" = c("Ingresos públicos", "% PIB", "Porcentaje del PIB"),
      "GGX_NGDP" = c("Gastos públicos", "% PIB", "Porcentaje del PIB")
    ),
    
    precios_costes = list(
      "PCPIPCH" = c("Inflación (IPC)", "var. %", "Variación porcentual"),
      "PCPIEPCH" = c("Inflación fin de período", "var. %", "Variación porcentual")
    ),
    
    indicadores_monetarios = list(
      "FPOLM_PA" = c("Tipo de interés de política monetaria", "%", "Porcentaje anual"),
      "FITB_PA" = c("Tipo de interés letras del Tesoro", "%", "Porcentaje anual"),
      "FILR_PA" = c("Tipo de interés de préstamos", "%", "Porcentaje anual"),
      "FIDR_PA" = c("Tipo de interés de depósitos", "%", "Porcentaje anual"),
      "FIMM_PA" = c("Tipo interbancario (money market)", "%", "Porcentaje anual"),
      "FM1_XDC" = c("Agregado monetario M1", "mill. UML", "Millones de moneda local"),
      "FM2_XDC" = c("Agregado monetario M2", "mill. UML", "Millones de moneda local"),
      "FM_3M_NUM" = c("Agregado monetario M3", "mill. UML", "Millones de moneda local"),
      "FASMB_XDC" = c("Base monetaria", "mill. UML", "Millones de moneda local"),
      "FIRA_PA" = c("Tipo de interés repo", "%", "Porcentaje anual")
    ),
    
    pro_memoria = list(
      "LP" = c("Población", "mill.", "Millones de personas"),
      "PPPPC" = c("PIB per cápita (PPA)", "USD int.", "Dólares internacionales")
    )
  )
  
  # ============================================================================
  # INDICADORES ADICIONALES DEL FMI (vía SDMX API)
  # ============================================================================
  
  indicadores_fmi_sdmx <- list(
    # CPI - Consumer Price Index
    cpi = list(
      flowref = "IMF.STA,CPI",
      indicadores = list(
        "PCPI_IX" = c("IPC (índice)", "índice", "Índice de precios al consumo"),
        "PCPI_PC_CP_A_PT" = c("Inflación anual IPC", "var. %", "Variación porcentual anual")
      )
    ),
    # IFS - International Financial Statistics (AMPLIADO CON INDICADORES MONETARIOS)
    ifs = list(
      flowref = "IMF.STA,IFS",
      indicadores = list(
        "EREER_IX" = c("Tipo de cambio efectivo real", "índice", "Índice (2010=100)"),
        "ENEER_IX" = c("Tipo de cambio efectivo nominal", "índice", "Índice (2010=100)"),
        "ENDA_XDC_USD_RATE" = c("Tipo de cambio nominal", "UML/USD", "Unidades de moneda local por USD"),
        "FITB_PA" = c("Tipo de interés letras del Tesoro", "%", "Porcentaje anual"),
        "FPOLM_PA" = c("Tipo de interés de política monetaria", "%", "Porcentaje anual"),
        "FILR_PA" = c("Tipo de interés de préstamos", "%", "Porcentaje anual"),
        "FIDR_PA" = c("Tipo de interés de depósitos", "%", "Porcentaje anual"),
        "FIMM_PA" = c("Tipo interbancario (money market)", "%", "Porcentaje anual"),
        "FM_3M_NUM" = c("Agregado monetario M3", "mill. UML", "Millones de moneda local"),
        "FM1_XDC" = c("Agregado monetario M1", "mill. UML", "Millones de moneda local"),
        "FM2_XDC" = c("Agregado monetario M2", "mill. UML", "Millones de moneda local"),
        "FASMB_XDC" = c("Base monetaria", "mill. UML", "Millones de moneda local"),
        "FIRA_PA" = c("Tipo de interés repo", "%", "Porcentaje anual")
      )
    ),
    # DOT - Direction of Trade Statistics
    dot = list(
      flowref = "IMF.STA,DOT",
      indicadores = list(
        "TXG_FOB_USD" = c("Exportaciones de bienes FOB", "mill. USD", "Millones de dólares (USD)"),
        "TMG_CIF_USD" = c("Importaciones de bienes CIF", "mill. USD", "Millones de dólares (USD)")
      )
    ),
    # GFS - Government Finance Statistics
    gfs = list(
      flowref = "IMF.STA,GFS",
      indicadores = list(
        "G1_XDC" = c("Ingresos totales del gobierno", "mill. UML", "Millones de moneda local"),
        "G2_XDC" = c("Gastos totales del gobierno", "mill. UML", "Millones de moneda local"),
        "GNFL_XDC" = c("Préstamo neto/Endeudamiento neto", "mill. UML", "Millones de moneda local")
      )
    )
  )
  
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
  
  # ============================================================================
  # FUNCIONES AUXILIARES
  # ============================================================================
  
  obtener_lista_paises <- function() {
    paises <- WDI::WDI_data$country
    
    paises |>
      dplyr::filter(region != "Aggregates") |>
      dplyr::select(iso2c, iso3c, country) |>
      dplyr::mutate(
        country_es = ifelse(
          country %in% names(diccionario_paises_es),
          diccionario_paises_es[country],
          country
        )
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
  # FUNCIÓN - Descarga Banco Mundial con reintentos
  # ============================================================================
  
  descargar_datos_bm <- function(pais_codigo, fecha_inicio, fecha_fin, 
                                 max_reintentos = 3, usar_cache = TRUE) {
    
    # 1. Preparar mapeos de forma vectorizada (más eficiente)
    mapeo_df <- purrr::map_dfr(names(indicadores_banco_mundial), function(cat) {
      purrr::map_dfr(names(indicadores_banco_mundial[[cat]]), function(cod) {
        tibble::tibble(
          indicador_codigo = cod,
          indicador_nombre = indicadores_banco_mundial[[cat]][[cod]][1],
          unidad_corta = indicadores_banco_mundial[[cat]][[cod]][2],
          unidad_larga = indicadores_banco_mundial[[cat]][[cod]][3]
        )
      })
    })
    
    codigos_indicadores <- mapeo_df$indicador_codigo
    
    # 2. Descargar con wbstats (más rápido y con caché integrado)
    datos <- NULL
    
    for (intento in 1:max_reintentos) {
      tryCatch({
        message(paste0("Intento ", intento, " de descarga del Banco Mundial..."))
        
        datos <- wbstats::wb_data(
          country = pais_codigo,
          indicator = codigos_indicadores,
          start_date = lubridate::year(fecha_inicio),
          end_date = lubridate::year(fecha_fin),
          return_wide = FALSE  # Devuelve formato largo directamente
        )
        
        if (!is.null(datos) && nrow(datos) > 0) break
        
      }, error = function(e) {
        message(paste0("Error en intento ", intento, ": ", e$message))
        if (intento < max_reintentos) Sys.sleep(2^intento)  # Backoff exponencial
      })
    }
    
    if (is.null(datos) || nrow(datos) == 0) {
      message("No se pudieron obtener datos del Banco Mundial.")
      return(NULL)
    }
    
    # 3. Join vectorizado en lugar de sapply (mucho más rápido)
    resultado <- datos |>
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
        prioridad_fuente = 3L,
        valor = dplyr::if_else(
          grepl("mill\\.", unidad_corta) & abs(valor) > 1e6,
          valor / 1e6,
          valor
        )
      )
    
    return(resultado)
  }
  
  # ============================================================================
  # FUNCIÓN FMI UNIFICADA - Descarga de todas las bases de datos disponibles
  # Usa imfapi para WEO, FM, BOP, CPI
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
    # 3. Mapeo completo de indicadores conocidos
    # -------------------------------------------------------------------------
    mapeo_indicadores <- list(
      # === SECTOR REAL ===
      "NGDP_RPCH" = c("PIB real", "var. %", "Variación porcentual"),
      "NGDP_R" = c("PIB real", "mm. UML", "Miles de millones de moneda local"),
      "NGDPD" = c("PIB nominal", "mm. USD", "Miles de millones de dólares (USD)"),
      "NGDP" = c("PIB nominal", "mm. UML", "Miles de millones de moneda local"),
      "NGDPDPC" = c("PIB per cápita", "USD", "Dólares (USD)"),
      "NGDPRPC" = c("PIB real per cápita", "UML", "Moneda local"),
      "NGDPRPPPPC" = c("PIB real per cápita (PPA)", "USD int.", "Dólares internacionales"),
      "NID_NGDP" = c("Inversión total", "% PIB", "Porcentaje del PIB"),
      "NGSD_NGDP" = c("Ahorro nacional bruto", "% PIB", "Porcentaje del PIB"),
      "FLIBOR6" = c("LIBOR 6 meses", "%", "Porcentaje anual"),
      
      # === MERCADO LABORAL ===
      "LUR" = c("Tasa de desempleo", "%", "Porcentaje"),
      "LE" = c("Empleo", "mill.", "Millones de personas"),
      
      # === SECTOR EXTERIOR ===
      "BCA" = c("Balanza cuenta corriente", "mm. USD", "Miles de millones de dólares (USD)"),
      "BCA_NGDPD" = c("Cuenta corriente", "% PIB", "Porcentaje del PIB"),
      "TX_RPCH" = c("Volumen de exportaciones", "var. %", "Variación porcentual"),
      "TM_RPCH" = c("Volumen de importaciones", "var. %", "Variación porcentual"),
      "TXG_RPCH" = c("Volumen de exportaciones de bienes", "var. %", "Variación porcentual"),
      "TMG_RPCH" = c("Volumen de importaciones de bienes", "var. %", "Variación porcentual"),
      "TXS_RPCH" = c("Volumen de exportaciones de servicios", "var. %", "Variación porcentual"),
      "TMS_RPCH" = c("Volumen de importaciones de servicios", "var. %", "Variación porcentual"),
      "TX" = c("Exportaciones de bienes y servicios", "mm. USD", "Miles de millones de dólares (USD)"),
      "TM" = c("Importaciones de bienes y servicios", "mm. USD", "Miles de millones de dólares (USD)"),
      "TXG_FOB_USD" = c("Exportaciones de bienes FOB", "mm. USD", "Miles de millones de dólares (USD)"),
      "TMG_CIF_USD" = c("Importaciones de bienes CIF", "mm. USD", "Miles de millones de dólares (USD)"),
      # BOP indicators - v8.0 AMPLIADO
      "BCA_BP6_USD" = c("Cuenta corriente (BOP)", "mm. USD", "Miles de millones de dólares (USD)"),
      "BFA_BP6_USD" = c("Cuenta financiera", "mm. USD", "Miles de millones de dólares (USD)"),
      "BFD_BP6_USD" = c("Inversión directa neta", "mm. USD", "Miles de millones de dólares (USD)"),
      "BFP_BP6_USD" = c("Inversión de cartera neta", "mm. USD", "Miles de millones de dólares (USD)"),
      "BGS_BP6_USD" = c("Balanza de bienes y servicios", "mm. USD", "Miles de millones de dólares (USD)"),
      "BXG_BP6_USD" = c("Exportaciones de bienes (BOP)", "mm. USD", "Miles de millones de dólares (USD)"),
      "BMG_BP6_USD" = c("Importaciones de bienes (BOP)", "mm. USD", "Miles de millones de dólares (USD)"),
      "BXGS_BP6_USD" = c("Exportaciones bienes y servicios (BOP)", "mm. USD", "Miles de millones de dólares (USD)"),
      "BMGS_BP6_USD" = c("Importaciones bienes y servicios (BOP)", "mm. USD", "Miles de millones de dólares (USD)"),
      "BIP_BP6_USD" = c("Rentas primarias netas", "mm. USD", "Miles de millones de dólares (USD)"),
      "BIS_BP6_USD" = c("Rentas secundarias netas", "mm. USD", "Miles de millones de dólares (USD)"),
      "BFDI_BP6_USD" = c("Inversión directa (activos)", "mm. USD", "Miles de millones de dólares (USD)"),
      "BFDL_BP6_USD" = c("Inversión directa (pasivos)", "mm. USD", "Miles de millones de dólares (USD)"),
      "BFPI_BP6_USD" = c("Inversión cartera (activos)", "mm. USD", "Miles de millones de dólares (USD)"),
      "BFPL_BP6_USD" = c("Inversión cartera (pasivos)", "mm. USD", "Miles de millones de dólares (USD)"),
      "BFOI_BP6_USD" = c("Otra inversión (activos)", "mm. USD", "Miles de millones de dólares (USD)"),
      "BFOL_BP6_USD" = c("Otra inversión (pasivos)", "mm. USD", "Miles de millones de dólares (USD)"),
      "BRA_BP6_USD" = c("Activos de reserva", "mm. USD", "Miles de millones de dólares (USD)"),
      
      # === SECTOR PÚBLICO ===
      "GGXWDG_NGDP" = c("Deuda pública bruta", "% PIB", "Porcentaje del PIB"),
      "GGXWDG" = c("Deuda pública bruta", "mm. UML", "Miles de millones de moneda local"),
      "GGXCNL_NGDP" = c("Saldo fiscal", "% PIB", "Porcentaje del PIB"),
      "GGXCNL" = c("Saldo fiscal", "mm. UML", "Miles de millones de moneda local"),
      "GGXONLB_NGDP" = c("Saldo primario", "% PIB", "Porcentaje del PIB"),
      "GGXONLB" = c("Saldo primario", "mm. UML", "Miles de millones de moneda local"),
      "GGR_NGDP" = c("Ingresos públicos", "% PIB", "Porcentaje del PIB"),
      "GGR" = c("Ingresos públicos", "mm. UML", "Miles de millones de moneda local"),
      "GGX_NGDP" = c("Gastos públicos", "% PIB", "Porcentaje del PIB"),
      "GGX" = c("Gastos públicos", "mm. UML", "Miles de millones de moneda local"),
      "GGXWDN_NGDP" = c("Deuda pública neta", "% PIB", "Porcentaje del PIB"),
      "GGXWDN" = c("Deuda pública neta", "mm. UML", "Miles de millones de moneda local"),
      # FM (Fiscal Monitor) - Indicadores estructurales
      "CAB_S13_POPGDP_PT" = c("Saldo estructural", "% PIB pot.", "Porcentaje del PIB potencial"),
      "CAPB_S13_POPGDP_PT" = c("Saldo primario estructural", "% PIB pot.", "Porcentaje del PIB potencial"),
      "EXP_S13_POPGDP_PT" = c("Gasto estructural", "% PIB pot.", "Porcentaje del PIB potencial"),
      "REV_S13_POPGDP_PT" = c("Ingreso estructural", "% PIB pot.", "Porcentaje del PIB potencial"),
      "G1_S13_POGDP_PT" = c("Ingresos del gobierno general", "% PIB pot.", "Porcentaje del PIB potencial"),
      "G2M_S13_POGDP_PT" = c("Gastos del gobierno general", "% PIB pot.", "Porcentaje del PIB potencial"),
      "G63G_S13_POGDP_PT" = c("Gasto en intereses bruto", "% PIB pot.", "Porcentaje del PIB potencial"),
      "G63N_S13_POGDP_PT" = c("Gasto en intereses neto", "% PIB pot.", "Porcentaje del PIB potencial"),
      "GNLB_S13_POGDP_PT" = c("Préstamo/endeudamiento neto", "% PIB pot.", "Porcentaje del PIB potencial"),
      "GPB_S13_POGDP_PT" = c("Saldo primario (FM)", "% PIB pot.", "Porcentaje del PIB potencial"),
      "GGSB" = c("Saldo estructural (WEO)", "mm. UML", "Miles de millones de moneda local"),
      "GGSB_NPGDP" = c("Saldo estructural (WEO)", "% PIB pot.", "Porcentaje del PIB potencial"),
      
      # === PRECIOS ===
      "PCPIPCH" = c("Inflación (IPC)", "var. %", "Variación porcentual"),
      "PCPIEPCH" = c("Inflación fin de período", "var. %", "Variación porcentual"),
      "PCPI_IX" = c("IPC (índice)", "índice", "Índice de precios al consumo"),
      "NGDP_D" = c("Deflactor del PIB", "índice", "Índice"),
      "PCPI_PC_CP_A_PT" = c("Inflación anual", "var. %", "Variación porcentual anual"),
      "PCPIH_PC_CP_A_PT" = c("Inflación (HICP)", "var. %", "Variación porcentual anual"),
      
      # === INDICADORES MONETARIOS/FINANCIEROS (IFS) ===
      "FPOLM_PA" = c("Tipo de interés de política monetaria", "%", "Porcentaje anual"),
      "FITB_PA" = c("Tipo de interés letras del Tesoro", "%", "Porcentaje anual"),
      "FILR_PA" = c("Tipo de interés de préstamos", "%", "Porcentaje anual"),
      "FIDR_PA" = c("Tipo de interés de depósitos", "%", "Porcentaje anual"),
      "FIMM_PA" = c("Tipo interbancario", "%", "Porcentaje anual"),
      "FM1_XDC" = c("Agregado monetario M1", "mm. UML", "Miles de millones de moneda local"),
      "FM2_XDC" = c("Agregado monetario M2", "mm. UML", "Miles de millones de moneda local"),
      "FMB_XDC" = c("Base monetaria", "mm. UML", "Miles de millones de moneda local"),
      "EREER_IX" = c("Tipo de cambio efectivo real (FMI)", "índice", "Índice (2010=100)"),
      "ENEER_IX" = c("Tipo de cambio efectivo nominal (FMI)", "índice", "Índice (2010=100)"),
      "ENDA_XDC_USD_RATE" = c("Tipo de cambio nominal", "UML/USD", "Unidades de moneda local por USD"),
      # IFS con códigos alternativos
      "NGDP_XDC" = c("PIB nominal (ML)", "mm. UML", "Miles de millones de moneda local"),
      "AIP_IX" = c("Producción industrial", "índice", "Índice de producción industrial"),
      "RADE_IX" = c("Reservas internacionales", "mm. USD", "Miles de millones de dólares (USD)"),
      
      # === FSI indicators ===
      "FSANL_PT" = c("Morosidad bancaria", "%", "Porcentaje de préstamos"),
      "FSERA_PT" = c("ROE bancario", "%", "Retorno sobre capital"),
      "FSKRC_PT" = c("Ratio de capital regulatorio", "%", "Capital / activos ponderados por riesgo"),
      
      # === PRO MEMORIA ===
      "LP" = c("Población", "mill.", "Millones de personas"),
      "PPPPC" = c("PIB per cápita (PPA)", "USD int.", "Dólares internacionales"),
      "PPPGDP" = c("PIB (PPA)", "mm. USD int.", "Miles de millones de dólares internacionales"),
      "PPPSH" = c("Participación en PIB mundial", "%", "Porcentaje del PIB mundial (PPA)")
    )
    
    # -------------------------------------------------------------------------
    # 4. Dataflows disponibles y sus configuraciones
    # -------------------------------------------------------------------------
    # Según diagnóstico:
    # - WEO: COUNTRY (ISO3), INDICATOR, FREQUENCY → Funciona bien
    # - FM: COUNTRY (ISO3), INDICATOR, FREQUENCY → Funciona bien  
    # - BOP: COUNTRY, BOP_ACCOUNTING_ENTRY, INDICATOR, UNIT, FREQUENCY → España no disponible
    # - CPI: COUNTRY, INDEX_TYPE, COICOP_1999, TYPE_OF_TRANSFORMATION, FREQUENCY
    # - IFS, FSI, DOT, GFS, GFSR: NO EXISTEN en la API actual
    
    datos_lista <- list()
    
    # -------------------------------------------------------------------------
    # 5. Descargar WEO (World Economic Outlook) - Principal fuente
    # -------------------------------------------------------------------------
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
      
      if (!is.null(datos_weo) && nrow(datos_weo) > 0) {
        datos_weo <- datos_weo |>
          dplyr::mutate(year = as.integer(substr(TIME_PERIOD, 1, 4))) |>
          dplyr::filter(year >= as.integer(anio_inicio) & year <= as.integer(anio_fin)) |>
          dplyr::mutate(
            indicador_codigo = INDICATOR,
            indicador_nombre = sapply(INDICATOR, function(x) {
              if (x %in% names(mapeo_indicadores)) mapeo_indicadores[[x]][1] else x
            }),
            unidad_corta = sapply(INDICATOR, function(x) {
              if (x %in% names(mapeo_indicadores)) mapeo_indicadores[[x]][2] else ""
            }),
            unidad_larga = sapply(INDICATOR, function(x) {
              if (x %in% names(mapeo_indicadores)) mapeo_indicadores[[x]][3] else ""
            }),
            valor = as.numeric(OBS_VALUE),
            fuente = "FMI (WEO)",
            prioridad_fuente = 1,
            country = pais_codigo_iso3,
            iso2c = pais_codigo_iso2
          ) |>
          dplyr::filter(!is.na(valor)) |>
          dplyr::select(country, year, iso2c, indicador_codigo, indicador_nombre,
                        unidad_corta, unidad_larga, valor, fuente, prioridad_fuente)
        
        if (nrow(datos_weo) > 0) {
          datos_lista[["WEO"]] <- datos_weo
          message(paste0("  ✓ FMI (WEO): ", nrow(datos_weo), " registros"))
        }
      }
    }, error = function(e) {
      # Silenciosamente ignorar errores
    })
    
    # -------------------------------------------------------------------------
    # 6. Descargar FM (Fiscal Monitor)
    # -------------------------------------------------------------------------
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
      
      if (!is.null(datos_fm) && nrow(datos_fm) > 0) {
        datos_fm <- datos_fm |>
          dplyr::mutate(year = as.integer(substr(TIME_PERIOD, 1, 4))) |>
          dplyr::filter(year >= as.integer(anio_inicio) & year <= as.integer(anio_fin)) |>
          dplyr::mutate(
            indicador_codigo = INDICATOR,
            indicador_nombre = sapply(INDICATOR, function(x) {
              if (x %in% names(mapeo_indicadores)) mapeo_indicadores[[x]][1] else x
            }),
            unidad_corta = sapply(INDICATOR, function(x) {
              if (x %in% names(mapeo_indicadores)) mapeo_indicadores[[x]][2] else ""
            }),
            unidad_larga = sapply(INDICATOR, function(x) {
              if (x %in% names(mapeo_indicadores)) mapeo_indicadores[[x]][3] else ""
            }),
            valor = as.numeric(OBS_VALUE),
            fuente = "FMI (FM)",
            prioridad_fuente = 1,
            country = pais_codigo_iso3,
            iso2c = pais_codigo_iso2
          ) |>
          dplyr::filter(!is.na(valor)) |>
          dplyr::select(country, year, iso2c, indicador_codigo, indicador_nombre,
                        unidad_corta, unidad_larga, valor, fuente, prioridad_fuente)
        
        if (nrow(datos_fm) > 0) {
          datos_lista[["FM"]] <- datos_fm
          message(paste0("  ✓ FMI (FM): ", nrow(datos_fm), " registros"))
        }
      }
    }, error = function(e) {
      # Silenciosamente ignorar errores
    })
    
    # -------------------------------------------------------------------------
    # 7. Descargar BOP (Balance of Payments) - Solo si el país está disponible
    # -------------------------------------------------------------------------
    tryCatch({
      # BOP tiene dimensiones: COUNTRY, BOP_ACCOUNTING_ENTRY, INDICATOR, UNIT, FREQUENCY
      # Dejamos las intermedias vacías para obtener todo
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
      
      if (!is.null(datos_bop) && nrow(datos_bop) > 0) {
        datos_bop <- datos_bop |>
          dplyr::mutate(year = as.integer(substr(TIME_PERIOD, 1, 4))) |>
          dplyr::filter(year >= as.integer(anio_inicio) & year <= as.integer(anio_fin)) |>
          dplyr::mutate(
            indicador_codigo = INDICATOR,
            indicador_nombre = sapply(INDICATOR, function(x) {
              if (x %in% names(mapeo_indicadores)) mapeo_indicadores[[x]][1] else x
            }),
            unidad_corta = sapply(INDICATOR, function(x) {
              if (x %in% names(mapeo_indicadores)) mapeo_indicadores[[x]][2] else ""
            }),
            unidad_larga = sapply(INDICATOR, function(x) {
              if (x %in% names(mapeo_indicadores)) mapeo_indicadores[[x]][3] else ""
            }),
            valor = as.numeric(OBS_VALUE),
            fuente = "FMI (BOP)",
            prioridad_fuente = 1,
            country = pais_codigo_iso3,
            iso2c = pais_codigo_iso2
          ) |>
          dplyr::filter(!is.na(valor)) |>
          dplyr::select(country, year, iso2c, indicador_codigo, indicador_nombre,
                        unidad_corta, unidad_larga, valor, fuente, prioridad_fuente)
        
        if (nrow(datos_bop) > 0) {
          datos_lista[["BOP"]] <- datos_bop
          message(paste0("  ✓ FMI (BOP): ", nrow(datos_bop), " registros"))
        }
      }
    }, error = function(e) {
      # BOP no disponible para este país - silenciosamente ignorar
    })
    
    # -------------------------------------------------------------------------
    # 8. Descargar CPI (Consumer Price Index) - v8.0 MEJORADO
    # -------------------------------------------------------------------------
    tryCatch({
      # CPI tiene dimensiones: COUNTRY, INDEX_TYPE, COICOP_1999, TYPE_OF_TRANSFORMATION, FREQUENCY
      datos_cpi <- suppressWarnings({
        imfapi::imf_get(
          dataflow_id = "CPI",
          dimensions = list(COUNTRY = pais_codigo_iso3, FREQUENCY = "A"),
          start_period = anio_inicio,
          end_period = anio_fin,
          progress = FALSE,
          max_tries = 2
        )
      })
      
      if (!is.null(datos_cpi) && nrow(datos_cpi) > 0) {
        # CPI puede tener diferentes columnas según la estructura
        posibles_cols <- c("INDICATOR", "INDEX_TYPE", "CLASSIFICATION", "TYPE_OF_TRANSFORMATION")
        col_indicator <- NULL
        for (col in posibles_cols) {
          if (col %in% names(datos_cpi)) {
            col_indicator <- col
            break
          }
        }
        
        if (!is.null(col_indicator)) {
          datos_cpi <- datos_cpi |>
            dplyr::mutate(year = as.integer(substr(TIME_PERIOD, 1, 4))) |>
            dplyr::filter(year >= as.integer(anio_inicio) & year <= as.integer(anio_fin)) |>
            dplyr::mutate(
              indicador_codigo = .data[[col_indicator]],
              # Mapeo mejorado para CPI con nombres descriptivos
              indicador_nombre = dplyr::case_when(
                grepl("PCPI_IX|^INDEX", .data[[col_indicator]], ignore.case = TRUE) ~ "IPC (índice)",
                grepl("HEADLINE", .data[[col_indicator]], ignore.case = TRUE) ~ "IPC general",
                grepl("CORE", .data[[col_indicator]], ignore.case = TRUE) ~ "IPC subyacente",
                grepl("PCPI_PC|PC_CP|PCT_CH", .data[[col_indicator]], ignore.case = TRUE) ~ "Inflación anual (CPI)",
                .data[[col_indicator]] %in% names(mapeo_indicadores) ~ sapply(.data[[col_indicator]], function(x) mapeo_indicadores[[x]][1]),
                TRUE ~ paste0("IPC - ", .data[[col_indicator]])
              ),
              unidad_corta = dplyr::case_when(
                grepl("INDEX|IX|HEADLINE|CORE", .data[[col_indicator]], ignore.case = TRUE) ~ "índice",
                grepl("PC|PCT|CH", .data[[col_indicator]], ignore.case = TRUE) ~ "var. %",
                TRUE ~ "índice"
              ),
              unidad_larga = dplyr::case_when(
                grepl("INDEX|IX|HEADLINE|CORE", .data[[col_indicator]], ignore.case = TRUE) ~ "Índice de precios al consumo",
                grepl("PC|PCT|CH", .data[[col_indicator]], ignore.case = TRUE) ~ "Variación porcentual anual",
                TRUE ~ "Índice de precios al consumo"
              ),
              valor = as.numeric(OBS_VALUE),
              fuente = "FMI (CPI)",
              prioridad_fuente = 1,
              country = pais_codigo_iso3,
              iso2c = pais_codigo_iso2
            ) |>
            dplyr::filter(!is.na(valor)) |>
            dplyr::select(country, year, iso2c, indicador_codigo, indicador_nombre,
                          unidad_corta, unidad_larga, valor, fuente, prioridad_fuente)
          
          if (nrow(datos_cpi) > 0) {
            datos_lista[["CPI"]] <- datos_cpi
            message(paste0("  ✓ FMI (CPI): ", nrow(datos_cpi), " registros"))
          }
        }
      }
    }, error = function(e) {
      # CPI no disponible - silenciosamente ignorar
    })
    
    # -------------------------------------------------------------------------
    # 9. Consolidar resultados
    # -------------------------------------------------------------------------
    if (length(datos_lista) == 0) {
      return(NULL)
    }
    
    resultado <- dplyr::bind_rows(datos_lista)
    
    # Eliminar duplicados (mismo indicador + año), manteniendo WEO/FM primero
    resultado <- resultado |>
      dplyr::mutate(
        orden_fuente = dplyr::case_when(
          grepl("WEO", fuente) ~ 1,
          grepl("FM", fuente) ~ 2,
          TRUE ~ 3
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
      
      # --- 5. PROCESAMIENTO ---
      datos_lista <- list()
      
      # Helper interno para no repetir código (DRY)
      procesar_indicador <- function(df, tipo_letra, cod_ind, nom_ind) {
        tryCatch({
          df |>
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
            # AQUÍ ESTÁ EL CAMBIO CLAVE: USAMOS LAS VARIABLES PRE-CALCULADAS
            dplyr::filter(year >= year_start_val & year <= year_end_val) |>
            dplyr::group_by(year) |>
            dplyr::summarise(valor = mean(valor, na.rm = TRUE), .groups = "drop") |>
            dplyr::mutate(
              country = pais_codigo_iso2,
              iso2c = pais_codigo_iso2,
              indicador_codigo = cod_ind,
              indicador_nombre = nom_ind,
              unidad_corta = "índice",
              unidad_larga = "Índice (2020=100)",
              fuente = "BIS",
              prioridad_fuente = 5  # Menor prioridad
            )
        }, error = function(e) NULL)
      }
      
      # REER (Real)
      datos_lista[["reer"]] <- procesar_indicador(eer_df, "R", "REER_BIS_BROAD", "Tipo de cambio efectivo real")
      # NEER (Nominal)
      datos_lista[["neer"]] <- procesar_indicador(eer_df, "N", "NEER_BIS_BROAD", "Tipo de cambio efectivo nominal")
      
      if (length(datos_lista) == 0) return(NULL)
      
      resultado <- dplyr::bind_rows(datos_lista) |>
        dplyr::select(country, year, iso2c, indicador_codigo, indicador_nombre,
                      unidad_corta, unidad_larga, valor, fuente, prioridad_fuente) |>
        dplyr::filter(!is.na(valor))
      
      return(resultado)
      
    }, error = function(e) {
      message("Error en la función: ", e$message)
      return(NULL)
    })
  }
  
  # Eurostat - Versión mejorada con indicadores específicos
  descargar_datos_eurostat <- function(pais_codigo_iso2, fecha_inicio, fecha_fin) {
    tryCatch({
      if (!requireNamespace("eurostat", quietly = TRUE)) {
        return(NULL)
      }
      
      if (!(pais_codigo_iso2 %in% paises_ue)) {
        message("País no es miembro de la UE")
        return(NULL)
      }
      
      datos_lista <- list()
      anio_inicio <- lubridate::year(fecha_inicio)
      anio_fin <- lubridate::year(fecha_fin)
      
      # --- PIB y componentes de demanda (nama_10_gdp) ---
      tryCatch({
        datos_gdp <- eurostat::get_eurostat(
          "nama_10_gdp",
          time_format = "num",
          filters = list(geo = pais_codigo_iso2)
        )
        if (!is.null(datos_gdp) && nrow(datos_gdp) > 0) {
          # Filtrar indicadores específicos y relevantes
          indicadores_gdp <- list(
            list(na_item = "B1GQ", unit = "CLV_PCH_PRE", nombre = "PIB real", unidad = "var. %", unidad_l = "Variación porcentual"),
            list(na_item = "P31_S14", unit = "CLV_PCH_PRE", nombre = "Consumo privado", unidad = "var. %", unidad_l = "Variación porcentual"),
            list(na_item = "P3_S13", unit = "CLV_PCH_PRE", nombre = "Consumo público", unidad = "var. %", unidad_l = "Variación porcentual"),
            list(na_item = "P51G", unit = "CLV_PCH_PRE", nombre = "Formación bruta de capital fijo", unidad = "var. %", unidad_l = "Variación porcentual"),
            list(na_item = "P6", unit = "CLV_PCH_PRE", nombre = "Exportaciones de bienes y servicios", unidad = "var. %", unidad_l = "Variación porcentual"),
            list(na_item = "P7", unit = "CLV_PCH_PRE", nombre = "Importaciones de bienes y servicios", unidad = "var. %", unidad_l = "Variación porcentual")
          )
          
          for (ind in indicadores_gdp) {
            datos_filtrado <- datos_gdp |>
              dplyr::filter(na_item == ind$na_item, unit == ind$unit) |>
              dplyr::filter(time >= anio_inicio & time <= anio_fin)
            
            if (nrow(datos_filtrado) > 0) {
              datos_filtrado <- datos_filtrado |>
                dplyr::mutate(
                  indicador_nombre = ind$nombre,
                  unidad_corta = ind$unidad,
                  unidad_larga = ind$unidad_l
                )
              datos_lista[[paste0("gdp_", ind$na_item, "_", ind$unit)]] <- datos_filtrado
            }
          }
        }
      }, error = function(e) {
        message("Eurostat PIB error: ", e$message)
      })
      
      # --- Desempleo (une_rt_a) ---
      tryCatch({
        datos_emp <- eurostat::get_eurostat(
          "une_rt_a",
          time_format = "num",
          filters = list(geo = pais_codigo_iso2)
        )
        if (!is.null(datos_emp) && nrow(datos_emp) > 0) {
          datos_filtrado <- datos_emp |>
            dplyr::filter(sex == "T", age == "Y15-74", unit == "PC_ACT") |>
            dplyr::filter(time >= anio_inicio & time <= anio_fin)
          
          if (nrow(datos_filtrado) > 0) {
            datos_filtrado <- datos_filtrado |>
              dplyr::mutate(
                indicador_nombre = "Tasa de desempleo",
                unidad_corta = "%",
                unidad_larga = "Porcentaje de la población activa"
              )
            datos_lista[["emp_total"]] <- datos_filtrado
          }
          
          datos_juvenil <- datos_emp |>
            dplyr::filter(sex == "T", age == "Y15-24", unit == "PC_ACT") |>
            dplyr::filter(time >= anio_inicio & time <= anio_fin)
          
          if (nrow(datos_juvenil) > 0) {
            datos_juvenil <- datos_juvenil |>
              dplyr::mutate(
                indicador_nombre = "Desempleo juvenil",
                unidad_corta = "%",
                unidad_larga = "Porcentaje 15-24 años"
              )
            datos_lista[["emp_juvenil"]] <- datos_juvenil
          }
        }
      }, error = function(e) {
        message("Eurostat empleo error: ", e$message)
      })
      
      # --- HICP Inflación (prc_hicp_aind) ---
      tryCatch({
        datos_hicp <- eurostat::get_eurostat(
          "prc_hicp_aind",
          time_format = "num",
          filters = list(geo = pais_codigo_iso2)
        )
        if (!is.null(datos_hicp) && nrow(datos_hicp) > 0) {
          datos_inflacion <- datos_hicp |>
            dplyr::filter(coicop == "CP00", unit == "RCH_A") |>
            dplyr::filter(time >= anio_inicio & time <= anio_fin)
          
          if (nrow(datos_inflacion) > 0) {
            datos_inflacion <- datos_inflacion |>
              dplyr::mutate(
                indicador_nombre = "HICP Inflación",
                unidad_corta = "var. %",
                unidad_larga = "Variación porcentual anual"
              )
            datos_lista[["hicp_general"]] <- datos_inflacion
          }
          
          datos_subyacente <- datos_hicp |>
            dplyr::filter(coicop == "TOT_X_NRG_FOOD", unit == "RCH_A") |>
            dplyr::filter(time >= anio_inicio & time <= anio_fin)
          
          if (nrow(datos_subyacente) > 0) {
            datos_subyacente <- datos_subyacente |>
              dplyr::mutate(
                indicador_nombre = "HICP subyacente",
                unidad_corta = "var. %",
                unidad_larga = "Excl. energía y alimentos"
              )
            datos_lista[["hicp_subyacente"]] <- datos_subyacente
          }
        }
      }, error = function(e) {
        message("Eurostat HICP error: ", e$message)
      })
      
      # --- CLU y productividad (nama_10_lp_ulc) ---
      tryCatch({
        datos_ulc <- eurostat::get_eurostat(
          "nama_10_lp_ulc",
          time_format = "num",
          filters = list(geo = pais_codigo_iso2)
        )
        if (!is.null(datos_ulc) && nrow(datos_ulc) > 0) {
          datos_clu <- datos_ulc |>
            dplyr::filter(na_item == "NULC_PER", unit == "PCH_PRE") |>
            dplyr::filter(time >= anio_inicio & time <= anio_fin)
          
          if (nrow(datos_clu) > 0) {
            datos_clu <- datos_clu |>
              dplyr::mutate(
                indicador_nombre = "CLU nominales",
                unidad_corta = "var. %",
                unidad_larga = "Variación porcentual"
              )
            datos_lista[["clu"]] <- datos_clu
          }
          
          datos_prod <- datos_ulc |>
            dplyr::filter(na_item == "RLPR_HW", unit == "PCH_PRE") |>
            dplyr::filter(time >= anio_inicio & time <= anio_fin)
          
          if (nrow(datos_prod) > 0) {
            datos_prod <- datos_prod |>
              dplyr::mutate(
                indicador_nombre = "Productividad/hora",
                unidad_corta = "var. %",
                unidad_larga = "Variación porcentual"
              )
            datos_lista[["productividad"]] <- datos_prod
          }
        }
      }, error = function(e) {
        message("Eurostat ULC error: ", e$message)
      })
      
      # --- Deuda y déficit público (gov_10dd_edpt1) ---
      tryCatch({
        datos_fiscal <- eurostat::get_eurostat(
          "gov_10dd_edpt1",
          time_format = "num",
          filters = list(geo = pais_codigo_iso2)
        )
        if (!is.null(datos_fiscal) && nrow(datos_fiscal) > 0) {
          datos_deuda <- datos_fiscal |>
            dplyr::filter(sector == "S13", na_item == "GD", unit == "PC_GDP") |>
            dplyr::filter(time >= anio_inicio & time <= anio_fin)
          
          if (nrow(datos_deuda) > 0) {
            datos_deuda <- datos_deuda |>
              dplyr::mutate(
                indicador_nombre = "Deuda pública",
                unidad_corta = "% PIB",
                unidad_larga = "Porcentaje del PIB"
              )
            datos_lista[["deuda_publica"]] <- datos_deuda
          }
          
          datos_saldo <- datos_fiscal |>
            dplyr::filter(sector == "S13", na_item == "B9", unit == "PC_GDP") |>
            dplyr::filter(time >= anio_inicio & time <= anio_fin)
          
          if (nrow(datos_saldo) > 0) {
            datos_saldo <- datos_saldo |>
              dplyr::mutate(
                indicador_nombre = "Saldo público",
                unidad_corta = "% PIB",
                unidad_larga = "Porcentaje del PIB"
              )
            datos_lista[["saldo_publico"]] <- datos_saldo
          }
        }
      }, error = function(e) {
        message("Eurostat fiscal error: ", e$message)
      })
      
      if (length(datos_lista) == 0) {
        return(NULL)
      }
      
      resultado <- dplyr::bind_rows(datos_lista) |>
        dplyr::filter(time >= anio_inicio & time <= anio_fin) |>
        dplyr::mutate(
          country = pais_codigo_iso2,
          year = as.integer(time),
          iso2c = pais_codigo_iso2,
          indicador_codigo = indicador_nombre,
          fuente = "Eurostat",
          prioridad_fuente = 2  # Prioridad: 2 = Eurostat
        ) |>
        dplyr::select(country, year, iso2c, indicador_codigo, indicador_nombre,
                      unidad_corta, unidad_larga, valor = values, fuente, prioridad_fuente) |>
        dplyr::filter(!is.na(valor)) |>
        dplyr::distinct()
      
      return(resultado)
    }, error = function(e) {
      message("Error descargando datos de Eurostat: ", e$message)
      return(NULL)
    })
  }
  
  # OMC - Organización Mundial del Comercio
  descargar_datos_omc <- function(pais_codigo_iso2, fecha_inicio, fecha_fin) {
    tryCatch({
      if (!requireNamespace("wtor", quietly = TRUE)) {
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
      
      codigos_ue <- c(
        "AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN",
        "ESP", "FRA", "DEU", "ITA", "GRC", "HUN", "SWE", "IRL", "LVA",
        "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "ROM"
      )
      
      if (pais_codigo_iso3 %in% codigos_ue) {
        codigo_pais <- "918"
      } else {
        tryCatch({
          countries_wto <- wtor::get_countries()
          codigo_pais <- countries_wto |>
            dplyr::filter(iso3_code == pais_codigo_iso3) |>
            dplyr::pull(country_code)
          
          if (length(codigo_pais) == 0) {
            message("País no encontrado en OMC")
            return(NULL)
          }
        }, error = function(e) {
          message("Error obteniendo código OMC: ", e$message)
          return(NULL)
        })
      }
      
      datos_aranceles <- tryCatch({
        datos1 <- wtor::get_timeseries_data(
          code = "TP_A_0010",
          reporting_economies = codigo_pais,
          time_period = "all"
        )
        
        datos2 <- wtor::get_timeseries_data(
          code = "TP_A_0030",
          reporting_economies = codigo_pais,
          time_period = "all"
        )
        
        if (!is.null(datos1) && !is.null(datos2)) {
          dplyr::bind_rows(datos1, datos2)
        } else if (!is.null(datos1)) {
          datos1
        } else if (!is.null(datos2)) {
          datos2
        } else {
          NULL
        }
      }, error = function(e) {
        message("Error descargando datos OMC: ", e$message)
        return(NULL)
      })
      
      if (is.null(datos_aranceles) || nrow(datos_aranceles) == 0) {
        return(NULL)
      }
      
      if (!"year" %in% names(datos_aranceles) || !"value" %in% names(datos_aranceles)) {
        message("Estructura de datos OMC inesperada")
        return(NULL)
      }
      
      resultado <- datos_aranceles |>
        dplyr::mutate(year = as.integer(year)) |>
        dplyr::filter(year >= lubridate::year(fecha_inicio) &
                        year <= lubridate::year(fecha_fin))
      
      if ("indicatorcode" %in% names(resultado)) {
        resultado <- resultado |>
          dplyr::mutate(
            indicador_nombre = dplyr::case_when(
              indicatorcode == "TP_A_0010" ~ "Media simple del arancel NMF",
              indicatorcode == "TP_A_0030" ~ "Media ponderada del arancel NMF",
              TRUE ~ as.character(indicatorcode)
            ),
            indicador_codigo = indicatorcode
          )
      } else if ("indicator" %in% names(resultado)) {
        resultado <- resultado |>
          dplyr::mutate(
            indicador_nombre = indicator,
            indicador_codigo = indicator
          )
      } else {
        message("No se encontró columna de indicador en datos OMC")
        return(NULL)
      }
      
      resultado <- resultado |>
        dplyr::mutate(
          country = pais_codigo_iso2,
          iso2c = pais_codigo_iso2,
          unidad_corta = "%",
          unidad_larga = "Porcentaje",
          fuente = "OMC",
          prioridad_fuente = 4  # Prioridad: 4 = OMC
        ) |>
        dplyr::select(country, year, iso2c, indicador_codigo, indicador_nombre,
                      unidad_corta, unidad_larga, valor = value, fuente, prioridad_fuente) |>
        dplyr::filter(!is.na(valor))
      
      return(resultado)
    }, error = function(e) {
      message("Error descargando datos de OMC: ", e$message)
      return(NULL)
    })
  }
  
  # ============================================================================
  # OCDE - Datos de la Organización para la Cooperación y el Desarrollo (v8.0)
  # NOTA: La API antigua (stats.oecd.org) fue deprecada. Ahora usamos API REST directa.
  # ============================================================================
  
  # Lista de países miembros de la OCDE (códigos ISO3)
  paises_ocde <- c(
    "AUS", "AUT", "BEL", "CAN", "CHL", "COL", "CRI", "CZE", "DNK", "EST",
    "FIN", "FRA", "DEU", "GRC", "HUN", "ISL", "IRL", "ISR", "ITA", "JPN",
    "KOR", "LVA", "LTU", "LUX", "MEX", "NLD", "NZL", "NOR", "POL", "PRT",
    "SVK", "SVN", "ESP", "SWE", "CHE", "TUR", "GBR", "USA"
  )
  
  descargar_datos_ocde <- function(pais_codigo_iso2, fecha_inicio, fecha_fin) {
    tryCatch({
      # Convertir ISO2 a ISO3
      pais_codigo_iso3 <- if (pais_codigo_iso2 %in% names(mapeo_iso2_iso3)) {
        mapeo_iso2_iso3[pais_codigo_iso2]
      } else {
        tryCatch(
          countrycode::countrycode(pais_codigo_iso2, "iso2c", "iso3c"),
          error = function(e) NULL
        )
      }
      
      if (is.null(pais_codigo_iso3) || !(pais_codigo_iso3 %in% paises_ocde)) {
        return(NULL)
      }
      
      anio_inicio <- lubridate::year(fecha_inicio)
      anio_fin <- lubridate::year(fecha_fin)
      datos_lista <- list()
      
      # Usar API SDMX de OCDE (nueva API)
      base_url <- "https://sdmx.oecd.org/public/rest/data"
      
      # --- 1. PIB real anual (SNA_TABLE1) ---
      tryCatch({
        # URL para PIB a precios constantes, variación anual
        url_gdp <- paste0(base_url, "/OECD.SDD.NAD,DSD_NAMAIN1@DF_TABLE1_EXPENDITURE,1.0/",
                          pais_codigo_iso3, ".A.B1GQ.V._Z.GY.USD_PPP?",
                          "startPeriod=", anio_inicio, "&endPeriod=", anio_fin)
        
        resp <- httr::GET(url_gdp, httr::timeout(30), 
                          httr::add_headers(Accept = "application/vnd.sdmx.data+csv;version=2.0.0"))
        
        if (httr::status_code(resp) == 200) {
          texto <- httr::content(resp, "text", encoding = "UTF-8")
          if (nchar(texto) > 100) {
            datos_gdp <- tryCatch({
              read.csv(text = texto, stringsAsFactors = FALSE)
            }, error = function(e) NULL)
            
            if (!is.null(datos_gdp) && nrow(datos_gdp) > 0 && "OBS_VALUE" %in% names(datos_gdp)) {
              datos_procesados <- datos_gdp |>
                dplyr::mutate(
                  year = as.integer(TIME_PERIOD),
                  indicador_codigo = "OCDE_GDP",
                  indicador_nombre = "PIB real (OCDE)",
                  unidad_corta = "var. %",
                  unidad_larga = "Variación porcentual anual",
                  valor = as.numeric(OBS_VALUE),
                  country = pais_codigo_iso3,
                  iso2c = pais_codigo_iso2,
                  fuente = "OCDE",
                  prioridad_fuente = 2
                ) |>
                dplyr::filter(!is.na(valor)) |>
                dplyr::select(country, year, iso2c, indicador_codigo, indicador_nombre,
                              unidad_corta, unidad_larga, valor, fuente, prioridad_fuente)
              
              if (nrow(datos_procesados) > 0) {
                datos_lista[["gdp"]] <- datos_procesados
              }
            }
          }
        }
      }, error = function(e) {
        # Silenciosamente ignorar errores de PIB
      })
      
      # --- 2. Tasa de desempleo (KEI) ---
      tryCatch({
        url_une <- paste0(base_url, "/OECD.SDD.TPS,DSD_KEI@DF_KEI,4.0/",
                          pais_codigo_iso3, ".A.LRUNTTTT...",
                          "?startPeriod=", anio_inicio, "&endPeriod=", anio_fin)
        
        resp <- httr::GET(url_une, httr::timeout(30),
                          httr::add_headers(Accept = "application/vnd.sdmx.data+csv;version=2.0.0"))
        
        if (httr::status_code(resp) == 200) {
          texto <- httr::content(resp, "text", encoding = "UTF-8")
          if (nchar(texto) > 100) {
            datos_une <- tryCatch({
              read.csv(text = texto, stringsAsFactors = FALSE)
            }, error = function(e) NULL)
            
            if (!is.null(datos_une) && nrow(datos_une) > 0 && "OBS_VALUE" %in% names(datos_une)) {
              datos_procesados <- datos_une |>
                dplyr::mutate(
                  year = as.integer(TIME_PERIOD),
                  indicador_codigo = "OCDE_UNE",
                  indicador_nombre = "Tasa de desempleo (OCDE)",
                  unidad_corta = "%",
                  unidad_larga = "Porcentaje de la población activa",
                  valor = as.numeric(OBS_VALUE),
                  country = pais_codigo_iso3,
                  iso2c = pais_codigo_iso2,
                  fuente = "OCDE",
                  prioridad_fuente = 2
                ) |>
                dplyr::filter(!is.na(valor)) |>
                dplyr::select(country, year, iso2c, indicador_codigo, indicador_nombre,
                              unidad_corta, unidad_larga, valor, fuente, prioridad_fuente)
              
              if (nrow(datos_procesados) > 0) {
                datos_lista[["une"]] <- datos_procesados
              }
            }
          }
        }
      }, error = function(e) {
        # Silenciosamente ignorar errores de desempleo
      })
      
      # --- 3. Inflación (PRICES_CPI) ---
      tryCatch({
        url_cpi <- paste0(base_url, "/OECD.SDD.TPS,DSD_PRICES@DF_PRICES_ALL,1.0/",
                          pais_codigo_iso3, ".A.CPI.PA._T.N.GY",
                          "?startPeriod=", anio_inicio, "&endPeriod=", anio_fin)
        
        resp <- httr::GET(url_cpi, httr::timeout(30),
                          httr::add_headers(Accept = "application/vnd.sdmx.data+csv;version=2.0.0"))
        
        if (httr::status_code(resp) == 200) {
          texto <- httr::content(resp, "text", encoding = "UTF-8")
          if (nchar(texto) > 100) {
            datos_cpi <- tryCatch({
              read.csv(text = texto, stringsAsFactors = FALSE)
            }, error = function(e) NULL)
            
            if (!is.null(datos_cpi) && nrow(datos_cpi) > 0 && "OBS_VALUE" %in% names(datos_cpi)) {
              datos_procesados <- datos_cpi |>
                dplyr::mutate(
                  year = as.integer(TIME_PERIOD),
                  indicador_codigo = "OCDE_CPI",
                  indicador_nombre = "Inflación (OCDE)",
                  unidad_corta = "var. %",
                  unidad_larga = "Variación porcentual anual",
                  valor = as.numeric(OBS_VALUE),
                  country = pais_codigo_iso3,
                  iso2c = pais_codigo_iso2,
                  fuente = "OCDE",
                  prioridad_fuente = 2
                ) |>
                dplyr::filter(!is.na(valor)) |>
                dplyr::select(country, year, iso2c, indicador_codigo, indicador_nombre,
                              unidad_corta, unidad_larga, valor, fuente, prioridad_fuente)
              
              if (nrow(datos_procesados) > 0) {
                datos_lista[["cpi"]] <- datos_procesados
              }
            }
          }
        }
      }, error = function(e) {
        # Silenciosamente ignorar errores de CPI
      })
      
      # --- 4. Tipo de interés a corto plazo (MEI) ---
      tryCatch({
        url_ir <- paste0(base_url, "/OECD.SDD.TPS,DSD_KEI@DF_KEI,4.0/",
                         pais_codigo_iso3, ".A.IR3TIB...",
                         "?startPeriod=", anio_inicio, "&endPeriod=", anio_fin)
        
        resp <- httr::GET(url_ir, httr::timeout(30),
                          httr::add_headers(Accept = "application/vnd.sdmx.data+csv;version=2.0.0"))
        
        if (httr::status_code(resp) == 200) {
          texto <- httr::content(resp, "text", encoding = "UTF-8")
          if (nchar(texto) > 100) {
            datos_ir <- tryCatch({
              read.csv(text = texto, stringsAsFactors = FALSE)
            }, error = function(e) NULL)
            
            if (!is.null(datos_ir) && nrow(datos_ir) > 0 && "OBS_VALUE" %in% names(datos_ir)) {
              datos_procesados <- datos_ir |>
                dplyr::mutate(
                  year = as.integer(TIME_PERIOD),
                  indicador_codigo = "OCDE_IR3M",
                  indicador_nombre = "Tipo de interés a 3 meses (OCDE)",
                  unidad_corta = "%",
                  unidad_larga = "Porcentaje anual",
                  valor = as.numeric(OBS_VALUE),
                  country = pais_codigo_iso3,
                  iso2c = pais_codigo_iso2,
                  fuente = "OCDE",
                  prioridad_fuente = 2
                ) |>
                dplyr::filter(!is.na(valor)) |>
                dplyr::select(country, year, iso2c, indicador_codigo, indicador_nombre,
                              unidad_corta, unidad_larga, valor, fuente, prioridad_fuente)
              
              if (nrow(datos_procesados) > 0) {
                datos_lista[["ir"]] <- datos_procesados
              }
            }
          }
        }
      }, error = function(e) {
        # Silenciosamente ignorar errores de tipos de interés
      })
      
      if (length(datos_lista) == 0) return(NULL)
      
      resultado <- dplyr::bind_rows(datos_lista)
      return(resultado)
      
    }, error = function(e) {
      return(NULL)
    })
  }
  
  # ============================================================================
  # DBnomics - Agregador de múltiples fuentes (NUEVO v8.0)
  # ============================================================================
  
  descargar_datos_dbnomics <- function(pais_codigo_iso2, fecha_inicio, fecha_fin) {
    tryCatch({
      if (!requireNamespace("rdbnomics", quietly = TRUE)) {
        message("Paquete rdbnomics no disponible")
        return(NULL)
      }
      
      # Convertir ISO2 a ISO3
      pais_codigo_iso3 <- if (pais_codigo_iso2 %in% names(mapeo_iso2_iso3)) {
        mapeo_iso2_iso3[pais_codigo_iso2]
      } else {
        tryCatch(
          countrycode::countrycode(pais_codigo_iso2, "iso2c", "iso3c"),
          error = function(e) NULL
        )
      }
      
      if (is.null(pais_codigo_iso3)) {
        message("No se pudo convertir código de país para DBnomics")
        return(NULL)
      }
      
      anio_inicio <- lubridate::year(fecha_inicio)
      anio_fin <- lubridate::year(fecha_fin)
      datos_lista <- list()
      
      # --- 1. PIB desde Eurostat via DBnomics ---
      tryCatch({
        serie_id <- paste0("Eurostat/namq_10_gdp/A.CLV_PCH_PRE.SCA.B1GQ.", pais_codigo_iso2)
        datos_pib <- rdbnomics::rdb(ids = serie_id)
        
        if (!is.null(datos_pib) && nrow(datos_pib) > 0) {
          datos_pib <- datos_pib |>
            dplyr::mutate(year = as.integer(substr(period, 1, 4))) |>
            dplyr::filter(year >= anio_inicio & year <= anio_fin) |>
            dplyr::mutate(
              indicador_codigo = "DBN_GDP",
              indicador_nombre = "PIB real (DBnomics)",
              unidad_corta = "var. %",
              unidad_larga = "Variación porcentual anual",
              valor = as.numeric(value),
              country = pais_codigo_iso3,
              iso2c = pais_codigo_iso2,
              fuente = "DBnomics",
              prioridad_fuente = 6
            ) |>
            dplyr::filter(!is.na(valor)) |>
            dplyr::select(country, year, iso2c, indicador_codigo, indicador_nombre,
                          unidad_corta, unidad_larga, valor, fuente, prioridad_fuente)
          
          if (nrow(datos_pib) > 0) {
            datos_lista[["pib"]] <- datos_pib
          }
        }
      }, error = function(e) {
        message("DBnomics PIB error: ", e$message)
      })
      
      # --- 2. Inflación desde IMF via DBnomics ---
      tryCatch({
        serie_id <- paste0("IMF/CPI/A.", pais_codigo_iso3, ".PCPI_IX")
        datos_cpi <- rdbnomics::rdb(ids = serie_id)
        
        if (!is.null(datos_cpi) && nrow(datos_cpi) > 0) {
          datos_cpi <- datos_cpi |>
            dplyr::mutate(year = as.integer(substr(period, 1, 4))) |>
            dplyr::filter(year >= anio_inicio & year <= anio_fin) |>
            dplyr::mutate(
              indicador_codigo = "DBN_CPI",
              indicador_nombre = "IPC (DBnomics)",
              unidad_corta = "índice",
              unidad_larga = "Índice de precios al consumo",
              valor = as.numeric(value),
              country = pais_codigo_iso3,
              iso2c = pais_codigo_iso2,
              fuente = "DBnomics",
              prioridad_fuente = 6
            ) |>
            dplyr::filter(!is.na(valor)) |>
            dplyr::select(country, year, iso2c, indicador_codigo, indicador_nombre,
                          unidad_corta, unidad_larga, valor, fuente, prioridad_fuente)
          
          if (nrow(datos_cpi) > 0) {
            datos_lista[["cpi"]] <- datos_cpi
          }
        }
      }, error = function(e) {
        message("DBnomics CPI error: ", e$message)
      })
      
      # --- 3. Desempleo desde OECD via DBnomics ---
      tryCatch({
        serie_id <- paste0("OECD/MEI/", pais_codigo_iso3, ".LRHUTTTT.STSA.A")
        datos_une <- rdbnomics::rdb(ids = serie_id)
        
        if (!is.null(datos_une) && nrow(datos_une) > 0) {
          datos_une <- datos_une |>
            dplyr::mutate(year = as.integer(substr(period, 1, 4))) |>
            dplyr::filter(year >= anio_inicio & year <= anio_fin) |>
            dplyr::mutate(
              indicador_codigo = "DBN_UNE",
              indicador_nombre = "Tasa de desempleo (DBnomics)",
              unidad_corta = "%",
              unidad_larga = "Porcentaje de la población activa",
              valor = as.numeric(value),
              country = pais_codigo_iso3,
              iso2c = pais_codigo_iso2,
              fuente = "DBnomics",
              prioridad_fuente = 6
            ) |>
            dplyr::filter(!is.na(valor)) |>
            dplyr::select(country, year, iso2c, indicador_codigo, indicador_nombre,
                          unidad_corta, unidad_larga, valor, fuente, prioridad_fuente)
          
          if (nrow(datos_une) > 0) {
            datos_lista[["une"]] <- datos_une
          }
        }
      }, error = function(e) {
        message("DBnomics UNE error: ", e$message)
      })
      
      # --- 4. Tipos de interés desde BIS via DBnomics ---
      tryCatch({
        serie_id <- paste0("BIS/WS_CBPOL/D.", pais_codigo_iso2, "..")
        datos_ir <- rdbnomics::rdb(ids = serie_id)
        
        if (!is.null(datos_ir) && nrow(datos_ir) > 0) {
          # Agregar a datos anuales (promedio)
          datos_ir <- datos_ir |>
            dplyr::mutate(year = as.integer(substr(period, 1, 4))) |>
            dplyr::filter(year >= anio_inicio & year <= anio_fin) |>
            dplyr::group_by(year) |>
            dplyr::summarise(valor = mean(as.numeric(value), na.rm = TRUE), .groups = "drop") |>
            dplyr::mutate(
              indicador_codigo = "DBN_IR",
              indicador_nombre = "Tipo de interés oficial (DBnomics)",
              unidad_corta = "%",
              unidad_larga = "Porcentaje anual",
              country = pais_codigo_iso3,
              iso2c = pais_codigo_iso2,
              fuente = "DBnomics",
              prioridad_fuente = 6
            ) |>
            dplyr::filter(!is.na(valor)) |>
            dplyr::select(country, year, iso2c, indicador_codigo, indicador_nombre,
                          unidad_corta, unidad_larga, valor, fuente, prioridad_fuente)
          
          if (nrow(datos_ir) > 0) {
            datos_lista[["ir"]] <- datos_ir
          }
        }
      }, error = function(e) {
        message("DBnomics IR error: ", e$message)
      })
      
      if (length(datos_lista) == 0) return(NULL)
      
      resultado <- dplyr::bind_rows(datos_lista)
      return(resultado)
      
    }, error = function(e) {
      message("Error general DBnomics: ", e$message)
      return(NULL)
    })
  }
  
  # ============================================================================
  # FUNCIÓN COMBINADA - v8.0 (con deduplicación mejorada)
  # ============================================================================
  
  # Función auxiliar para obtener nombre canónico de indicadores equivalentes
  obtener_nombre_canonico <- function(nombre_indicador) {
    # Mapeo de indicadores equivalentes entre fuentes - v8.0 AMPLIADO
    equivalencias <- list(
      "PIB real" = c("PIB real", "PIB real (OCDE)", "PIB real (DBnomics)", "GDP constant prices"),
      "PIB nominal" = c("PIB nominal", "GDP current prices"),
      "Tasa de desempleo" = c("Tasa de desempleo", "Tasa de desempleo (OCDE)", "Tasa de desempleo (DBnomics)", "Desempleo nacional"),
      "Inflación (IPC)" = c("Inflación (IPC)", "Inflación (OCDE)", "HICP Inflación", "Inflación anual IPC", "Inflación anual (CPI)"),
      "IPC" = c("IPC (índice)", "IPC general", "IPC (DBnomics)", "CPI_INDEX", "HEADLINE"),
      "Deuda pública" = c("Deuda pública", "Deuda pública (% PIB)", "Deuda pública bruta"),
      "Saldo público" = c("Saldo público", "Saldo fiscal (% PIB)", "Saldo fiscal"),
      "Exportaciones" = c("Exportaciones de bienes y servicios", "Exportaciones de bienes FOB", "Exportaciones", "Exportaciones (OCDE)"),
      "Importaciones" = c("Importaciones de bienes y servicios", "Importaciones de bienes CIF", "Importaciones", "Importaciones (OCDE)"),
      "Cuenta corriente" = c("Balanza por cuenta corriente", "Balanza cuenta corriente", "Cuenta corriente", 
                             "Cuenta corriente (BOP)", "Cuenta corriente (OCDE)"),
      "Tipo de cambio efectivo real" = c("Tipo de cambio efectivo real", "Tipo de cambio efectivo real (FMI)", 
                                         "REER_BIS_BROAD", "EREER_IX"),
      "Tipo de cambio efectivo nominal" = c("Tipo de cambio efectivo nominal", "Tipo de cambio efectivo nominal (FMI)",
                                            "NEER_BIS_BROAD", "ENEER_IX")
    )
    
    for (nombre_canonico in names(equivalencias)) {
      if (nombre_indicador %in% equivalencias[[nombre_canonico]]) {
        return(nombre_canonico)
      }
    }
    return(nombre_indicador)
  }
  
  # Función de deduplicación por prioridad de fuente
  deduplicar_por_prioridad <- function(datos) {
    if (is.null(datos) || nrow(datos) == 0) return(datos)
    
    # Si no existe prioridad_fuente, asignar valor alto (baja prioridad)
    if (!"prioridad_fuente" %in% names(datos)) {
      datos$prioridad_fuente <- 99
    }
    
    # Añadir nombre canónico para agrupar indicadores equivalentes
    datos <- datos |>
      dplyr::mutate(
        nombre_canonico = sapply(indicador_nombre, obtener_nombre_canonico)
      )
    
    # Contar registros antes de deduplicar
    n_antes <- nrow(datos)
    
    # Deduplicar: mantener el registro con menor prioridad_fuente (mayor prioridad)
    # para cada combinación de nombre_canonico + year + unidad_corta
    datos_dedup <- datos |>
      dplyr::arrange(nombre_canonico, year, unidad_corta, prioridad_fuente) |>
      dplyr::group_by(nombre_canonico, year, unidad_corta) |>
      dplyr::slice(1) |>
      dplyr::ungroup() |>
      dplyr::select(-nombre_canonico)
    
    n_despues <- nrow(datos_dedup)
    n_eliminados <- n_antes - n_despues
    
    if (n_eliminados > 0) {
      message(paste0("   ℹ Deduplicación: ", n_eliminados, " registros duplicados eliminados"))
    }
    
    return(datos_dedup)
  }
  
  # Función para normalizar unidades y valores
  normalizar_valores <- function(datos) {
    if (is.null(datos) || nrow(datos) == 0) return(datos)
    
    # Usar stringr para detección más robusta en vectores
    datos <- datos |>
      dplyr::mutate(
        # Detectar tipo de unidad
        es_miles_millones = stringr::str_detect(unidad_corta, stringr::regex("mm\\.", ignore_case = TRUE)),
        es_millones = stringr::str_detect(unidad_corta, stringr::regex("mill\\.", ignore_case = TRUE)) & !es_miles_millones,
        es_poblacion_empleo = stringr::str_detect(indicador_nombre, stringr::regex("Población|Empleo|población activa", ignore_case = TRUE)),
        
        # Aplicar conversiones
        valor = dplyr::case_when(
          # === MILES DE MILLONES (mm.) ===
          # El WEO devuelve valores en millones de UML, convertir a miles de millones
          es_miles_millones & abs(valor) > 1e8 ~ valor / 1e6,
          es_miles_millones & abs(valor) > 1e5 & abs(valor) <= 1e8 ~ valor / 1e3,
          
          # === MILLONES (mill.) para población y empleo ===
          es_millones & es_poblacion_empleo & abs(valor) > 1e6 ~ valor / 1e6,
          
          # === MILLONES (mill.) para otros indicadores ===
          es_millones & abs(valor) > 1e9 ~ valor / 1e6,
          
          # Sin cambios para el resto
          TRUE ~ valor
        )
      ) |>
      dplyr::select(-es_miles_millones, -es_millones, -es_poblacion_empleo)
    
    return(datos)
  }
  
  # ============================================================================
  # FUNCIÓN PARA DETECTAR CATEGORÍA DE INDICADOR
  # (usada tanto en descargar_datos_combinados como en exportar_a_excel)
  # ============================================================================
  
  detectar_categoria_indicador <- function(indicador_nombre, indicador_codigo) {
    nombre <- tolower(indicador_nombre)
    codigo <- tolower(indicador_codigo)
    
    # SECTOR REAL
    if (grepl("pib|gdp|producción|producto interior|crecimiento económico", nombre) && 
        !grepl("per cápita|ppa|deflactor", nombre)) {
      return("Sector real")
    }
    if (grepl("consumo privado|consumo público|consumo hogares|consumo gobierno", nombre)) {
      return("Sector real")
    }
    if (grepl("formación bruta|inversión total|fbcf|capital fijo", nombre)) {
      return("Sector real")
    }
    if (grepl("ahorro nacional|ahorro bruto", nombre)) {
      return("Sector real")
    }
    if (grepl("valor añadido|agricultura|industria|servicios|construcción", nombre) && 
        grepl("% pib|porcentaje", nombre, ignore.case = TRUE)) {
      return("Sector real")
    }
    if (codigo %in% c("ngdp_rpch", "ngdpd", "ngdp", "nid_ngdp", "ngsd_ngdp", "b1gq")) {
      return("Sector real")
    }
    
    # MERCADO LABORAL
    if (grepl("desempleo|unemployment|paro", nombre)) {
      return("Mercado laboral")
    }
    if (grepl("empleo|employment|ocupación|ocupados|población activa", nombre) && 
        !grepl("autoempleo", nombre)) {
      return("Mercado laboral")
    }
    if (grepl("clu|coste laboral|salario", nombre) && !grepl("impuesto", nombre)) {
      return("Mercado laboral")
    }
    if (grepl("productividad", nombre) && grepl("trabajo|hora|laboral", nombre)) {
      return("Mercado laboral")
    }
    if (codigo %in% c("lur", "le", "sl.uem.totl.zs")) {
      return("Mercado laboral")
    }
    
    # SECTOR EXTERIOR
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
    if (grepl("^b[a-z]{2,3}_|^cab|^bgs|^bxg|^bmg|^bfa|^bfd|^bfp|^bip|^bis_|^bra", codigo)) {
      return("Sector exterior")
    }
    if (codigo %in% c("bca_ngdpd", "tx_rpch", "tm_rpch", "reer_bis_broad", "neer_bis_broad")) {
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
    if (grepl("inflación|inflation|ipc|cpi|hicp|precios al consumo", nombre)) {
      return("Precios y monetarios")
    }
    if (grepl("deflactor", nombre)) {
      return("Precios y monetarios")
    }
    if (grepl("tipo de interés|interest rate|tasa de interés|tipo interbancario", nombre)) {
      return("Precios y monetarios")
    }
    if (grepl("masa monetaria|agregado monetario|m1|m2|m3|base monetaria", nombre)) {
      return("Precios y monetarios")
    }
    if (grepl("crédito|credit|préstamos", nombre) && 
        grepl("sector privado|bancario|interno", nombre)) {
      return("Precios y monetarios")
    }
    if (grepl("morosidad|non.?performing|npl|préstamos dudosos", nombre)) {
      return("Precios y monetarios")
    }
    if (grepl("roe|roa|rentabilidad bancaria|ratio de capital|solvencia bancaria", nombre)) {
      return("Precios y monetarios")
    }
    if (grepl("spread|diferencial|margen de intermediación", nombre)) {
      return("Precios y monetarios")
    }
    if (grepl("pcpi|cpi_|headline|core|^ipc", codigo)) {
      return("Precios y monetarios")
    }
    if (codigo %in% c("pcpipch", "pcpiepch", "ngdp_d", "fpolm_pa", "fitb_pa", "fm2_xdc")) {
      return("Precios y monetarios")
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
                                         usar_ocde = TRUE,
                                         usar_dbnomics = TRUE,
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
    n_fuentes <- sum(c(usar_fmi, usar_eurostat, usar_ocde, usar_bm, usar_omc, usar_bis, usar_dbnomics))
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
    
    # 3. OCDE - Prioridad 2 (NUEVO v8.0)
    if (usar_ocde) {
      progreso(progreso_actual, "Descargando OCDE...")
      datos_ocde <- tryCatch({
        descargar_datos_ocde(pais_codigo, fecha_inicio, fecha_fin)
      }, error = function(e) {
        message("     ⚠ OCDE: ", e$message)
        NULL
      })
      if (!is.null(datos_ocde) && nrow(datos_ocde) > 0) {
        datos_lista$ocde <- datos_ocde
        resumen_fuentes$OCDE <- nrow(datos_ocde)
        message(paste0("✓ OCDE: ", nrow(datos_ocde), " registros"))
      } else {
        message("   ℹ OCDE: sin datos (país no miembro o error de conexión)")
      }
      progreso_actual <- progreso_actual + incremento
    }
    
    # 4. Banco Mundial - Prioridad 3
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
    
    # 7. DBnomics - Prioridad 6 (respaldo) (NUEVO v8.0)
    if (usar_dbnomics) {
      progreso(progreso_actual, "Descargando DBnomics...")
      datos_dbnomics <- tryCatch({
        descargar_datos_dbnomics(pais_codigo, fecha_inicio, fecha_fin)
      }, error = function(e) {
        message("     ⚠ DBnomics: ", e$message)
        NULL
      })
      if (!is.null(datos_dbnomics) && nrow(datos_dbnomics) > 0) {
        datos_lista$dbnomics <- datos_dbnomics
        resumen_fuentes$DBnomics <- nrow(datos_dbnomics)
        message(paste0("✓ DBnomics: ", nrow(datos_dbnomics), " registros"))
      } else {
        message("   ℹ DBnomics: sin datos disponibles")
      }
      progreso_actual <- progreso_actual + incremento
    }
    
    if (length(datos_lista) == 0) {
      return(list(datos = NULL, resumen = resumen_fuentes))
    }
    
    # Asegurar que todas las columnas year son integer antes de combinar
    datos_lista <- lapply(datos_lista, function(df) {
      if (!is.null(df) && "year" %in% names(df)) {
        df$year <- as.integer(df$year)
      }
      # Asegurar que prioridad_fuente existe
      if (!is.null(df) && !"prioridad_fuente" %in% names(df)) {
        df$prioridad_fuente <- 99
      }
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
    
    # Añadir categoría detectada a cada registro
    datos_normalizados <- datos_normalizados |>
      dplyr::rowwise() |>
      dplyr::mutate(
        categoria = detectar_categoria_indicador(indicador_nombre, indicador_codigo)
      ) |>
      dplyr::ungroup()
    
    n_total_despues <- nrow(datos_normalizados)
    n_indicadores <- length(unique(datos_normalizados$indicador_nombre))
    message(paste0("✅ Descarga completada: ", n_total_despues, " registros únicos de ", n_indicadores, " indicadores"))
    
    return(list(
      datos = datos_normalizados,
      resumen = resumen_fuentes
    ))
  }
  
  # ============================================================================
  # ORGANIZAR POR CATEGORÍA - v8.0 (con patrones flexibles para BOP/CPI)
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
    
    # v8.0: Función para detectar indicadores por patrones regex
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
      
      # PRECIOS, COSTES E INDICADORES MONETARIOS (UNIFICADO v8.0)
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
    datos <- datos |>
      dplyr::rowwise() |>
      dplyr::mutate(
        categoria_detectada = detectar_categoria(indicador_nombre, indicador_codigo)
      ) |>
      dplyr::ungroup()
    
    # Función auxiliar para procesar cada categoría
    procesar_categoria <- function(datos_filtrados) {
      if (nrow(datos_filtrados) == 0) return(NULL)
      
      datos_cat <- datos_filtrados |>
        dplyr::group_by(year, indicador_nombre, unidad_corta, unidad_larga, fuente) |>
        dplyr::summarise(valor = dplyr::first(valor, na_rm = TRUE), .groups = "drop") |>
        dplyr::ungroup()
      
      if (nrow(datos_cat) == 0) return(NULL)
      
      indicadores_unicos <- datos_cat |>
        dplyr::select(indicador_nombre, unidad_corta, unidad_larga, fuente) |>
        dplyr::distinct()
      
      combinacion_completa <- tidyr::expand_grid(
        indicadores_unicos,
        year = todos_los_anios
      )
      
      datos_cat_completos <- combinacion_completa |>
        dplyr::left_join(datos_cat, by = c("indicador_nombre", "unidad_corta", "unidad_larga", "fuente", "year"))
      
      datos_cat_pivot <- datos_cat_completos |>
        tidyr::pivot_wider(
          id_cols = c(indicador_nombre, unidad_corta, unidad_larga, fuente),
          names_from = year,
          values_from = valor,
          names_sort = TRUE,
          values_fn = list(valor = ~mean(.x, na.rm = TRUE))
        )
      
      return(datos_cat_pivot)
    }
    
    # Crear listas de datos por categoría
    datos_por_categoria <- list()
    
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
    "Ratio empleo-población", "Población activa", "Empleo",
    "CLU nominales", "Productividad/hora",
    # Sector exterior
    "Media simple del arancel NMF", "Media ponderada del arancel NMF",
    "Cuenta corriente", "Balanza cuenta corriente",
    "Inversión extranjera directa neta",
    "Deuda externa", "Reservas internacionales",
    "Tipo de cambio oficial", "Tipo de cambio USD/EUR",
    "Tipo de cambio efectivo real", "Tipo de cambio efectivo nominal",
    # Sector público
    "Ingresos públicos", "Gastos públicos", "Gasto público",
    "Recaudación tributaria",
    "Saldo público", "Saldo fiscal", "Saldo primario", "Saldo estructural",
    "Deuda pública", "Deuda pública bruta", "Deuda pública neta",
    # Precios
    "Inflación (IPC)", "HICP Inflación", "HICP subyacente",
    "Inflación fin de período", "Deflactor del PIB",
    "IPC (índice)",
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
  # EXPORTAR A WORD CON SUBDIVISIONES
  # ============================================================================
  
  exportar_a_word <- function(datos_por_categoria, pais_nombre, fecha_inicio, fecha_fin, archivo_salida) {
    
    doc <- officer::read_docx()
    doc <- doc |> officer::body_set_default_section(
      officer::prop_section(
        page_size = officer::page_size(orient = "landscape"),
        page_margins = officer::page_mar(bottom = 0.5, top = 0.5, right = 0.5, left = 0.5,
                                         header = 0.3, footer = 0.3, gutter = 0)
      )
    )
    
    doc <- doc |>
      officer::body_add_par(
        paste0("INDICADORES ECONÓMICOS – ", toupper(pais_nombre)),
        style = "heading 1"
      ) |>
      officer::body_add_par(
        paste0("Período: ", lubridate::year(fecha_inicio), "–", lubridate::year(fecha_fin)),
        style = "Normal"
      ) |>
      officer::body_add_par("", style = "Normal")
    
    # Definir estructura de categorías y subcategorías según dictamen de Narnia
    estructura_categorias <- list(
      sector_real = list(
        nombre = "Sector real",
        subcategorias = list(
          list(
            nombre = "Producción y demanda",
            patrones = c("PIB", "Consumo", "Formación bruta", "Inversión", "Demanda",
                         "Exportaciones", "Importaciones", "Ahorro")
          ),
          list(
            nombre = "Oferta",
            patrones = c("Agricultura", "Industria", "Servicios", "VAB", "valor añadido")
          )
        )
      ),
      mercado_laboral = list(
        nombre = "Mercado laboral",
        subcategorias = list(
          list(
            nombre = "Indicadores principales",
            patrones = c("desempleo", "Desempleo", "empleo", "Empleo", "actividad", "Población activa", "Ratio empleo")
          ),
          list(
            nombre = "Costes laborales y productividad",
            patrones = c("CLU", "Productividad", "productividad", "Coste", "salario")
          )
        )
      ),
      sector_exterior = list(
        nombre = "Sector exterior",
        subcategorias = list(
          list(
            nombre = "Nivel de proteccionismo",
            patrones = c("arancel", "Arancel", "NMF")
          ),
          list(
            nombre = "Balanza de pagos y posición exterior",
            patrones = c("Cuenta corriente", "Inversión extranjera", "Deuda externa", "Reservas")
          ),
          list(
            nombre = "Competitividad y tipo de cambio",
            patrones = c("cambio", "Cambio", "REER", "NEER", "efectivo")
          ),
          list(
            nombre = "Comercio exterior",
            patrones = c("Exportaciones", "Importaciones", "Volumen")
          )
        )
      ),
      sector_publico = list(
        nombre = "Sector público",
        subcategorias = list(
          list(
            nombre = "Ingresos y gastos",
            patrones = c("Ingresos", "Gasto", "Recaudación")
          ),
          list(
            nombre = "Déficit y deuda",
            patrones = c("Deuda", "Saldo", "Déficit", "primario")
          )
        )
      ),
      precios_monetarios = list(
        nombre = "Precios e indicadores monetarios y financieros",
        subcategorias = list(
          list(
            nombre = "Inflación y precios",
            patrones = c("Inflación", "IPC", "HICP", "Deflactor", "subyacente", "precios")
          ),
          list(
            nombre = "Tipos de interés",
            patrones = c("interés", "interbancario", "política monetaria", "depósito", "préstamo")
          ),
          list(
            nombre = "Agregados monetarios y crédito",
            patrones = c("monetaria", "Crédito", "M1", "M2", "M3", "base monetaria")
          ),
          list(
            nombre = "Sector bancario",
            patrones = c("ROE", "ROA", "morosidad", "capital", "solvencia", "NPL", "bancario")
          )
        )
      ),
      pro_memoria = list(
        nombre = "Pro-memoria",
        subcategorias = list(
          list(
            nombre = "Indicadores demográficos y sociales",
            patrones = c("Población", "urbana", "Esperanza", "Gini", "INB", "per cápita", "PPA")
          )
        )
      )
    )
    
    # Función auxiliar para crear tabla
    crear_tabla_word <- function(datos_tabla) {
      if ("unidad_larga" %in% names(datos_tabla)) {
        datos_tabla <- datos_tabla |> dplyr::select(-unidad_larga)
      }
      if ("fuente" %in% names(datos_tabla)) {
        datos_tabla <- datos_tabla |> dplyr::select(-fuente)
      }
      
      cols_nombres <- names(datos_tabla)
      
      for (col in cols_nombres) {
        if (is.numeric(datos_tabla[[col]])) {
          datos_tabla[[col]] <- sapply(datos_tabla[[col]], function(x) {
            formatear_numero_es(x, 2)
          })
        } else {
          datos_tabla[[col]] <- ifelse(is.na(datos_tabla[[col]]) | datos_tabla[[col]] == "", " ", as.character(datos_tabla[[col]]))
        }
      }
      
      if ("unidad_corta" %in% cols_nombres) {
        datos_tabla$unidad_corta <- ifelse(datos_tabla$unidad_corta != " " & datos_tabla$unidad_corta != "",
                                           paste0("(", datos_tabla$unidad_corta, ")"), " ")
      }
      
      nombres_cols_tabla <- cols_nombres
      nombres_cols_tabla[nombres_cols_tabla == "indicador_nombre"] <- "Indicador"
      nombres_cols_tabla[nombres_cols_tabla == "unidad_corta"] <- " "
      names(datos_tabla) <- nombres_cols_tabla
      
      return(datos_tabla)
    }
    
    # Función para filtrar indicadores por patrones
    filtrar_por_patrones <- function(datos, patrones) {
      if (is.null(datos) || nrow(datos) == 0) return(NULL)
      
      patron_regex <- paste(patrones, collapse = "|")
      datos_filtrado <- datos |>
        dplyr::filter(grepl(patron_regex, indicador_nombre, ignore.case = TRUE))
      
      if (nrow(datos_filtrado) == 0) return(NULL)
      
      # Ordenar según orden predefinido
      datos_filtrado <- ordenar_indicadores(datos_filtrado)
      
      return(datos_filtrado)
    }
    
    # Procesar cada categoría
    for (cat_id in names(datos_por_categoria)) {
      if (!(cat_id %in% names(estructura_categorias))) next
      
      estructura <- estructura_categorias[[cat_id]]
      datos_cat <- datos_por_categoria[[cat_id]]
      
      if (is.null(datos_cat) || nrow(datos_cat) == 0) next
      
      # Añadir título de categoría (heading 2)
      doc <- doc |>
        officer::body_add_par(estructura$nombre, style = "heading 2")
      
      # Si hay subcategorías definidas, dividir los datos
      if (length(estructura$subcategorias) > 0) {
        indicadores_usados <- c()
        
        for (subcat in estructura$subcategorias) {
          datos_subcat <- filtrar_por_patrones(datos_cat, subcat$patrones)
          
          if (!is.null(datos_subcat) && nrow(datos_subcat) > 0) {
            # Evitar duplicados
            datos_subcat <- datos_subcat |>
              dplyr::filter(!(indicador_nombre %in% indicadores_usados))
            
            if (nrow(datos_subcat) > 0) {
              indicadores_usados <- c(indicadores_usados, unique(datos_subcat$indicador_nombre))
              
              datos_tabla <- crear_tabla_word(as.data.frame(datos_subcat))
              
              # Añadir subtítulo (texto en negrita)
              doc <- doc |>
                officer::body_add_par(subcat$nombre, style = "heading 3")
              
              doc <- doc |>
                officer::body_add_table(
                  value = datos_tabla,
                  style = "table_template",
                  first_row = TRUE,
                  first_column = FALSE
                ) |>
                officer::body_add_par("", style = "Normal")
            }
          }
        }
        
        # Añadir indicadores restantes que no coincidieron con ningún patrón
        datos_restantes <- datos_cat |>
          dplyr::filter(!(indicador_nombre %in% indicadores_usados))
        
        if (nrow(datos_restantes) > 0) {
          datos_tabla <- crear_tabla_word(as.data.frame(datos_restantes))
          
          doc <- doc |>
            officer::body_add_par("Otros indicadores", style = "heading 3")
          
          doc <- doc |>
            officer::body_add_table(
              value = datos_tabla,
              style = "table_template",
              first_row = TRUE,
              first_column = FALSE
            ) |>
            officer::body_add_par("", style = "Normal")
        }
      } else {
        # Si no hay subcategorías, mostrar todos los datos juntos
        datos_tabla <- crear_tabla_word(as.data.frame(datos_cat))
        
        doc <- doc |>
          officer::body_add_table(
            value = datos_tabla,
            style = "table_template",
            first_row = TRUE,
            first_column = FALSE
          ) |>
          officer::body_add_par("", style = "Normal")
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
      # Preparar datos con formato requerido:
      # Sección | Indicador | Código | Unidad | Fuente | Años...
      
      # Usar valor_original si existe, si no usar valor
      col_valor <- if ("valor_original" %in% names(datos_completos)) "valor_original" else "valor"
      
      # Verificar si ya existe la columna categoria
      tiene_categoria <- "categoria" %in% names(datos_completos)
      
      datos_para_excel <- datos_completos |>
        dplyr::mutate(
          year = as.integer(year),
          # Usar la categoría ya calculada si existe
          Seccion = if (tiene_categoria) categoria else {
            purrr::map2_chr(indicador_nombre, indicador_codigo, detectar_categoria_indicador)
          },
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
          # Valor original sin transformar
          valor_export = .data[[col_valor]]
        ) |>
        dplyr::select(year, Seccion, Indicador = indicador_nombre, Codigo, Unidad, Fuente = fuente, valor_export) |>
        dplyr::group_by(year, Seccion, Indicador, Codigo, Unidad, Fuente) |>
        dplyr::summarise(valor_export = dplyr::first(valor_export, na_rm = TRUE), .groups = "drop")
      
      # Crear combinación completa de indicadores y años
      indicadores_info <- datos_para_excel |>
        dplyr::select(Seccion, Indicador, Codigo, Unidad, Fuente) |>
        dplyr::distinct()
      
      combinacion_completa <- tidyr::expand_grid(
        indicadores_info,
        year = as.integer(todos_los_anios)
      )
      
      # Unir y pivotar
      datos_para_excel <- combinacion_completa |>
        dplyr::left_join(datos_para_excel, by = c("Seccion", "Indicador", "Codigo", "Unidad", "Fuente", "year")) |>
        tidyr::pivot_wider(
          id_cols = c(Seccion, Indicador, Codigo, Unidad, Fuente),
          names_from = year,
          values_from = valor_export,
          names_sort = TRUE
        ) |>
        # Ordenar por sección y luego por indicador
        dplyr::arrange(
          factor(Seccion, levels = c("Sector real", "Mercado laboral", "Sector exterior", 
                                     "Sector público", "Precios y monetarios", "Pro-memoria", "Sin clasificar")),
          Indicador
        ) |>
        as.data.frame()
      
      # Reemplazar NA por texto vacío en Código
      datos_para_excel$Codigo <- ifelse(is.na(datos_para_excel$Codigo), "", datos_para_excel$Codigo)
      datos_para_excel$Unidad <- ifelse(is.na(datos_para_excel$Unidad), "", datos_para_excel$Unidad)
      
      # Escribir datos
      openxlsx::writeData(wb, "Datos consolidados", datos_para_excel,
                          startRow = 1, startCol = 1, headerStyle = estilo_encabezado)
      
      n_filas <- nrow(datos_para_excel)
      n_cols <- ncol(datos_para_excel)
      
      if (n_filas > 0) {
        # Estilo para columnas de texto (Sección, Indicador, Código, Unidad, Fuente)
        openxlsx::addStyle(wb, "Datos consolidados", estilo_indicador,
                           rows = 2:(n_filas + 1), cols = 1:5, gridExpand = TRUE)
        
        # Estilo para columnas de datos numéricos
        if (n_cols > 5) {
          openxlsx::addStyle(wb, "Datos consolidados", estilo_datos,
                             rows = 2:(n_filas + 1),
                             cols = 6:n_cols, gridExpand = TRUE)
        }
      }
      
      # Anchos de columna
      openxlsx::setColWidths(wb, "Datos consolidados", cols = 1, widths = 20)  # Sección
      openxlsx::setColWidths(wb, "Datos consolidados", cols = 2, widths = 45)  # Indicador
      openxlsx::setColWidths(wb, "Datos consolidados", cols = 3, widths = 25)  # Código
      openxlsx::setColWidths(wb, "Datos consolidados", cols = 4, widths = 30)  # Unidad
      openxlsx::setColWidths(wb, "Datos consolidados", cols = 5, widths = 15)  # Fuente
      if (n_cols > 5) {
        openxlsx::setColWidths(wb, "Datos consolidados", cols = 6:n_cols, widths = 15)
      }
      openxlsx::freezePane(wb, "Datos consolidados", firstActiveRow = 2, firstActiveCol = 6)
    }
    
    nombres_categorias <- c(
      "sector_real" = "Sector real",
      "mercado_laboral" = "Mercado laboral",
      "sector_exterior" = "Sector exterior",
      "sector_publico" = "Sector público",
      "precios_monetarios" = "Precios y monetarios",
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
              
              selectInput(
                "pais",
                label = tags$span(icon("flag"), " País:"),
                choices = NULL,
                selected = NULL,
                width = "100%"
              ),
              
              numericInput(
                "fecha_inicio",
                label = tags$span(icon("calendar"), " Año inicio:"),
                value = lubridate::year(Sys.Date()) - 6,
                min = 1990,
                max = lubridate::year(Sys.Date()) + 5,
                step = 1,
                width = "100%"
              ),
              
              numericInput(
                "fecha_fin",
                label = tags$span(icon("calendar-check"), " Año fin:"),
                value = lubridate::year(Sys.Date()) + 2,
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
                    "FMI (WEO, FM, BOP, CPI)" = "fmi",
                    "Eurostat (UE)" = "eurostat",
                    "OCDE" = "ocde",
                    "Banco Mundial (WDI)" = "bm",
                    "OMC" = "omc",
                    "BIS" = "bis",
                    "DBnomics" = "dbnomics"
                  ),
                  selected = c("fmi", "eurostat", "ocde", "bm", "omc", "bis", "dbnomics"),
                  inline = FALSE,
                  width = "100%"
                ),
                uiOutput("alerta_eurostat"),
                uiOutput("alerta_ocde")
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
              nav_panel("Sector exterior", DT::DTOutput("tabla_sector_exterior")),
              nav_panel("Sector público", DT::DTOutput("tabla_sector_publico")),
              nav_panel("Precios e Ind. monetarios", DT::DTOutput("tabla_precios_monetarios")),
              nav_panel("Pro-memoria", DT::DTOutput("tabla_promemoria"))
            )
          )
        ),
        
        # Botones de exportación
        layout_columns(
          col_widths = c(6, 6),
          class = "mb-4",
          
          downloadButton(
            "btn_exportar_word",
            label = tagList(icon("file-word"), " Exportar a Word"),
            class = "btn-warning btn-lg w-100 py-3"
          ),
          
          downloadButton(
            "btn_exportar_excel",
            label = tagList(icon("file-excel"), " Exportar a Excel"),
            class = "btn-success btn-lg w-100 py-3"
          )
        )
      )
    ),
    
    # Panel de indicadores
    nav_panel(
      title = "Indicadores",
      icon = icon("list"),
      
      div(
        class = "container-fluid py-3",
        
        # Fila 1: Banco Mundial y FMI
        layout_columns(
          col_widths = c(6, 6),
          
          # BANCO MUNDIAL
          card(
            card_header(
              style = "background: linear-gradient(135deg, #87ceeb 0%, #6bb9d9 100%) !important;",
              icon("globe"), " Banco Mundial (World Development Indicators)"
            ),
            card_body(
              p(class = "text-muted small", "Base de datos con más de 1.400 indicadores de desarrollo para todos los países del mundo. Datos históricos con amplia cobertura temporal."),
              
              h5(icon("chart-bar"), " Sector real"),
              tags$ul(
                class = "list-unstyled",
                lapply(names(indicadores_banco_mundial$sector_real), function(cod) {
                  info <- indicadores_banco_mundial$sector_real[[cod]]
                  tags$li(
                    tags$code(cod, class = "badge bg-light text-dark me-2"),
                    info[1], " ", tags$small(class = "text-muted", paste0("(", info[2], ")"))
                  )
                })
              ),
              
              hr(),
              h5(icon("users"), " Mercado laboral"),
              tags$ul(
                class = "list-unstyled",
                lapply(names(indicadores_banco_mundial$mercado_laboral), function(cod) {
                  info <- indicadores_banco_mundial$mercado_laboral[[cod]]
                  tags$li(
                    tags$code(cod, class = "badge bg-light text-dark me-2"),
                    info[1], " ", tags$small(class = "text-muted", paste0("(", info[2], ")"))
                  )
                })
              ),
              
              hr(),
              h5(icon("exchange-alt"), " Sector exterior"),
              tags$ul(
                class = "list-unstyled",
                lapply(names(indicadores_banco_mundial$sector_exterior), function(cod) {
                  info <- indicadores_banco_mundial$sector_exterior[[cod]]
                  tags$li(
                    tags$code(cod, class = "badge bg-light text-dark me-2"),
                    info[1], " ", tags$small(class = "text-muted", paste0("(", info[2], ")"))
                  )
                })
              ),
              
              hr(),
              h5(icon("landmark"), " Sector público"),
              tags$ul(
                class = "list-unstyled",
                lapply(names(indicadores_banco_mundial$sector_publico), function(cod) {
                  info <- indicadores_banco_mundial$sector_publico[[cod]]
                  tags$li(
                    tags$code(cod, class = "badge bg-light text-dark me-2"),
                    info[1], " ", tags$small(class = "text-muted", paste0("(", info[2], ")"))
                  )
                })
              ),
              
              hr(),
              h5(icon("tags"), " Precios y costes"),
              tags$ul(
                class = "list-unstyled",
                lapply(names(indicadores_banco_mundial$precios_costes), function(cod) {
                  info <- indicadores_banco_mundial$precios_costes[[cod]]
                  tags$li(
                    tags$code(cod, class = "badge bg-light text-dark me-2"),
                    info[1], " ", tags$small(class = "text-muted", paste0("(", info[2], ")"))
                  )
                })
              ),
              
              hr(),
              h5(icon("coins"), " Indicadores monetarios"),
              tags$ul(
                class = "list-unstyled",
                lapply(names(indicadores_banco_mundial$indicadores_monetarios), function(cod) {
                  info <- indicadores_banco_mundial$indicadores_monetarios[[cod]]
                  tags$li(
                    tags$code(cod, class = "badge bg-light text-dark me-2"),
                    info[1], " ", tags$small(class = "text-muted", paste0("(", info[2], ")"))
                  )
                })
              ),
              
              hr(),
              h5(icon("info-circle"), " Pro-memoria"),
              tags$ul(
                class = "list-unstyled",
                lapply(names(indicadores_banco_mundial$pro_memoria), function(cod) {
                  info <- indicadores_banco_mundial$pro_memoria[[cod]]
                  tags$li(
                    tags$code(cod, class = "badge bg-light text-dark me-2"),
                    info[1], " ", tags$small(class = "text-muted", paste0("(", info[2], ")"))
                  )
                })
              )
            )
          ),
          
          # FMI
          card(
            card_header(
              style = "background: linear-gradient(135deg, #f4c7ab 0%, #e8b598 100%) !important; color: #5a3825 !important;",
              icon("university"), " FMI (WEO + FM + BOP + FSI)"
            ),
            card_body(
              p(class = "text-muted small", "Datos del Fondo Monetario Internacional: World Economic Outlook (WEO), Fiscal Monitor (FM), Balance of Payments (BOP) y Financial Soundness Indicators (FSI)."),
              
              h5(icon("chart-bar"), " Sector real (WEO)"),
              tags$ul(
                class = "list-unstyled",
                lapply(names(indicadores_fmi$sector_real), function(cod) {
                  info <- indicadores_fmi$sector_real[[cod]]
                  tags$li(
                    tags$code(cod, class = "badge bg-warning text-dark me-2"),
                    info[1], " ", tags$small(class = "text-muted", paste0("(", info[2], ")"))
                  )
                })
              ),
              
              hr(),
              h5(icon("users"), " Mercado laboral (WEO)"),
              tags$ul(
                class = "list-unstyled",
                lapply(names(indicadores_fmi$mercado_laboral), function(cod) {
                  info <- indicadores_fmi$mercado_laboral[[cod]]
                  tags$li(
                    tags$code(cod, class = "badge bg-warning text-dark me-2"),
                    info[1], " ", tags$small(class = "text-muted", paste0("(", info[2], ")"))
                  )
                })
              ),
              
              hr(),
              h5(icon("exchange-alt"), " Sector exterior (WEO)"),
              tags$ul(
                class = "list-unstyled",
                lapply(names(indicadores_fmi$sector_exterior), function(cod) {
                  info <- indicadores_fmi$sector_exterior[[cod]]
                  tags$li(
                    tags$code(cod, class = "badge bg-warning text-dark me-2"),
                    info[1], " ", tags$small(class = "text-muted", paste0("(", info[2], ")"))
                  )
                })
              ),
              
              hr(),
              h5(icon("landmark"), " Sector público (WEO)"),
              tags$ul(
                class = "list-unstyled",
                lapply(names(indicadores_fmi$sector_publico), function(cod) {
                  info <- indicadores_fmi$sector_publico[[cod]]
                  tags$li(
                    tags$code(cod, class = "badge bg-warning text-dark me-2"),
                    info[1], " ", tags$small(class = "text-muted", paste0("(", info[2], ")"))
                  )
                })
              ),
              
              hr(),
              h5(icon("tags"), " Precios y costes (WEO)"),
              tags$ul(
                class = "list-unstyled",
                lapply(names(indicadores_fmi$precios_costes), function(cod) {
                  info <- indicadores_fmi$precios_costes[[cod]]
                  tags$li(
                    tags$code(cod, class = "badge bg-warning text-dark me-2"),
                    info[1], " ", tags$small(class = "text-muted", paste0("(", info[2], ")"))
                  )
                })
              ),
              
              hr(),
              h5(icon("coins"), " Indicadores monetarios (IFS vía SDMX)"),
              tags$ul(
                class = "list-unstyled",
                lapply(names(indicadores_fmi$indicadores_monetarios), function(cod) {
                  info <- indicadores_fmi$indicadores_monetarios[[cod]]
                  tags$li(
                    tags$code(cod, class = "badge bg-warning text-dark me-2"),
                    info[1], " ", tags$small(class = "text-muted", paste0("(", info[2], ")"))
                  )
                })
              ),
              
              hr(),
              h5(icon("info-circle"), " Pro-memoria (WEO)"),
              tags$ul(
                class = "list-unstyled",
                lapply(names(indicadores_fmi$pro_memoria), function(cod) {
                  info <- indicadores_fmi$pro_memoria[[cod]]
                  tags$li(
                    tags$code(cod, class = "badge bg-warning text-dark me-2"),
                    info[1], " ", tags$small(class = "text-muted", paste0("(", info[2], ")"))
                  )
                })
              )
            )
          )
        ),
        
        # Fila 2: OMC, BIS y Eurostat
        layout_columns(
          col_widths = c(4, 4, 4),
          class = "mt-3",
          
          # OMC
          card(
            card_header(
              style = "background: linear-gradient(135deg, #b8d4be 0%, #a3c9a8 100%) !important; color: #2d4a32 !important;",
              icon("balance-scale"), " OMC (Organización Mundial del Comercio)"
            ),
            card_body(
              p(class = "text-muted small", "Datos arancelarios y de política comercial para todos los miembros de la OMC."),
              
              h5(icon("percent"), " Indicadores arancelarios"),
              tags$ul(
                class = "list-unstyled small",
                tags$li(
                  tags$code("TP_A_0010", class = "badge bg-success text-white me-2"),
                  "Arancel NMF simple medio (%)"
                ),
                tags$li(
                  tags$code("TP_A_0030", class = "badge bg-success text-white me-2"),
                  "Media ponderada del arancel NMF (%)"
                )
              ),
              p(class = "small text-muted mt-2", "NMF = Nación Más Favorecida (Most Favoured Nation)")
            )
          ),
          
          # BIS
          card(
            card_header(
              style = "background: linear-gradient(135deg, #d4b8d4 0%, #c9a3c9 100%) !important; color: #4a2d4a !important;",
              icon("chart-line"), " BIS (Bank for International Settlements)"
            ),
            card_body(
              p(class = "text-muted small", "Estadísticas financieras internacionales del Banco de Pagos Internacionales."),
              
              h5(icon("exchange-alt"), " Tipos de cambio efectivos"),
              tags$ul(
                class = "list-unstyled small",
                tags$li(
                  tags$code("REER_BIS_BROAD", class = "badge bg-info text-white me-2"),
                  "Tipo de cambio efectivo real (índice amplio)"
                ),
                tags$li(
                  tags$code("NEER_BIS_BROAD", class = "badge bg-info text-white me-2"),
                  "Tipo de cambio efectivo nominal (índice amplio)"
                )
              ),
              p(class = "small text-muted mt-2", "Índices calculados sobre una cesta amplia de socios comerciales (base 2020=100)")
            )
          ),
          
          # Eurostat
          card(
            card_header(
              style = "background: linear-gradient(135deg, #003399 0%, #002266 100%) !important; color: white !important;",
              icon("flag"), " Eurostat (UE)"
            ),
            card_body(
              p(class = "text-muted small", "Oficina estadística de la Unión Europea. Solo disponible para países miembros de la UE."),
              
              h5(icon("chart-bar"), " Cuentas nacionales"),
              tags$ul(
                class = "list-unstyled small",
                tags$li("PIB real y nominal"),
                tags$li("Crecimiento del PIB"),
                tags$li("PIB per cápita")
              ),
              
              hr(),
              h5(icon("users"), " Empleo"),
              tags$ul(
                class = "list-unstyled small",
                tags$li("Tasa de desempleo"),
                tags$li("Tasa de empleo")
              ),
              
              hr(),
              h5(icon("tags"), " Precios"),
              tags$ul(
                class = "list-unstyled small",
                tags$li("HICP (Índice armonizado de precios al consumo)"),
                tags$li("Inflación anual")
              ),
              
              hr(),
              h5(icon("landmark"), " Finanzas públicas"),
              tags$ul(
                class = "list-unstyled small",
                tags$li("Deuda pública (% PIB)"),
                tags$li("Saldo público (% PIB)")
              )
            )
          )
        )
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
    es_pais_ocde <- reactiveVal(TRUE)  # NUEVO v8.0
    
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
    
    # Observar cambios en el país seleccionado y actualizar la bandera
    observeEvent(input$pais, {
      req(input$pais)
      iso2 <- input$pais
      bandera_actual(paste0("https://flagcdn.com/w160/", tolower(iso2), ".png"))
      
      # Verificar si el país es de la UE para Eurostat
      pais_en_ue <- iso2 %in% paises_ue
      es_pais_ue(pais_en_ue)
      
      # Verificar si el país es de la OCDE (NUEVO v8.0)
      pais_iso3 <- if (iso2 %in% names(mapeo_iso2_iso3)) {
        mapeo_iso2_iso3[iso2]
      } else {
        tryCatch(countrycode::countrycode(iso2, "iso2c", "iso3c"), error = function(e) NULL)
      }
      pais_en_ocde <- !is.null(pais_iso3) && pais_iso3 %in% paises_ocde
      es_pais_ocde(pais_en_ocde)
      
      if (!pais_en_ocde & !pais_en_ue) {
        # Primero deseleccionar
        fuentes_actuales <- input$fuentes_datos
        fuentes_sin_ocde_y_sin_eurostat <- setdiff(fuentes_actuales, c("eurostat","ocde"))
        updateCheckboxGroupInput(session, "fuentes_datos", selected = fuentes_sin_ocde_y_sin_eurostat)
        # Luego deshabilitar con un pequeño delay
        shinyjs::delay(100, shinyjs::disable(selector = "input[value='eurostat']"))
        # Luego deshabilitar con un pequeño delay
        shinyjs::delay(100, shinyjs::disable(selector = "input[value='ocde']"))
      } else if (!pais_en_ocde){
        fuentes_actuales <- input$fuentes_datos
        fuentes_sin_ocde <- setdiff(fuentes_actuales, "ocde")
        updateCheckboxGroupInput(session, "fuentes_datos", selected = fuentes_sin_ocde)
        # Luego deshabilitar con un pequeño delay
        shinyjs::delay(100, shinyjs::disable(selector = "input[value='ocde']"))
      } else if (!pais_en_ue){
        # Primero deseleccionar
        fuentes_actuales <- input$fuentes_datos
        fuentes_sin_eurostat <- setdiff(fuentes_actuales, "eurostat")
        updateCheckboxGroupInput(session, "fuentes_datos", selected = fuentes_sin_eurostat)
        # Luego deshabilitar con un pequeño delay
        shinyjs::delay(100, shinyjs::disable(selector = "input[value='eurostat']"))
      }
      
      if (pais_en_ue & pais_en_ocde) {
        shinyjs::enable(selector = "input[value='eurostat']")
        shinyjs::enable(selector = "input[value='ocde']")
        fuentes_actuales <- input$fuentes_datos
        if (!("eurostat" %in% fuentes_actuales)) {
          updateCheckboxGroupInput(session, "fuentes_datos", selected = c(fuentes_actuales, c("eurostat", "ocde")))
        }
      } else if (pais_en_ue){
        shinyjs::enable(selector = "input[value='eurostat']")
        fuentes_actuales <- input$fuentes_datos
        if (!("eurostat" %in% fuentes_actuales)) {
          updateCheckboxGroupInput(session, "fuentes_datos", selected = c(fuentes_actuales, "eurostat"))
        }
      } else if (pais_en_ocde) {
        shinyjs::enable(selector = "input[value='ocde']")
        fuentes_actuales <- input$fuentes_datos
        if (!("ocde" %in% fuentes_actuales)) {
          updateCheckboxGroupInput(session, "fuentes_datos", selected = c(fuentes_actuales, "ocde"))
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
    
    # Alerta de OCDE (NUEVO v8.0)
    output$alerta_ocde <- renderUI({
      if (!es_pais_ocde()) {
        div(
          class = "alert alert-warning mt-2 py-1",
          style = "font-size: 0.85rem;",
          icon("info-circle"), " OCDE solo disponible para países miembros"
        )
      }
    })
    
    # Botones seleccionar/deseleccionar todas - v8.0 actualizado
    observeEvent(input$btn_seleccionar_todas, {
      fuentes_base <- c("fmi", "bm", "omc", "bis", "dbnomics")
      if (es_pais_ue()) {
        fuentes_base <- c(fuentes_base, "eurostat")
      }
      if (es_pais_ocde()) {
        fuentes_base <- c(fuentes_base, "ocde")
      }
      updateCheckboxGroupInput(session, "fuentes_datos", selected = fuentes_base)
    })
    
    observeEvent(input$btn_deseleccionar_todas, {
      updateCheckboxGroupInput(session, "fuentes_datos", selected = character(0))
    })
    
    # Renderizar la bandera
    output$bandera_pais <- renderUI({
      req(bandera_actual())
      tags$img(
        src = bandera_actual(),
        height = "80px",
        style = "border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.2);"
      )
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
        usar_ocde <- "ocde" %in% input$fuentes_datos && es_pais_ocde()  # NUEVO v8.0
        usar_dbnomics <- "dbnomics" %in% input$fuentes_datos  # NUEVO v8.0
        
        # Contar fuentes activas para calcular incrementos
        n_fuentes <- sum(c(usar_fmi, usar_eurostat, usar_ocde, usar_bm, usar_omc, usar_bis, usar_dbnomics))
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
          usar_ocde = usar_ocde,
          usar_dbnomics = usar_dbnomics,
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
          pageLength = 15,
          scrollX = TRUE,
          dom = 'Bfrtip',
          language = list(
            url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'
          )
        ),
        rownames = FALSE,
        class = "table table-striped table-hover"
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
        req(datos_por_categoria(), pais_seleccionado())
        
        # Crear archivo en directorio de salida
        archivo_destino <- file.path(output_dir, basename(file))
        
        tryCatch({
          exportar_a_word(
            datos_por_categoria(),
            pais_seleccionado(),
            as.Date(paste0(input$fecha_inicio, "-01-01")),
            as.Date(paste0(input$fecha_fin, "-12-31")),
            archivo_destino
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