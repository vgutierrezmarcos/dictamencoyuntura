# ============================================================================
# APLICACIÓN SHINY PARA DESCARGA DE DATOS MACROECONÓMICOS
# Preparación de Dictámenes Económicos - Versión 4.0
# ============================================================================
#
# FUENTES DE DATOS:
# - FMI (WEO, BOP, IFS, FSI)
# - Banco Mundial (World Development Indicators)
# - OMC (Organización Mundial del Comercio)
# - BIS (Bank for International Settlements)
# - FRED (Federal Reserve Economic Data)
# - Eurostat
#
# Fecha: Diciembre 2025
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
    "fredr",
    "eurostat",
    "BIS"
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
  # INDICADORES DEL BANCO MUNDIAL (selección robusta)
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
    
    pro_memoria = list(
      "LP" = c("Población", "mill.", "Millones de personas"),
      "PPPPC" = c("PIB per cápita (PPA)", "USD int.", "Dólares internacionales")
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
  
  # ============================================================================
  # FUNCIÓN - Descarga Banco Mundial con reintentos
  # ============================================================================
  
  descargar_datos_bm <- function(pais_codigo, fecha_inicio, fecha_fin, max_reintentos = 3) {
    
    codigos_indicadores <- unlist(lapply(indicadores_banco_mundial, names), use.names = FALSE)
    
    mapeo_nombres <- list()
    mapeo_unidades_cortas <- list()
    mapeo_unidades_largas <- list()
    for (cat in names(indicadores_banco_mundial)) {
      for (cod in names(indicadores_banco_mundial[[cat]])) {
        mapeo_nombres[[cod]] <- indicadores_banco_mundial[[cat]][[cod]][1]
        mapeo_unidades_cortas[[cod]] <- indicadores_banco_mundial[[cat]][[cod]][2]
        mapeo_unidades_largas[[cod]] <- indicadores_banco_mundial[[cat]][[cod]][3]
      }
    }
    
    datos <- NULL
    
    for (intento in 1:max_reintentos) {
      tryCatch({
        message(paste0("Intento ", intento, " de descarga del Banco Mundial..."))
        
        datos <- WDI::WDI(
          country = pais_codigo,
          indicator = codigos_indicadores,
          start = lubridate::year(fecha_inicio),
          end = lubridate::year(fecha_fin),
          extra = TRUE
        )
        
        if (!is.null(datos) && nrow(datos) > 0) {
          break
        }
        
      }, error = function(e) {
        message(paste0("Error en intento ", intento, ": ", e$message))
        if (intento < max_reintentos) {
          Sys.sleep(2)
        }
      })
    }
    
    if (is.null(datos) || nrow(datos) == 0) {
      message("No se pudieron obtener datos del Banco Mundial después de varios intentos.")
      return(NULL)
    }
    
    cols_disponibles <- intersect(codigos_indicadores, colnames(datos))
    
    if (length(cols_disponibles) == 0) {
      message("No se encontraron indicadores válidos.")
      return(NULL)
    }
    
    resultado <- datos |>
      dplyr::select(country, year, iso2c, dplyr::any_of(cols_disponibles)) |>
      tidyr::pivot_longer(
        cols = dplyr::any_of(cols_disponibles),
        names_to = "indicador_codigo",
        values_to = "valor"
      ) |>
      dplyr::mutate(
        indicador_nombre = sapply(indicador_codigo, function(x) mapeo_nombres[[x]]),
        unidad_corta = sapply(indicador_codigo, function(x) mapeo_unidades_cortas[[x]]),
        unidad_larga = sapply(indicador_codigo, function(x) mapeo_unidades_largas[[x]]),
        fuente = "Banco Mundial"
      ) |>
      dplyr::filter(!is.na(valor))
    
    resultado <- resultado |>
      dplyr::mutate(
        valor = dplyr::case_when(
          grepl("mill\\.", unidad_corta) & abs(valor) > 1e6 ~ valor / 1e6,
          TRUE ~ valor
        )
      )
    
    return(resultado)
  }
  
  # ============================================================================
  # FUNCIÓN FMI - Usando imfweo si disponible
  # ============================================================================
  
  descargar_datos_fmi <- function(pais_codigo_iso2, fecha_inicio, fecha_fin) {
    
    paises <- obtener_lista_paises()
    match_idx <- which(paises$iso2c == pais_codigo_iso2)
    
    if (length(match_idx) == 0) {
      pais_codigo_iso3 <- mapeo_iso2_iso3[pais_codigo_iso2]
      if (is.na(pais_codigo_iso3)) {
        message("No se encontró código ISO3 para: ", pais_codigo_iso2)
        return(NULL)
      }
    } else {
      pais_codigo_iso3 <- paises$iso3c[match_idx]
    }
    
    mapeo_nombres <- list()
    mapeo_unidades_cortas <- list()
    mapeo_unidades_largas <- list()
    for (cat in names(indicadores_fmi)) {
      for (cod in names(indicadores_fmi[[cat]])) {
        mapeo_nombres[[cod]] <- indicadores_fmi[[cat]][[cod]][1]
        mapeo_unidades_cortas[[cod]] <- indicadores_fmi[[cat]][[cod]][2]
        mapeo_unidades_largas[[cod]] <- indicadores_fmi[[cat]][[cod]][3]
      }
    }
    
    if (requireNamespace("imfweo", quietly = TRUE)) {
      tryCatch({
        codigos_fmi <- unlist(lapply(indicadores_fmi, names), use.names = FALSE)
        codigos_fmi <- codigos_fmi[codigos_fmi != ""]
        
        datos_weo <- imfweo::weo_get(
          entities = pais_codigo_iso3,
          series = codigos_fmi,
          start_year = lubridate::year(fecha_inicio),
          end_year = lubridate::year(fecha_fin)
        )
        
        if (!is.null(datos_weo) && nrow(datos_weo) > 0) {
          resultado <- datos_weo |>
            dplyr::rename(
              indicador_codigo = series_id,
              valor = value
            ) |>
            dplyr::mutate(
              indicador_nombre = sapply(indicador_codigo, function(x) {
                if (x %in% names(mapeo_nombres)) mapeo_nombres[[x]] else x
              }),
              unidad_corta = sapply(indicador_codigo, function(x) {
                if (x %in% names(mapeo_unidades_cortas)) mapeo_unidades_cortas[[x]] else ""
              }),
              unidad_larga = sapply(indicador_codigo, function(x) {
                if (x %in% names(mapeo_unidades_largas)) mapeo_unidades_largas[[x]] else ""
              }),
              fuente = "FMI (WEO)",
              country = entity_name,
              iso2c = pais_codigo_iso2
            ) |>
            dplyr::select(country, year, iso2c, indicador_codigo, indicador_nombre, unidad_corta, unidad_larga, valor, fuente) |>
            dplyr::filter(!is.na(valor))
          
          return(resultado)
        }
      }, error = function(e) {
        message("Error con imfweo: ", e$message)
      })
    }
    
    message("Datos del FMI no disponibles para este país/período.")
    return(NULL)
  }
  
  # ============================================================================
  # FUNCIONES DE DESCARGA DE FUENTES ADICIONALES
  # ============================================================================
  
  # FMI - Otras bases de datos (BOP, IFS, FSI)
  descargar_datos_fmi_otras <- function(pais_codigo_iso2, fecha_inicio, fecha_fin) {
    tryCatch({
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
      
      if (!requireNamespace("imfr", quietly = TRUE)) {
        return(NULL)
      }
      
      datos_lista <- list()
      
      tryCatch({
        datos_bop <- imfr::imf_dataset(
          database_id = "BOP",
          indicator = c("BBCA_BP6_USD", "BFI_BP6_USD", "BFID_BP6_USD"),
          ref_area = pais_codigo_iso3,
          start = lubridate::year(fecha_inicio),
          end = lubridate::year(fecha_fin)
        )
        if (!is.null(datos_bop) && nrow(datos_bop) > 0) {
          datos_bop$fuente <- "FMI (BOP)"
          datos_lista[["bop"]] <- datos_bop
        }
      }, error = function(e) {
        message("BOP no disponible: ", e$message)
      })
      
      tryCatch({
        datos_fsi <- imfr::imf_dataset(
          database_id = "FSI",
          indicator = c("FSDLI_PT", "FSDIR_PT"),
          ref_area = pais_codigo_iso3,
          start = lubridate::year(fecha_inicio),
          end = lubridate::year(fecha_fin)
        )
        if (!is.null(datos_fsi) && nrow(datos_fsi) > 0) {
          datos_fsi$fuente <- "FMI (FSI)"
          datos_lista[["fsi"]] <- datos_fsi
        }
      }, error = function(e) {
        message("FSI no disponible: ", e$message)
      })
      
      if (length(datos_lista) == 0) {
        return(NULL)
      }
      
      datos_combinados <- dplyr::bind_rows(datos_lista)
      
      if ("TIME_PERIOD" %in% names(datos_combinados)) {
        datos_combinados <- datos_combinados |>
          dplyr::rename(year = TIME_PERIOD)
      }
      if ("OBS_VALUE" %in% names(datos_combinados)) {
        datos_combinados <- datos_combinados |>
          dplyr::rename(valor = OBS_VALUE)
      }
      
      resultado <- datos_combinados |>
        dplyr::mutate(
          country = pais_codigo_iso3,
          iso2c = pais_codigo_iso2,
          indicador_codigo = INDICATOR,
          indicador_nombre = INDICATOR,
          unidad_corta = "",
          unidad_larga = ""
        ) |>
        dplyr::select(country, year, iso2c, indicador_codigo, indicador_nombre,
                      unidad_corta, unidad_larga, valor, fuente) |>
        dplyr::filter(!is.na(valor))
      
      return(resultado)
    }, error = function(e) {
      message("Error descargando otras bases del FMI: ", e$message)
      return(NULL)
    })
  }
  
  # BIS - Tipo de cambio efectivo real y nominal
  descargar_datos_bis <- function(pais_codigo_iso2, fecha_inicio, fecha_fin) {
    tryCatch({
      if (!requireNamespace("BIS", quietly = TRUE)) {
        message("Paquete BIS no disponible.")
        return(NULL)
      }
      
      mapeo_bis <- c(
        "AR" = "AR: Argentina", "AU" = "AU: Australia", "BR" = "BR: Brazil",
        "CA" = "CA: Canada", "CH" = "CH: Switzerland", "CL" = "CL: Chile",
        "CN" = "CN: China", "CO" = "CO: Colombia", "CZ" = "CZ: Czech Republic",
        "DK" = "DK: Denmark", "GB" = "GB: United Kingdom", "HK" = "HK: Hong Kong SAR",
        "HU" = "HU: Hungary", "ID" = "ID: Indonesia", "IL" = "IL: Israel",
        "IN" = "IN: India", "IS" = "IS: Iceland", "JP" = "JP: Japan",
        "KR" = "KR: Korea", "MX" = "MX: Mexico", "MY" = "MY: Malaysia",
        "NO" = "NO: Norway", "NZ" = "NZ: New Zealand", "PE" = "PE: Peru",
        "PH" = "PH: Philippines", "PL" = "PL: Poland", "RO" = "RO: Romania",
        "RU" = "RU: Russia", "SA" = "SA: Saudi Arabia", "SE" = "SE: Sweden",
        "SG" = "SG: Singapore", "TH" = "TH: Thailand", "TR" = "TR: Turkey",
        "TW" = "TW: Chinese Taipei", "US" = "US: United States", "ZA" = "ZA: South Africa",
        "AT" = "XM: Euro area", "BE" = "XM: Euro area", "CY" = "XM: Euro area",
        "DE" = "XM: Euro area", "EE" = "XM: Euro area", "ES" = "XM: Euro area",
        "FI" = "XM: Euro area", "FR" = "XM: Euro area", "GR" = "XM: Euro area",
        "IE" = "XM: Euro area", "IT" = "XM: Euro area", "LT" = "XM: Euro area",
        "LU" = "XM: Euro area", "LV" = "XM: Euro area", "MT" = "XM: Euro area",
        "NL" = "XM: Euro area", "PT" = "XM: Euro area", "SI" = "XM: Euro area",
        "SK" = "XM: Euro area"
      )
      
      area_bis <- mapeo_bis[pais_codigo_iso2]
      if (is.na(area_bis)) {
        message("País no disponible en BIS")
        return(NULL)
      }
      
      ds <- BIS::get_datasets()
      eer_df <- BIS::get_bis(ds$url[ds$id == "WS_EER_csv_flat"])
      
      if (is.null(eer_df) || nrow(eer_df) == 0) {
        message("No se pudieron descargar datos de BIS")
        return(NULL)
      }
      
      datos_lista <- list()
      
      reer_data <- eer_df |>
        dplyr::filter(
          ref_area == area_bis,
          stringr::str_sub(eer_basket, 1, 1) == "B",
          stringr::str_sub(eer_type, 1, 1) == "R",
          !is.na(time_period)
        ) |>
        dplyr::mutate(
          year = as.integer(stringr::str_sub(time_period, 1, 4)),
          valor = as.numeric(obs_value)
        ) |>
        dplyr::filter(year >= lubridate::year(fecha_inicio) &
                        year <= lubridate::year(fecha_fin)) |>
        dplyr::group_by(year) |>
        dplyr::summarise(valor = mean(valor, na.rm = TRUE), .groups = "drop") |>
        dplyr::mutate(
          country = pais_codigo_iso2,
          iso2c = pais_codigo_iso2,
          indicador_codigo = "REER_BIS_BROAD",
          indicador_nombre = "Tipo de cambio efectivo real (Broad)",
          unidad_corta = "índice",
          unidad_larga = "Índice (media 2020=100)",
          fuente = "BIS"
        )
      
      if (nrow(reer_data) > 0) {
        datos_lista[["reer"]] <- reer_data
      }
      
      neer_data <- eer_df |>
        dplyr::filter(
          ref_area == area_bis,
          stringr::str_sub(eer_basket, 1, 1) == "B",
          stringr::str_sub(eer_type, 1, 1) == "N",
          freq == "M: Monthly"
        ) |>
        dplyr::mutate(
          time_period = as.Date(time_period),
          year = lubridate::year(time_period),
          valor = as.numeric(obs_value)
        ) |>
        dplyr::filter(year >= lubridate::year(fecha_inicio) &
                        year <= lubridate::year(fecha_fin)) |>
        dplyr::group_by(year) |>
        dplyr::summarise(valor = mean(valor, na.rm = TRUE), .groups = "drop") |>
        dplyr::mutate(
          country = pais_codigo_iso2,
          iso2c = pais_codigo_iso2,
          indicador_codigo = "NEER_BIS_BROAD",
          indicador_nombre = "Tipo de cambio efectivo nominal (Broad)",
          unidad_corta = "índice",
          unidad_larga = "Índice (media 2020=100)",
          fuente = "BIS"
        )
      
      if (nrow(neer_data) > 0) {
        datos_lista[["neer"]] <- neer_data
      }
      
      if (length(datos_lista) == 0) {
        return(NULL)
      }
      
      resultado <- dplyr::bind_rows(datos_lista) |>
        dplyr::select(country, year, iso2c, indicador_codigo, indicador_nombre,
                      unidad_corta, unidad_larga, valor, fuente) |>
        dplyr::filter(!is.na(valor))
      
      return(resultado)
    }, error = function(e) {
      message("Error descargando datos de BIS: ", e$message)
      return(NULL)
    })
  }
  
  # FRED - Reserva Federal de San Luis
  descargar_datos_fred <- function(fecha_inicio, fecha_fin) {
    tryCatch({
      if (!requireNamespace("fredr", quietly = TRUE)) {
        return(NULL)
      }
      
      if (is.null(fredr::fredr_get_key()) || fredr::fredr_get_key() == "") {
        message("No se ha configurado la API key de FRED. Omitiendo.")
        return(NULL)
      }
      
      series_fred <- c(
        "DEXUSEU" = c("Tipo de cambio USD/EUR", "USD/EUR", "Dólares por euro"),
        "DFF" = c("Tasa fondos federales", "%", "Porcentaje anual"),
        "UNRATE" = c("Tasa desempleo EE.UU.", "%", "Porcentaje"),
        "CPIAUCSL" = c("IPC EE.UU.", "índice", "1982-84=100")
      )
      
      datos_lista <- list()
      
      for (serie_id in names(series_fred)) {
        tryCatch({
          datos_serie <- fredr::fredr(
            series_id = serie_id,
            observation_start = fecha_inicio,
            observation_end = fecha_fin
          )
          
          if (!is.null(datos_serie) && nrow(datos_serie) > 0) {
            datos_serie <- datos_serie |>
              dplyr::mutate(
                year = lubridate::year(date)
              ) |>
              dplyr::group_by(year) |>
              dplyr::summarise(
                valor = mean(value, na.rm = TRUE),
                .groups = "drop"
              ) |>
              dplyr::mutate(
                country = "US",
                iso2c = "US",
                indicador_codigo = serie_id,
                indicador_nombre = series_fred[[serie_id]][1],
                unidad_corta = series_fred[[serie_id]][2],
                unidad_larga = series_fred[[serie_id]][3],
                fuente = "FRED"
              )
            
            datos_lista[[serie_id]] <- datos_serie
          }
        }, error = function(e) {
          message(paste0("Error con serie FRED ", serie_id, ": ", e$message))
        })
      }
      
      if (length(datos_lista) == 0) {
        return(NULL)
      }
      
      resultado <- dplyr::bind_rows(datos_lista) |>
        dplyr::select(country, year, iso2c, indicador_codigo, indicador_nombre,
                      unidad_corta, unidad_larga, valor, fuente) |>
        dplyr::filter(!is.na(valor))
      
      return(resultado)
    }, error = function(e) {
      message("Error descargando datos de FRED: ", e$message)
      return(NULL)
    })
  }
  
  # Eurostat
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
      
      tryCatch({
        datos_gdp <- eurostat::get_eurostat(
          "nama_10_gdp",
          time_format = "num",
          filters = list(geo = pais_codigo_iso2)
        )
        if (!is.null(datos_gdp) && nrow(datos_gdp) > 0) {
          datos_gdp$indicador <- "PIB (Eurostat)"
          datos_lista[["gdp"]] <- datos_gdp
        }
      }, error = function(e) {
        message("Eurostat PIB error: ", e$message)
      })
      
      tryCatch({
        datos_emp <- eurostat::get_eurostat(
          "une_rt_a",
          time_format = "num",
          filters = list(geo = pais_codigo_iso2)
        )
        if (!is.null(datos_emp) && nrow(datos_emp) > 0) {
          datos_emp$indicador <- "Desempleo (Eurostat)"
          datos_lista[["emp"]] <- datos_emp
        }
      }, error = function(e) {
        message("Eurostat empleo error: ", e$message)
      })
      
      tryCatch({
        datos_hicp <- eurostat::get_eurostat(
          "prc_hicp_aind",
          time_format = "num",
          filters = list(geo = pais_codigo_iso2)
        )
        if (!is.null(datos_hicp) && nrow(datos_hicp) > 0) {
          datos_hicp$indicador <- "HICP (Eurostat)"
          datos_lista[["hicp"]] <- datos_hicp
        }
      }, error = function(e) {
        message("Eurostat HICP error: ", e$message)
      })
      
      if (length(datos_lista) == 0) {
        return(NULL)
      }
      
      resultado <- dplyr::bind_rows(datos_lista) |>
        dplyr::filter(time >= lubridate::year(fecha_inicio) &
                        time <= lubridate::year(fecha_fin)) |>
        dplyr::mutate(
          country = pais_codigo_iso2,
          year = as.integer(time),
          iso2c = pais_codigo_iso2,
          indicador_codigo = indicador,
          indicador_nombre = indicador,
          unidad_corta = "",
          unidad_larga = "",
          fuente = "Eurostat"
        ) |>
        dplyr::select(country, year, iso2c, indicador_codigo, indicador_nombre,
                      unidad_corta, unidad_larga, valor = values, fuente) |>
        dplyr::filter(!is.na(valor))
      
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
          fuente = "OMC"
        ) |>
        dplyr::select(country, year, iso2c, indicador_codigo, indicador_nombre,
                      unidad_corta, unidad_larga, valor = value, fuente) |>
        dplyr::filter(!is.na(valor))
      
      return(resultado)
    }, error = function(e) {
      message("Error descargando datos de OMC: ", e$message)
      return(NULL)
    })
  }
  
  # ============================================================================
  # FUNCIÓN COMBINADA
  # ============================================================================
  
  descargar_datos_combinados <- function(pais_codigo, fecha_inicio, fecha_fin,
                                         usar_fmi = TRUE,
                                         usar_bm = TRUE,
                                         usar_omc = TRUE,
                                         usar_bis = TRUE,
                                         usar_fred = TRUE,
                                         usar_eurostat = TRUE) {
    datos_lista <- list()
    resumen_fuentes <- list()
    
    if (usar_fmi) {
      message("Descargando datos del FMI (WEO)...")
      datos_fmi_weo <- descargar_datos_fmi(pais_codigo, fecha_inicio, fecha_fin)
      if (!is.null(datos_fmi_weo) && nrow(datos_fmi_weo) > 0) {
        datos_lista$fmi_weo <- datos_fmi_weo
        resumen_fuentes$`FMI (WEO)` <- nrow(datos_fmi_weo)
        message(paste0("✓ FMI (WEO): ", nrow(datos_fmi_weo), " registros"))
      }
      
      message("Descargando datos del FMI (otras bases)...")
      datos_fmi_otras <- descargar_datos_fmi_otras(pais_codigo, fecha_inicio, fecha_fin)
      if (!is.null(datos_fmi_otras) && nrow(datos_fmi_otras) > 0) {
        datos_lista$fmi_otras <- datos_fmi_otras
        resumen_fuentes$`FMI (otras)` <- nrow(datos_fmi_otras)
        message(paste0("✓ FMI (otras bases): ", nrow(datos_fmi_otras), " registros"))
      }
    }
    
    if (usar_bm) {
      message("Descargando datos del Banco Mundial...")
      datos_bm <- descargar_datos_bm(pais_codigo, fecha_inicio, fecha_fin)
      if (!is.null(datos_bm) && nrow(datos_bm) > 0) {
        datos_lista$bm <- datos_bm
        resumen_fuentes$`Banco Mundial` <- nrow(datos_bm)
        message(paste0("✓ Banco Mundial: ", nrow(datos_bm), " registros"))
      }
    }
    
    if (usar_omc) {
      message("Descargando datos de la OMC...")
      datos_omc <- descargar_datos_omc(pais_codigo, fecha_inicio, fecha_fin)
      if (!is.null(datos_omc) && nrow(datos_omc) > 0) {
        datos_lista$omc <- datos_omc
        resumen_fuentes$OMC <- nrow(datos_omc)
        message(paste0("✓ OMC: ", nrow(datos_omc), " registros"))
      }
    }
    
    if (usar_bis) {
      message("Descargando datos de BIS...")
      datos_bis <- descargar_datos_bis(pais_codigo, fecha_inicio, fecha_fin)
      if (!is.null(datos_bis) && nrow(datos_bis) > 0) {
        datos_lista$bis <- datos_bis
        resumen_fuentes$BIS <- nrow(datos_bis)
        message(paste0("✓ BIS: ", nrow(datos_bis), " registros"))
      }
    }
    
    if (usar_fred) {
      message("Descargando datos de FRED...")
      datos_fred <- descargar_datos_fred(fecha_inicio, fecha_fin)
      if (!is.null(datos_fred) && nrow(datos_fred) > 0) {
        datos_lista$fred <- datos_fred
        resumen_fuentes$FRED <- nrow(datos_fred)
        message(paste0("✓ FRED: ", nrow(datos_fred), " registros"))
      }
    }
    
    if (usar_eurostat) {
      message("Descargando datos de Eurostat...")
      datos_eurostat <- descargar_datos_eurostat(pais_codigo, fecha_inicio, fecha_fin)
      if (!is.null(datos_eurostat) && nrow(datos_eurostat) > 0) {
        datos_lista$eurostat <- datos_eurostat
        resumen_fuentes$Eurostat <- nrow(datos_eurostat)
        message(paste0("✓ Eurostat: ", nrow(datos_eurostat), " registros"))
      }
    }
    
    if (length(datos_lista) == 0) {
      return(list(datos = NULL, resumen = resumen_fuentes))
    }
    
    return(list(
      datos = dplyr::bind_rows(datos_lista),
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
    
    if (is.null(anio_inicio)) anio_inicio <- min(datos$year, na.rm = TRUE)
    if (is.null(anio_fin)) anio_fin <- max(datos$year, na.rm = TRUE)
    todos_los_anios <- seq(anio_inicio, anio_fin)
    
    get_codigos <- function(lista) {
      unlist(lapply(lista, names), use.names = FALSE)
    }
    
    categorias_indicadores <- list(
      sector_real = c(get_codigos(list(indicadores_banco_mundial$sector_real)),
                      get_codigos(list(indicadores_fmi$sector_real))),
      mercado_laboral = c(get_codigos(list(indicadores_banco_mundial$mercado_laboral)),
                          get_codigos(list(indicadores_fmi$mercado_laboral))),
      sector_exterior = c(get_codigos(list(indicadores_banco_mundial$sector_exterior)),
                          get_codigos(list(indicadores_fmi$sector_exterior))),
      sector_publico = c(get_codigos(list(indicadores_banco_mundial$sector_publico)),
                         get_codigos(list(indicadores_fmi$sector_publico))),
      precios_costes = c(get_codigos(list(indicadores_banco_mundial$precios_costes)),
                         get_codigos(list(indicadores_fmi$precios_costes))),
      indicadores_monetarios = get_codigos(list(indicadores_banco_mundial$indicadores_monetarios)),
      pro_memoria = c(get_codigos(list(indicadores_banco_mundial$pro_memoria)),
                      get_codigos(list(indicadores_fmi$pro_memoria)))
    )
    
    datos_por_categoria <- list()
    
    for (cat_nombre in names(categorias_indicadores)) {
      datos_cat <- datos |>
        dplyr::filter(indicador_codigo %in% categorias_indicadores[[cat_nombre]]) |>
        dplyr::group_by(year, indicador_nombre, unidad_corta, unidad_larga, fuente) |>
        dplyr::summarise(valor = dplyr::first(valor, na_rm = TRUE), .groups = "drop") |>
        dplyr::ungroup()
      
      if (nrow(datos_cat) > 0) {
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
        
        datos_por_categoria[[cat_nombre]] <- datos_cat_pivot
      }
    }
    
    return(datos_por_categoria)
  }
  
  # ============================================================================
  # EXPORTAR A WORD
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
    
    nombres_categorias <- c(
      "sector_real" = "Sector real",
      "mercado_laboral" = "Mercado laboral",
      "sector_exterior" = "Sector exterior",
      "sector_publico" = "Sector público",
      "precios_costes" = "Precios y costes",
      "indicadores_monetarios" = "Indicadores monetarios y financieros",
      "pro_memoria" = "Pro-memoria"
    )
    
    for (cat_id in names(datos_por_categoria)) {
      cat_nombre <- nombres_categorias[cat_id]
      if (is.na(cat_nombre)) next
      
      datos_cat <- datos_por_categoria[[cat_id]]
      
      if (!is.null(datos_cat) && nrow(datos_cat) > 0) {
        datos_tabla <- as.data.frame(datos_cat)
        
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
              if (is.na(x)) {
                " "
              } else {
                format(round(x, 2), nsmall = 2, big.mark = ".", decimal.mark = ",", trim = TRUE)
              }
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
        
        doc <- doc |>
          officer::body_add_par(cat_nombre, style = "heading 2")
        
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
  # EXPORTAR A EXCEL
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
                "Fecha generación", "Fuentes de datos"),
      Valor = c(pais_nombre, pais_codigo,
                as.character(anio_inicio),
                as.character(anio_fin),
                format(Sys.Date(), "%d/%m/%Y"),
                "FMI (WEO, BOP, IFS, FSI), Banco Mundial (WDI), OMC, BIS, FRED, Eurostat"),
      stringsAsFactors = FALSE
    )
    
    openxlsx::writeData(wb, "Información", info_data, startRow = 2, startCol = 2)
    openxlsx::addStyle(wb, "Información", estilo_encabezado, rows = 2, cols = 2:3, gridExpand = TRUE)
    openxlsx::setColWidths(wb, "Información", cols = 2:3, widths = c(25, 50))
    
    openxlsx::addWorksheet(wb, "Datos consolidados")
    
    if (!is.null(datos_completos) && nrow(datos_completos) > 0) {
      datos_para_excel <- datos_completos |>
        dplyr::mutate(
          indicador_completo = dplyr::case_when(
            !is.na(unidad_larga) & unidad_larga != "" ~ paste0(indicador_nombre, " (", unidad_larga, ")"),
            TRUE ~ indicador_nombre
          )
        ) |>
        dplyr::select(year, indicador_completo, fuente, valor) |>
        dplyr::group_by(year, indicador_completo, fuente) |>
        dplyr::summarise(valor = mean(valor, na.rm = TRUE), .groups = "drop")
      
      indicadores_fuentes <- datos_para_excel |>
        dplyr::select(indicador_completo, fuente) |>
        dplyr::distinct()
      
      combinacion_completa <- tidyr::expand_grid(
        indicadores_fuentes,
        year = as.integer(todos_los_anios)
      )
      
      datos_para_excel <- combinacion_completa |>
        dplyr::left_join(datos_para_excel, by = c("indicador_completo", "fuente", "year")) |>
        tidyr::pivot_wider(
          id_cols = c(indicador_completo, fuente),
          names_from = year,
          values_from = valor,
          names_sort = TRUE
        ) |>
        dplyr::arrange(indicador_completo) |>
        as.data.frame()
      
      names(datos_para_excel)[names(datos_para_excel) == "indicador_completo"] <- "Indicador"
      names(datos_para_excel)[names(datos_para_excel) == "fuente"] <- "Fuente"
      
      for (col in names(datos_para_excel)) {
        if (is.numeric(datos_para_excel[[col]])) {
          datos_para_excel[[col]] <- sapply(datos_para_excel[[col]], function(x) {
            if (is.na(x)) " " else round(x, 2)
          })
        }
      }
      
      openxlsx::writeData(wb, "Datos consolidados", datos_para_excel,
                          startRow = 1, startCol = 1, headerStyle = estilo_encabezado)
      
      n_filas <- nrow(datos_para_excel)
      n_cols <- ncol(datos_para_excel)
      
      if (n_filas > 0) {
        openxlsx::addStyle(wb, "Datos consolidados", estilo_indicador,
                           rows = 2:(n_filas + 1), cols = 1, gridExpand = TRUE)
        
        openxlsx::addStyle(wb, "Datos consolidados", estilo_indicador,
                           rows = 2:(n_filas + 1), cols = 2, gridExpand = TRUE)
        
        if (n_cols > 2) {
          openxlsx::addStyle(wb, "Datos consolidados", estilo_datos,
                             rows = 2:(n_filas + 1),
                             cols = 3:n_cols, gridExpand = TRUE)
        }
      }
      
      openxlsx::setColWidths(wb, "Datos consolidados", cols = 1, widths = 50)
      openxlsx::setColWidths(wb, "Datos consolidados", cols = 2, widths = 15)
      if (n_cols > 2) {
        openxlsx::setColWidths(wb, "Datos consolidados", cols = 3:n_cols, widths = 12)
      }
      openxlsx::freezePane(wb, "Datos consolidados", firstActiveRow = 2, firstActiveCol = 3)
    }
    
    nombres_categorias <- c(
      "sector_real" = "Sector real",
      "mercado_laboral" = "Mercado laboral",
      "sector_exterior" = "Sector exterior",
      "sector_publico" = "Sector público",
      "precios_costes" = "Precios y costes",
      "indicadores_monetarios" = "Ind. monetarios",
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
                    "FMI (WEO, BOP, FSI)" = "fmi",
                    "Banco Mundial (WDI)" = "bm",
                    "OMC" = "omc",
                    "BIS" = "bis",
                    "FRED" = "fred",
                    "Eurostat (UE)" = "eurostat"
                  ),
                  selected = c("fmi", "bm", "omc", "bis", "fred", "eurostat"),
                  inline = FALSE,
                  width = "100%"
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
              nav_panel("Sector exterior", DT::DTOutput("tabla_sector_exterior")),
              nav_panel("Sector público", DT::DTOutput("tabla_sector_publico")),
              nav_panel("Precios y costes", DT::DTOutput("tabla_precios")),
              nav_panel("Indicadores monetarios", DT::DTOutput("tabla_monetarios")),
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
        
        layout_columns(
          col_widths = c(6, 6),
          
          card(
            card_header(
              style = "background: linear-gradient(135deg, #87ceeb 0%, #6bb9d9 100%) !important;",
              icon("globe"), " Banco Mundial (WDI)"
            ),
            card_body(
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
              )
            )
          ),
          
          card(
            card_header(
              style = "background: linear-gradient(135deg, #f4c7ab 0%, #e8b598 100%) !important; color: #5a3825 !important;",
              icon("university"), " FMI (World Economic Outlook)"
            ),
            card_body(
              div(
                class = "alert alert-info",
                icon("lightbulb"), " ",
                tags$strong("Nota:"), " Los datos del FMI incluyen proyecciones a futuro, ideales para dictámenes económicos."
              ),
              
              h5(icon("chart-bar"), " Sector real"),
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
              h5(icon("landmark"), " Sector público"),
              tags$ul(
                class = "list-unstyled",
                lapply(names(indicadores_fmi$sector_publico), function(cod) {
                  info <- indicadores_fmi$sector_publico[[cod]]
                  tags$li(
                    tags$code(cod, class = "badge bg-warning text-dark me-2"),
                    info[1], " ", tags$small(class = "text-muted", paste0("(", info[2], ")"))
                  )
                })
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
              tags$li(tags$strong("Banco Mundial:"), " World Development Indicators (WDI) - amplia cobertura de países"),
              tags$li(tags$strong("FMI:"), " World Economic Outlook (WEO), Balance of Payments (BOP), Financial Soundness Indicators (FSI) - incluye proyecciones"),
              tags$li(tags$strong("OMC:"), " Datos arancelarios de la Organización Mundial del Comercio"),
              tags$li(tags$strong("BIS:"), " Tipos de cambio efectivos del Bank for International Settlements"),
              tags$li(tags$strong("FRED:"), " Datos de la Reserva Federal de San Luis (requiere API key)"),
              tags$li(tags$strong("Eurostat:"), " Datos estadísticos de la Unión Europea (solo países UE)")
            ),
            
            hr(),
            
            h5(icon("key"), " Configuración de FRED:"),
            
            p("Para usar datos de FRED, necesitas una API key gratuita:"),
            
            tags$ol(
              tags$li("Visita ", tags$a("https://fred.stlouisfed.org/docs/api/api_key.html",
                                        href = "https://fred.stlouisfed.org/docs/api/api_key.html",
                                        target = "_blank")),
              tags$li("Crea una cuenta y solicita tu API key"),
              tags$li(tags$code("fredr_set_key(\"TU_API_KEY\")"), " antes de usar la app")
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
      
      if (!pais_en_ue) {
        # Desmarcar y deshabilitar Eurostat
        fuentes_actuales <- input$fuentes_datos
        fuentes_sin_eurostat <- setdiff(fuentes_actuales, "eurostat")
        updateCheckboxGroupInput(session, "fuentes_datos", selected = fuentes_sin_eurostat)
        shinyjs::disable(selector = "input[value='eurostat']")
      } else {
        shinyjs::enable(selector = "input[value='eurostat']")
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
      if (es_pais_ue()) {
        updateCheckboxGroupInput(session, "fuentes_datos",
                                 selected = c("bm", "fmi", "omc", "bis", "fred", "eurostat"))
      } else {
        updateCheckboxGroupInput(session, "fuentes_datos",
                                 selected = c("bm", "fmi", "omc", "bis", "fred"))
      }
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
      req(resumen_fuentes())
      resumen <- resumen_fuentes()
      
      if (length(resumen) == 0) return(NULL)
      
      total <- sum(unlist(resumen))
      
      div(
        class = "resumen-fuentes mb-4",
        h5(icon("check-circle", class = "text-success"), " Datos descargados: ", total, " registros"),
        div(
          class = "d-flex flex-wrap gap-2 mt-2",
          lapply(names(resumen), function(fuente) {
            tags$span(
              class = "badge bg-primary",
              fuente, ": ", resumen[[fuente]]
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
        
        incProgress(0.1, detail = "Conectando con las fuentes...")
        
        usar_bm <- "bm" %in% input$fuentes_datos
        usar_fmi <- "fmi" %in% input$fuentes_datos
        usar_omc <- "omc" %in% input$fuentes_datos
        usar_bis <- "bis" %in% input$fuentes_datos
        usar_fred <- "fred" %in% input$fuentes_datos
        usar_eurostat <- "eurostat" %in% input$fuentes_datos && es_pais_ue()
        
        incProgress(0.2, detail = "Descargando indicadores...")
        
        resultado <- descargar_datos_combinados(
          input$pais,
          as.Date(paste0(input$fecha_inicio, "-01-01")),
          as.Date(paste0(input$fecha_fin, "-12-31")),
          usar_bm = usar_bm,
          usar_fmi = usar_fmi,
          usar_omc = usar_omc,
          usar_bis = usar_bis,
          usar_fred = usar_fred,
          usar_eurostat = usar_eurostat
        )
        
        incProgress(0.6, detail = "Procesando datos...")
        
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
        
        incProgress(0.8, detail = "Organizando por categorías...")
        
        datos_cat <- organizar_por_categoria(resultado$datos, input$fecha_inicio, input$fecha_fin)
        datos_por_categoria(datos_cat)
        
        incProgress(1, detail = "¡Completado!")
      })
      
      showNotification(
        tagList(icon("check"), " Datos descargados correctamente"),
        type = "message",
        duration = 3
      )
    })
    
    # Función auxiliar para renderizar tablas DT
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
      for (col in cols_numericas) {
        datos_df[[col]] <- sapply(datos_df[[col]], function(x) {
          if (is.na(x)) " " else round(x, 2)
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
      ) |>
        DT::formatRound(columns = cols_numericas, digits = 2)
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
    
    output$tabla_precios <- DT::renderDT({
      req(datos_por_categoria())
      if ("precios_costes" %in% names(datos_por_categoria())) {
        renderizar_tabla(datos_por_categoria()$precios_costes)
      }
    })
    
    output$tabla_monetarios <- DT::renderDT({
      req(datos_por_categoria())
      if ("indicadores_monetarios" %in% names(datos_por_categoria())) {
        renderizar_tabla(datos_por_categoria()$indicadores_monetarios)
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