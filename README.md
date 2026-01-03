<div align="center">

# 📊 Dictamen de Coyuntura Económica

### Herramienta de Preparación para Opositores a Técnico Comercial y Economista del Estado

[![R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-0077B5?style=for-the-badge&logo=rstudio&logoColor=white)](https://shiny.rstudio.com/)
[![License](https://img.shields.io/badge/License-MIT-green.svg?style=for-the-badge)](LICENSE)

**Una aplicación Shiny integral para la descarga, análisis y exportación de datos macroeconómicos de fuentes internacionales**

[Características](#-características-principales) • [Instalación](#-instalación) • [Uso](#-guía-de-uso) • [Ejemplos](#-ejemplos) • [Contribuir](#-contribuir)

---

![Aplicación Shiny](https://img.shields.io/badge/Versión-9.1-purple?style=flat-square)
![Estado](https://img.shields.io/badge/Estado-Activo-success?style=flat-square)
![Mantenimiento](https://img.shields.io/badge/Mantenimiento-Activo-brightgreen?style=flat-square)

</div>

---

## 📖 Índice

- [Descripción](#-descripción)
- [Características Principales](#-características-principales)
- [Fuentes de Datos](#-fuentes-de-datos)
- [Instalación](#-instalación)
- [Guía de Uso](#-guía-de-uso)
- [Estructura del Proyecto](#-estructura-del-proyecto)
- [Ejemplos](#-ejemplos)
- [Novedades v9.1](#-novedades-en-versión-91)
- [Contribuir](#-contribuir)
- [Licencia](#-licencia)
- [Contacto](#-contacto)

---

## 🎯 Descripción

**`dictamencoyuntura`** es una aplicación Shiny diseñada específicamente para facilitar la preparación del **ejercicio de dictamen de coyuntura económica** de las oposiciones a **Técnico Comercial y Economista del Estado**.

La aplicación permite a los opositores:

- ✅ **Descargar automáticamente** datos macroeconómicos de 7 fuentes internacionales
- 📊 **Visualizar y analizar** indicadores económicos organizados por categorías temáticas
- 📄 **Exportar a Word y Excel** con formato profesional compatible con plantillas oficiales
- 🌍 **Seleccionar cualquier país** del mundo con datos disponibles
- ⏱️ **Ahorrar tiempo** en la recopilación de datos, permitiendo centrarse en el análisis económico

### 🎓 Contexto del Examen

El **dictamen de coyuntura económica** es la segunda parte del primer ejercicio de las oposiciones. Los candidatos disponen de **1 hora y 45 minutos** para:

1. Analizar datos macroeconómicos suministrados
2. Redactar un dictamen económico coherente y bien fundamentado
3. Defender posteriormente el dictamen ante el tribunal durante 15 minutos

Esta aplicación simula el tipo de cuadros macroeconómicos que podrían aparecer en el examen, facilitando la práctica y preparación.

---

## ✨ Características Principales

### 🔄 Descarga Automatizada de Datos

- Integración con **7 fuentes internacionales** de prestigio
- Descarga paralela optimizada para reducir tiempos de espera
- Sistema robusto de gestión de errores
- Caché inteligente para evitar descargas duplicadas

### 📑 Organización por Categorías Económicas

Los indicadores se agrupan según las categorías estándar del análisis económico:

| Categoría | Subcategorías |
|-----------|---------------|
| **Sector Real** | PIB, Crecimiento, Output Gap, Oferta |
| **Mercado Laboral** | Empleo, Desempleo, Participación, Productividad |
| **Sector Exterior** | Exportaciones, Importaciones, Balanza de Pagos, Competitividad |
| **Sector Público** | Ingresos, Gastos, Balances, Deuda |
| **Precios y Costes** | Inflación IPC, Deflactor PIB |
| **Indicadores Monetarios y Financieros** | Tipos de interés, Agregados monetarios, Indicadores financieros |
| **Pro-memoria** | Población, PIB per cápita, Tipo de cambio |

### 📤 Exportación Profesional

#### 📄 Exportación a Word
- Formato compatible con plantilla oficial del Ministerio
- Fuente Aptos (estándar oficial)
- Tablas formateadas con colores institucionales
- Estructura lista para análisis económico

#### 📊 Exportación a Excel
- Múltiples hojas organizadas por categoría
- Formato condicional para facilitar lectura
- Metadatos completos (fuente, indicador, unidades)
- Gráficos automáticos de evolución temporal

### 🎨 Interfaz Intuitiva

- **Diseño responsive** adaptado a diferentes tamaños de pantalla
- **Colores institucionales** del Ministerio de Economía (#5F2987 púrpura, #E2EFD9 verde claro)
- **Banderas de países** para identificación visual
- **Tooltips informativos** con descripciones de cada indicador
- **Número formateado en español** (coma decimal, punto de miles)

---

## 🌐 Fuentes de Datos

La aplicación integra datos de las siguientes fuentes internacionales:

| Fuente | Descripción | Indicadores Principales |
|--------|-------------|-------------------------|
| 🏦 **FMI** | Fondo Monetario Internacional | WEO, BOP, FSI, IFS, CPI |
| 🇪🇺 **Eurostat** | Oficina Estadística de la UE | Datos de países de la Unión Europea |
| 🌍 **OCDE** | Organización para la Cooperación y el Desarrollo Económicos | Indicadores de países desarrollados |
| 🌎 **Banco Mundial** | World Development Indicators | Datos globales de desarrollo |
| 🏪 **OMC** | Organización Mundial del Comercio | Comercio internacional |
| 💰 **BIS** | Banco de Pagos Internacionales | Tipos de cambio efectivos |
| 📈 **DBnomics** | Base de datos económica agregada | Datos macroeconómicos adicionales |

### 🔐 Sistema de Priorización

Cuando varios indicadores similares están disponibles de diferentes fuentes, la aplicación aplica una jerarquía de prioridad:

**FMI** > **Eurostat** > **Banco Mundial** > **OMC** > **BIS** > **OCDE** > **DBnomics**

Esto garantiza que siempre se utilicen los datos de mayor calidad y más oficiales.

---

## 🚀 Instalación

### Requisitos Previos

- **R** versión ≥ 4.0.0 ([Descargar R](https://cran.r-project.org/))
- **RStudio** (recomendado) ([Descargar RStudio](https://posit.co/download/rstudio-desktop/))
- Conexión a Internet para descargar datos

### Paso 1: Clonar el Repositorio

```bash
# Opción 1: HTTPS
git clone https://github.com/vgutierrezmarcos/dictamencoyuntura.git

# Opción 2: SSH
git clone git@github.com:vgutierrezmarcos/dictamencoyuntura.git

# Navegar al directorio
cd dictamencoyuntura
```

### Paso 2: Instalar Dependencias

La aplicación **instalará automáticamente** todos los paquetes necesarios la primera vez que se ejecute. Los paquetes que se instalarán son:

```r
# Paquetes de infraestructura Shiny
shiny, bslib, shinyjs

# Paquetes de descarga de datos
WDI, imfr, imfapi, eurostat, OECD, wtor, BIS, rdbnomics

# Paquetes de manipulación de datos
dplyr, tidyr, purrr, lubridate, stringr

# Paquetes de exportación
officer, flextable, openxlsx

# Otros paquetes
DT, httr, readxl, countrycode
```

**Nota**: La instalación automática puede tardar **5-10 minutos** en la primera ejecución.

### Paso 3 (Opcional): Instalación Manual de Paquetes

Si prefieres instalar los paquetes manualmente antes de ejecutar la aplicación:

```r
# Instalar devtools si no lo tienes
install.packages("devtools")

# Instalar el paquete desde el repositorio local
devtools::install(".", dependencies = TRUE)
```

---

## 📘 Guía de Uso

### Inicio Rápido

#### Opción 1: Desde RStudio

```r
# Abrir el proyecto en RStudio
# Archivo > Abrir Proyecto > dictamencoyuntura.Rproj

# Cargar el paquete
library(dictamencoyuntura)

# Lanzar la aplicación
dictamencoyuntura_app()
```

#### Opción 2: Desde consola de R

```r
# Establecer directorio de trabajo
setwd("ruta/a/dictamencoyuntura")

# Cargar el paquete
library(dictamencoyuntura)

# Lanzar la aplicación
dictamencoyuntura_app()
```

#### Opción 3: Con directorio de salida personalizado

```r
# Especificar dónde guardar las exportaciones
dictamencoyuntura_app(output_dir = "mis_exportaciones")
```

### 🎮 Uso de la Interfaz

#### 1️⃣ **Selección del País**

<div align="center">

```
┌─────────────────────────────────────┐
│  🇪🇸  Seleccionar país:           │
│     [España                    ▼]  │
└─────────────────────────────────────┘
```

</div>

- Escribe el nombre del país o selecciona de la lista desplegable
- La bandera del país aparecerá automáticamente
- **Nota**: Algunos países solo tienen datos disponibles en ciertas fuentes

#### 2️⃣ **Configuración del Período**

<div align="center">

```
┌────────────────┬────────────────┐
│  Año inicial   │   Año final    │
│    [2015  ]    │    [2024  ]    │
└────────────────┴────────────────┘
```

</div>

- Selecciona el rango temporal de interés
- Recomendado: **10 años** para análisis de ciclo completo

#### 3️⃣ **Selección de Fuentes de Datos**

<div align="center">

```
☑ FMI (Fondo Monetario Internacional)
☑ Eurostat (solo países UE)
☑ OCDE
☑ Banco Mundial
☑ OMC
☑ BIS
☑ DBnomics
```

</div>

- Por defecto, todas las fuentes están seleccionadas
- **Eurostat** se desactiva automáticamente para países fuera de la UE
- **OCDE** se desactiva para países no miembros

#### 4️⃣ **Descarga de Datos**

<div align="center">

```
┌─────────────────────────────────┐
│   🔄  Descargar Datos           │
└─────────────────────────────────┘
```

</div>

- Haz clic en "Descargar Datos"
- El botón se desactivará durante la descarga
- Barra de progreso mostrará el avance
- Tiempo estimado: **30 segundos - 2 minutos** según fuentes seleccionadas

#### 5️⃣ **Visualización de Resultados**

La aplicación organiza los datos en pestañas:

- **📊 Por Categoría**: Datos agrupados por temas económicos
- **🔍 Datos Completos**: Todos los indicadores con metadatos
- **📈 Resumen de Fuentes**: Estadísticas de cobertura por fuente

#### 6️⃣ **Exportación**

<div align="center">

```
┌─────────────────┬─────────────────┐
│  📄 Exportar    │  📊 Exportar    │
│     a Word      │    a Excel      │
└─────────────────┴─────────────────┘
```

</div>

Los archivos se guardan en la carpeta `output/` con nomenclatura:
- Word: `Dictamen_ES_20260103.docx`
- Excel: `Dictamen_ES_20260103.xlsx`

---

## 📁 Estructura del Proyecto

```
dictamencoyuntura/
│
├── 📄 README.md                           # Este archivo
├── 📄 DESCRIPTION                         # Metadatos del paquete R
├── 📄 NAMESPACE                           # Exportación de funciones
├── 📄 .gitignore                          # Archivos ignorados por Git
│
├── 📂 R/                                  # Código fuente R
│   ├── app_dictamenes_economicos.R        # Aplicación principal Shiny
│   ├── download_functions.R               # Funciones de descarga
│   ├── export_functions.R                 # Funciones de exportación
│   └── utilities.R                        # Utilidades auxiliares
│
├── 📂 templates/                          # Plantillas de exportación
│   └── Plantilla_Ejercicios_Dictamen.dotx # Plantilla Word oficial
│
├── 📂 data/                               # Datos de referencia
│   ├── paises.csv                         # Lista de países
│   └── categorias.csv                     # Categorías de indicadores
│
├── 📂 man/                                # Documentación de funciones
│   └── dictamencoyuntura_app.Rd
│
├── 📂 inst/                               # Archivos instalables
│   └── extdata/                           # Datos adicionales
│
├── 📂 output/                             # Exportaciones (se crea al ejecutar)
│   ├── Dictamen_ES_20260103.docx
│   └── Dictamen_ES_20260103.xlsx
│
├── 📂 docs/                               # Documentación adicional
│   ├── Esquema_dictamen_económico.pdf     # Guía del examen
│   ├── Ejemplo_Narnia.pdf                 # Examen resuelto ejemplo
│   └── Guía_solución.pdf                  # Esquema de solución
│
└── 📂 examples/                           # Ejemplos de uso
    └── ejemplo_uso_basico.R
```

---

## 💡 Ejemplos

### Ejemplo 1: Análisis de España (2015-2024)

```r
library(dictamencoyuntura)

# Lanzar aplicación
dictamencoyuntura_app()

# En la interfaz:
# 1. Seleccionar "España" 
# 2. Período: 2015 - 2024
# 3. Todas las fuentes seleccionadas
# 4. Clic en "Descargar Datos"
# 5. Exportar a Word y Excel
```

**Resultado**: Análisis completo del ciclo económico español post-crisis financiera hasta la actualidad.

### Ejemplo 2: Comparación de Países Emergentes

```r
# Analizar Brasil
dictamencoyuntura_app()
# ... configurar para Brasil, descargar y exportar

# Analizar India
dictamencoyuntura_app()
# ... configurar para India, descargar y exportar

# Comparar los archivos Excel resultantes
```

### Ejemplo 3: Uso Programático (Avanzado)

```r
# Para usuarios avanzados que deseen usar las funciones directamente
library(dictamencoyuntura)

# Descargar datos del FMI para España
datos_fmi <- descargar_datos_fmi(
  pais = "ES",
  fecha_inicio = as.Date("2015-01-01"),
  fecha_fin = as.Date("2024-12-31")
)

# Exportar directamente a Excel sin interfaz
exportar_a_excel(
  datos_categorizados = datos_fmi,
  pais_nombre = "España",
  archivo_salida = "mi_analisis_españa.xlsx"
)
```

---

## 🆕 Novedades en Versión 9.1

### ✅ Nuevos Indicadores

| Categoría | Indicador | Fuente |
|-----------|-----------|--------|
| **Sector Real** | Output gap (% PIB potencial) | FMI |
| **Sector Real** | Exportaciones netas (contribución al crecimiento) | FMI |
| **Oferta** | Valor añadido bruto por sectores | FMI |
| **Mercado Laboral** | Variación % empleo y fuerza laboral | FMI |
| **Sector Exterior** | Balanza de pagos (% PIB) | FMI |
| **Sector Exterior** | Posición inversión internacional neta (NIIP, % PIB) | FMI |
| **Sector Exterior** | Deuda externa bruta (% PIB) | FMI |

### 🔄 Mejoras y Modificaciones

- ✅ Reorganización del sector público en "Ingresos y gastos" y "Balances y deuda"
- ✅ Nomenclatura actualizada de inflación: "Tasa de variación interanual del IPC"
- ✅ Ahorro e inversión movidos a Sector Exterior para mejor coherencia analítica
- ✅ "Ratio empleo-población" renombrado a "Tasa de empleo"
- ✅ Eliminados índices de precios (solo variaciones porcentuales)
- ✅ Subcategorías sin unidades en los títulos para mayor claridad

### 🗑️ Indicadores Eliminados

- ❌ PIB nominal en USD del Banco Mundial (disponible en FMI)
- ❌ PIB nominal fiscal en moneda local

### 🐛 Correcciones de Bugs

- ✅ Compatibilidad mejorada con API SDMX 3.0 del FMI
- ✅ Gestión de códigos ISO2/ISO3 en diferentes bases de datos
- ✅ Corrección de errores en datos de Eurostat para países pequeños
- ✅ Mejor manejo de valores nulos en exportaciones

---

## 🤝 Contribuir

¡Las contribuciones son bienvenidas! Si deseas mejorar esta aplicación:

### 🐛 Reportar Bugs

Abre un [issue](https://github.com/vgutierrezmarcos/dictamencoyuntura/issues) describiendo:
- El problema encontrado
- Pasos para reproducirlo
- Comportamiento esperado vs. observado
- Capturas de pantalla si es posible

### 💡 Proponer Mejoras

¿Tienes ideas para nuevas funcionalidades? Abre un [issue](https://github.com/vgutierrezmarcos/dictamencoyuntura/issues) con:
- Descripción de la funcionalidad
- Justificación (¿por qué sería útil?)
- Ejemplos de uso

### 🔧 Pull Requests

1. **Fork** el repositorio
2. Crea una **rama** para tu funcionalidad (`git checkout -b feature/nueva-funcionalidad`)
3. **Commit** tus cambios (`git commit -am 'Añade nueva funcionalidad'`)
4. **Push** a la rama (`git push origin feature/nueva-funcionalidad`)
5. Abre un **Pull Request**

### 📝 Guía de Estilo

- Código en **español** (nombres de variables, funciones, comentarios)
- Usar **tidyverse** para manipulación de datos
- Documentar funciones con **roxygen2**
- Incluir **ejemplos** en la documentación

---

## 📋 Roadmap

### 🎯 Versión 10.0 (Planificada)

- [ ] **Gráficos interactivos** con plotly en la interfaz
- [ ] **Comparación entre países** en la misma sesión
- [ ] **Análisis automático** con IA (identificación de fases del ciclo)
- [ ] **Plantillas personalizables** para exportación
- [ ] **Modo offline** con caché de datos descargados
- [ ] **Aplicación web** desplegada en shinyapps.io

### 🔮 Futuras Mejoras

- [ ] Integración con más fuentes (BdE, INE, etc.)
- [ ] Generación automática de texto de análisis
- [ ] Exámenes de práctica interactivos
- [ ] Sistema de favoritos para países frecuentes
- [ ] Exportación a PowerPoint

---

## 📜 Licencia

Este proyecto está bajo la licencia **MIT**. Ver el archivo [LICENSE](LICENSE) para más detalles.

```
MIT License

Copyright (c) 2026 Víctor Gutiérrez Marcos

Se permite el uso, copia, modificación y distribución de este software
con fines educativos y de preparación de oposiciones.
```

---

## 👤 Contacto

**Víctor Gutiérrez Marcos**

- 📧 Email: [victorgutierrezmarcos@gmail.com](mailto:victorgutierrezmarcos@gmail.com)
- 🏢 Ministerio de Economía, Comercio y Empresa de España
- 💼 LinkedIn: [Tu perfil de LinkedIn]
- 🐙 GitHub: [@vgutierrezmarcos](https://github.com/vgutierrezmarcos)

---

## 🙏 Agradecimientos

Esta aplicación ha sido desarrollada con el objetivo de facilitar la preparación de opositores a Técnico Comercial y Economista del Estado. Un agradecimiento especial a:

- 📚 **Compañeros opositores** que han proporcionado feedback valioso
- 🏛️ **Ministerio de Economía** por la formación en análisis económico
- 🌐 **Instituciones internacionales** (FMI, Banco Mundial, OCDE, etc.) por facilitar el acceso público a datos
- 💻 **Comunidad R** por las excelentes herramientas de código abierto

---

## ⚠️ Disclaimer

Esta aplicación es una **herramienta de preparación no oficial** para el ejercicio de dictamen de coyuntura económica de las oposiciones a Técnico Comercial y Economista del Estado.

**Notas importantes**:

- ✋ No está afiliada ni respaldada por el Ministerio de Economía, Comercio y Empresa
- 📊 Los datos provienen de fuentes públicas internacionales y pueden contener errores o estar desactualizados
- 🎓 El usuario es responsable de verificar la exactitud de los datos antes de su uso
- 📖 Esta herramienta complementa, pero no sustituye, el estudio de la teoría económica y el análisis crítico

**Uso recomendado**:
- Como herramienta de **práctica** para familiarizarse con el formato de los datos
- Para **ahorrar tiempo** en la recopilación de información
- Como **punto de partida** para el análisis, no como solución final

---

<div align="center">

### 🌟 Si esta aplicación te resulta útil, ¡dale una estrella al repositorio!

[![GitHub stars](https://img.shields.io/github/stars/vgutierrezmarcos/dictamencoyuntura?style=social)](https://github.com/vgutierrezmarcos/dictamencoyuntura/stargazers)
[![GitHub forks](https://img.shields.io/github/forks/vgutierrezmarcos/dictamencoyuntura?style=social)](https://github.com/vgutierrezmarcos/dictamencoyuntura/network/members)

---

**¡Mucha suerte en las oposiciones!** 🎓📈

---

*Última actualización: 3 de enero de 2026*

</div>