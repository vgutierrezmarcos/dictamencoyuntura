<div align="center">

# 📊 Obtención de datos para el dictamen de coyuntura

### Herramienta de preparación para opositores a Técnico Comercial y Economista del Estado

[![R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-0077B5?style=for-the-badge&logo=rstudio&logoColor=white)](https://shiny.rstudio.com/)

**Una aplicación Shiny integral para la descarga, análisis y exportación de datos macroeconómicos de fuentes internacionales**

[Características](#-características-principales) • [Instalación](#-instalación) • [Uso](#-guía-de-uso) • [Ejemplo](#-ejemplos) • [Contribuir](#-contribuir)

---

![Estado](https://img.shields.io/badge/Estado-Activo-success?style=flat-square)
![Mantenimiento](https://img.shields.io/badge/Mantenimiento-Activo-brightgreen?style=flat-square)

</div>

---

## 📖 Índice

- [Descripción](#-descripción)
- [Características principales](#-características-principales)
- [Fuentes de datos](#-fuentes-de-datos)
- [Instalación](#-instalación)
- [Guía de uso](#-guía-de-uso)
- [Estructura del proyecto](#-estructura-del-proyecto)
- [Ejemplos](#-ejemplos)
- [Contribuir](#-contribuir)
- [Contacto](#-contacto)

---

## 🎯 Descripción

**`dictamencoyuntura`** es una aplicación Shiny diseñada específicamente para facilitar la preparación del **ejercicio de dictamen de coyuntura económica** de las oposiciones a **Técnico Comercial y Economista del Estado**.

La aplicación permite a los opositores:

- ✅ **Descargar automáticamente** datos macroeconómicos de 5 fuentes internacionales
- 📊 **Visualizar y analizar** indicadores económicos organizados por categorías temáticas
- 📄 **Exportar a Word y Excel** con formato profesional compatible con plantillas oficiales
- 🌍 **Seleccionar cualquier país** del mundo con datos disponibles
- ⏱️ **Ahorrar tiempo** en la recopilación de datos, permitiendo centrarse en el análisis económico

### 🎓 Contexto del examen

El **dictamen de coyuntura económica** es la segunda parte del primer ejercicio de las oposiciones. Los opositores deben:

1. Analizar datos macroeconómicos suministrados
2. Redactar un dictamen económico coherente y bien fundamentado
3. Defender posteriormente el dictamen ante el tribunal

Esta aplicación simula el tipo de cuadros macroeconómicos que podrían aparecer en el examen, facilitando la práctica y preparación.

---

## ✨ Características Principales

### 🔄 Descarga automatizada de datos

- Integración con **5 fuentes internacionales**
- Descarga optimizada para reducir tiempos de espera

### 📑 Organización por categorías económicas

Los indicadores se agrupan según las siguientes categorías:

| Categoría |
|-----------|
| **Sector real** |
| **Mercado laboral** |
| **Sector exterior** |
| **Sector público** |
| **Indicadores monetarios y financieros** |
| **Pro-memoria** |

### 📤 Exportación de datos

#### 📄 Exportación a Word
- Formato compatible con plantilla habitual utilizada en el examen
- No indica el país seleccionado en este archivo

#### 📊 Exportación a Excel
- Múltiples hojas organizadas por categoría
- Metadatos completos (fuente, indicador, unidades)

### 🎨 Interfaz

- **Diseño sencillo y visual**. Incluye las banderas de los países para identificación visual e indica el número de indicadores descargados.

---

## 🌐 Fuentes de datos

La aplicación integra datos de las siguientes fuentes internacionales:

| Fuente | Descripción | Indicadores principales |
|--------|-------------|-------------------------|
| 🏦 **FMI** | Fondo Monetario Internacional | WEO, BOP, FSI, IFS, CPI |
| 🇪🇺 **Eurostat** | Oficina Estadística de la UE | Datos de países de la Unión Europea |
| 🌎 **Banco Mundial** | World Development Indicators | Datos globales de desarrollo |
| 🏪 **OMC** | Organización Mundial del Comercio | Comercio internacional |
| 💰 **BIS** | Banco de Pagos Internacionales | Tipos de cambio efectivos |
| 📈 **DBnomics** | Base de datos económica agregada | Datos macroeconómicos adicionales |


---

## 🚀 Instalación

### Requisitos previos

- **R** versión ≥ 4.0.0 ([Descargar R](https://cran.r-project.org/))
- **RStudio** (recomendado) ([Descargar RStudio](https://posit.co/download/rstudio-desktop/))
- Conexión a Internet para descargar datos

### Paso 1: Clonar el repositorio

```bash
# Clonar desde GitHub (desde la terminal de RStudio)
git clone https://github.com/vgutierrezmarcos/dictamencoyuntura.git

# Navegar al directorio
cd dictamencoyuntura
```

### Paso 2: Instalar dependencias

La aplicación **instalará automáticamente** todos los paquetes necesarios la primera vez que se ejecute. Los paquetes que se instalarán son:

```r
# Paquetes de infraestructura Shiny
shiny, bslib, shinyjs

# Paquetes de descarga de datos
wbstats, imfr, imfapi, eurostat, OECD, wtor, BIS, rdbnomics

# Paquetes de manipulación de datos
dplyr, tidyr, purrr, lubridate, stringr

# Paquetes de exportación
officer, flextable, openxlsx

# Otros paquetes
DT, httr, readxl, countrycode
```

**Nota**: La instalación automática puede tardar **5-10 minutos** en la primera ejecución.

### Recibir actualizaciones

```bash
# Para recibir las últimas actualizaciones hacer pull (desde la terminal de RStudio, con el proyecto dictamencoyuntura abierto)
git pull
```

---

## 📘 Guía de uso

### Inicio rápido

```r
# Abrir el proyecto en RStudio
# Archivo > Abrir Proyecto > dictamencoyuntura.Rproj

# Cargar el paquete
devtools::load_all()

# Lanzar la aplicación
dictamencoyuntura_app()

# De manera opcional puedes especificar en qué carpeta quieres guardar las exportaciones
dictamencoyuntura_app(output_dir = "mis_exportaciones")
```

### 🎮 Uso de la interfaz

#### 1️⃣ **Selección del país**

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

#### 2️⃣ **Configuración del período**

<div align="center">

```
┌────────────────┬────────────────┐
│  Año inicial   │   Año final    │
│    [2020  ]    │    [2028  ]    │
└────────────────┴────────────────┘
```

</div>

- Selecciona el rango temporal de interés

#### 3️⃣ **Selección de fuentes de datos**

```
☑ FMI (Fondo Monetario Internacional)
☑ Banco Mundial
☑ Eurostat (solo países UE)
☑ OMC
☑ BIS
```

- Por defecto, todas las fuentes están seleccionadas
- **Eurostat** se desactiva automáticamente para países fuera de la UE

#### 4️⃣ **Descarga de datos**

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

#### 5️⃣ **Visualización derResultados**

La aplicación organiza los datos en pestañas:

- **📊 Por categoría**: Datos agrupados por temas económicos
- **📈 Resumen de fuentes**: Estadísticas de cobertura por fuente

#### 6️⃣ **Exportación**

<div align="center">

```
┌─────────────────┬─────────────────┐
│  📄 Exportar    │  📊 Exportar    │
│     a Word      │    a Excel      │
└─────────────────┴─────────────────┘
```

</div>

Los archivos se guardan (por defecto en la carpeta `output/`) con nomenclatura:
- Word: `Dictamen_ES_20260103.docx`
- Excel: `Dictamen_ES_20260103.xlsx`

---

## 📁 Estructura del proyecto

```
dictamencoyuntura/
│
├── 📄 README.md                            # Este archivo
├── 📄 DESCRIPTION                          # Metadatos del paquete R
├── 📄 NAMESPACE                            # Exportación de funciones
├── 📄 .gitignore                           # Archivos ignorados por Git
│
├── 📂 data/                                
│   └─ Indicadores_Dictamen_Economico.xlsx  # Fichero con el listado de indicadores a descargar
│
├── 📂 R/                                   # Código fuente R
│   └── app_dictamenes_economicos.R         # Aplicación principal Shiny
│
├── 📂 templates/                           # Plantillas de exportación
│   └── Plantilla_Ejercicios_Dictamen.dotx  # Plantilla Word
│
└── 📂 output/                              # Exportaciones (se crea al ejecutar)
    ├── Dictamen_ES_20260103.docx
    └── Dictamen_ES_20260103.xlsx
```

---

## 💡 Ejemplo de uso

### Ejemplo: Análisis de Brasil (2015-2024)

```r
devtools::load_all()

# Lanzar aplicación
dictamencoyuntura_app()

# En la interfaz:
# 1. Seleccionar "Brasil" 
# 2. Período: 2015 - 2024
# 3. Todas las fuentes seleccionadas
# 4. Clic en "Descargar Datos"
# 5. Exportar a Word y Excel
```

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


---

## 🔮 Roadmap

###  Futuras mejoras

- [ ] **Análisis automático** con IA que permita generar preguntas de manera directa
- [ ] **Integración con más fuentes de datos**

---

## 👤 Contacto

**Víctor Gutiérrez Marcos**

- 💼 LinkedIn: [Víctor Gutiérrez Marcos](https://www.linkedin.com/in/victorgutierrezmarcos)
- 📧 Email: [victorgutierrezmarcos@gmail.com](mailto:victorgutierrezmarcos@gmail.com)

---

## ⚠️ Disclaimer

Esta aplicación es una **herramienta de preparación no oficial** para el ejercicio de dictamen de coyuntura económica de las oposiciones a Técnico Comercial y Economista del Estado.

**Notas importantes**:

- 📊 Los datos provienen de fuentes públicas internacionales y pueden contener errores o estar desactualizados
- 🎓 El usuario es responsable de verificar la exactitud de los datos antes de su uso

**Uso recomendado**:
- Como herramienta de **práctica** para familiarizarse con el formato de los datos
- Para **ahorrar tiempo** en la recopilación de información

---

<div align="center">

### 🌟 Si esta aplicación te resulta útil, ¡dale una estrella al repositorio!

[![GitHub stars](https://img.shields.io/github/stars/vgutierrezmarcos/dictamencoyuntura?style=social)](https://github.com/vgutierrezmarcos/dictamencoyuntura/stargazers)

---

**¡Mucha suerte en las oposiciones!** 🎓📈

---

*Última actualización: 3 de enero de 2026*

</div>