# Análisis de Clientes y Recomendaciones para Eroski

Este proyecto realiza un análisis exhaustivo de los clientes de Eroski utilizando R. A través de técnicas de minería de datos, segmentación de clientes y sistemas de recomendación, se busca mejorar la comprensión del comportamiento de compra y sugerir productos personalizados.

## Funcionalidades principales

- **Análisis de clientes:** Segmentación mediante técnicas de clustering para identificar grupos con patrones de compra similares.
- **Modelo de recomendación:** Sistema de recomendación implementado con el algoritmo ALS (Alternating Least Squares).
- **Aplicación Shiny:** Interfaz interactiva para visualizar resultados y explorar el análisis.
- **API REST:** Servicio desarrollado con `plumber` para obtener recomendaciones de productos vía solicitudes HTTP.
  
## Tecnologías utilizadas

- **Lenguaje:** R
- **Librerías principales:**
  - `rsparse` para el modelo ALS
  - `plotly` para visualización interactiva
  - `shiny` para construir la interfaz web
  - `plumber` para construir la API REST
  - `ggplot2`, `reshape2`, entre otras
  - `dplyr` para manipulación eficiente de datos
  - `stringr` para manejo de cadenas de texto

## Cómo ejecutar el proyecto

1. **Clona el repositorio**:
   ```bash
   git clone https://github.com/AnderSainzdelamaza/Grupo_AzulClaro_Reto04
   cd Grupo_AzulClaro_Reto04
