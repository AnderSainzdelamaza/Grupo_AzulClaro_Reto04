
# Cargar datos
tickets <- readRDS("C:/Users/arbai/OneDrive - Mondragon Unibertsitatea/reto 4/DATOS/tickets_enc (1).RDS")
objetivos <- readRDS("C:/Users/arbai/OneDrive - Mondragon Unibertsitatea/reto 4/DATOS/objetivos (1).RDS")
maestroestr <- readRDS("C:/Users/arbai/Downloads/maestroestr (1).RDS")
# Cargar librerías necesarias
library(data.table)
library(rsparse)  # Para el algoritmo ALS (Alternating Least Squares)
library(Matrix)   # Para manejo de matrices dispersas


# Convertir a data.table para manipulación eficiente
setDT(tickets)
setDT(maestroestr)

# Obtener el producto objetivo a promocionar
producto_objetivo <- objetivos$objetivo1$obj

# Mostrar información del producto objetivo
descripcion_producto <- maestroestr[cod_est == producto_objetivo, descripcion]
cat("Producto a promocionar:", producto_objetivo, "-", descripcion_producto, "\n")

# Paso 1: Preparar los datos para el algoritmo ALS
# Crear matriz de interacciones usuario-item, donde cada fila es un usuario (cliente)
# y cada columna es un ítem (producto)

# Simplificar los datos de tickets a solo las columnas necesarias
interacciones <- tickets[, .(id_cliente_enc, cod_est)]

# Contar cuántas veces cada cliente compró cada producto
interacciones_conteo <- interacciones[, .(cantidad = .N), by = .(id_cliente_enc, cod_est)]

# Crear mapeo de IDs de clientes y productos a índices numéricos secuenciales
# Esto es necesario para crear la matriz dispersa de manera eficiente
clientes_unicos <- unique(interacciones_conteo$id_cliente_enc)
productos_unicos <- unique(interacciones_conteo$cod_est)

cliente_indice <- data.table(
  id_cliente_enc = clientes_unicos,
  cliente_idx = 1:length(clientes_unicos)
)

producto_indice <- data.table(
  cod_est = productos_unicos,
  producto_idx = 1:length(productos_unicos)
)

# Agregar índices a los datos de interacciones
interacciones_indices <- merge(interacciones_conteo, cliente_indice, by = "id_cliente_enc")
interacciones_indices <- merge(interacciones_indices, producto_indice, by = "cod_est")

# Crear matriz dispersa para el algoritmo
matriz_dispersa <- sparseMatrix(
  i = interacciones_indices$cliente_idx,
  j = interacciones_indices$producto_idx,
  x = interacciones_indices$cantidad,
  dims = c(length(clientes_unicos), length(productos_unicos))
)

# Paso 2: Entrenar el modelo ALS (Alternating Least Squares)
cat("Entrenando modelo ALS...\n")
modelo_als <- WRMF$new(
  rank = 20,                # Número de factores latentes
  lambda = 0.1,             # Regularización
  max_iter = 10,            # Máximo de iteraciones
  nthread = parallel::detectCores() - 1,  # Paralelización
  use_float = FALSE,        # Usar precisión double
  non_negative = TRUE       # Restricción de factores no negativos
)

# Ajustar el modelo (entrenar)
# En rsparse actual, usamos fit_transform() en lugar de fit()
factores_usuario <- modelo_als$fit_transform(matriz_dispersa)
factores_item <- modelo_als$components

# Paso 3: Generar puntuaciones para todos los clientes para el producto objetivo
cat("Generando recomendaciones...\n")
# En WRMF, después de fit_transform() necesitamos llamar a components() para obtener los factores de item
factores_item <- modelo_als$components

# Ahora multiplicamos correctamente los factores para obtener las puntuaciones
puntuaciones <- as.matrix(factores_usuario %*% factores_item)

# Paso 4: Encontrar el índice del producto objetivo
indice_producto_objetivo <- producto_indice[cod_est == producto_objetivo, producto_idx]

# Si el producto no existe en el conjunto de datos, mostrar error
if (length(indice_producto_objetivo) == 0) {
  stop("El producto objetivo no se encuentra en los datos de entrenamiento.")
}

# Paso 5: Extraer las puntuaciones predichas para el producto objetivo para todos los clientes
puntuaciones_producto_objetivo <- puntuaciones[, indice_producto_objetivo]

# Convertir a data.table para facilitar la manipulación
resultado <- data.table(
  indice_interno = 1:length(puntuaciones_producto_objetivo),
  afinidad_predicha = puntuaciones_producto_objetivo
)

# Unir con los IDs originales de los clientes
resultado <- merge(resultado, cliente_indice, by.x = "indice_interno", by.y = "cliente_idx")

# Ordenar por puntuación descendente
setorder(resultado, -afinidad_predicha)

# Seleccionar los 10 mejores clientes
top10_clientes <- resultado[1:10]

# Paso 6: Obtener información adicional sobre estos clientes para análisis
# Verificar si ya compraron el producto objetivo
compras_previas <- tickets[
  id_cliente_enc %in% top10_clientes$id_cliente_enc & cod_est == producto_objetivo,
  .(veces_comprado_producto = .N),
  by = id_cliente_enc
]

# Agregar esta información al resultado
top10_clientes <- merge(top10_clientes, compras_previas, by = "id_cliente_enc", all.x = TRUE)
top10_clientes[is.na(veces_comprado_producto), veces_comprado_producto := 0]

# Obtener el total de compras por cliente (como medida de actividad)
actividad_cliente <- tickets[
  id_cliente_enc %in% top10_clientes$id_cliente_enc,
  .(total_compras_historicas = .N),
  by = id_cliente_enc
]
top10_clientes <- merge(top10_clientes, actividad_cliente, by = "id_cliente_enc")

# Añadir una columna con el ranking explícito para facilitar la interpretación de resultados
top10_clientes[, ranking_recomendacion := rank(-afinidad_predicha, ties.method = "first")]

# Reordenar columnas para una mejor presentación
setcolorder(top10_clientes, c("ranking_recomendacion", "id_cliente_enc", "afinidad_predicha",
                              "veces_comprado_producto", "total_compras_historicas", "indice_interno"))

# Mostrar resultados con nombres de columnas más descriptivos
print(top10_clientes)

# Guardar resultados
# Solo guardamos los IDs de los clientes según lo solicitado
saveRDS(top10_clientes$id_cliente_enc, "resultado_objetivo1_ALS.RDS")

# También guardamos el análisis completo para referencia
# Renombramos las columnas en el archivo de salida para mayor claridad
nombres_descriptivos <- c(
  "ranking_recomendacion" = "Ranking",
  "id_cliente_enc" = "ID_Cliente",
  "afinidad_predicha" = "Afinidad_Predicha",
  "veces_comprado_producto" = "Compras_Previas_Producto",
  "total_compras_historicas" = "Total_Compras_Cliente",
  "indice_interno" = "Indice_Tecnico"
)

# Crear una copia con nombres de columnas para el archivo final
top10_para_guardar <- copy(top10_clientes)
setnames(top10_para_guardar, old = names(nombres_descriptivos), new = nombres_descriptivos)

fwrite(
  top10_para_guardar,
  "analisis_recomendaciones_objetivo1_ALS.csv"
)

# Mostrar un resumen final
cat("\nRecomendación completada usando ALS: Los 10 mejores clientes para el producto",
    producto_objetivo, "han sido identificados.\n")
cat("De estos clientes:", sum(top10_clientes$veces_comprado_producto > 0),
    "ya han comprado el producto anteriormente.\n")
cat("Afinidad promedio predicha:", mean(top10_clientes$afinidad_predicha), "\n")














# Visualización única y clara para resultados del modelo ALS
# Un solo gráfico entendible que muestra la información más útil

# Cargar librerías necesarias
library(plotly)
library(data.table)

# Si necesitas instalar plotly:
# install.packages("plotly")

# Este script asume que ya tienes el objeto top10_clientes disponible
# Si no lo tienes, este código crea datos simulados para la visualización
if (!exists("top10_clientes")) {
  # Datos simulados para poder ejecutar la visualización
  set.seed(123)
  top10_clientes <- data.table(
    ranking_recomendacion = 1:10,
    id_cliente_enc = paste0("cliente_", 1:10),
    afinidad_predicha = c(0.92, 0.87, 0.81, 0.76, 0.72, 0.68, 0.65, 0.62, 0.58, 0.55),
    veces_comprado_producto = c(0, 2, 1, 0, 1, 0, 3, 0, 1, 0),
    total_compras_historicas = c(145, 278, 95, 188, 210, 133, 312, 87, 156, 62)
  )
}

# Asegurarse de que los datos estén ordenados por ranking
setorder(top10_clientes, ranking_recomendacion)

# Identificar si son clientes nuevos o existentes para el producto
top10_clientes[, cliente_tipo := ifelse(veces_comprado_producto > 0,
                                        "Cliente existente",
                                        "Cliente nuevo")]

# Crear etiquetas claras para los clientes
top10_clientes[, cliente_label := paste0("#", ranking_recomendacion, ": ", id_cliente_enc)]

# Crear un único gráfico de barras horizontal que muestre toda la información relevante
grafico_unico <- plot_ly() %>%
  # Barras principales que muestran la afinidad predicha
  add_trace(
    data = top10_clientes,
    y = ~reorder(cliente_label, ranking_recomendacion), # Ordenar por ranking
    x = ~afinidad_predicha,
    type = 'bar',
    orientation = 'h',
    name = 'Afinidad predicha',
    marker = list(
      color = ~afinidad_predicha,
      colorscale = list(c(0, "#d0e3fa"), c(1, "#0066cc")), # Azul, intuitivo
      line = list(color = 'rgba(0,0,0,0.3)', width = 1)
    ),
    text = ~paste(
      "<b>", cliente_label, "</b>",
      "<br>Afinidad:", round(afinidad_predicha, 2),
      "<br>Compras del producto:", veces_comprado_producto,
      "<br>Compras totales:", total_compras_historicas
    ),
    hoverinfo = 'text',
    showlegend = FALSE
  ) %>%
  # Añadir iconos o indicadores para clientes nuevos vs existentes
  add_annotations(
    data = top10_clientes,
    y = ~reorder(cliente_label, ranking_recomendacion),
    x = rep(0, nrow(top10_clientes)),  # Colocar al inicio
    text = ifelse(top10_clientes$veces_comprado_producto > 0, "🔄", "🆕"),
    showarrow = FALSE,
    xanchor = 'right',
    xshift = -10,
    font = list(size = 14)
  ) %>%
  # Añadir texto al final de cada barra con el número de compras previas
  add_annotations(
    data = top10_clientes,
    y = ~reorder(cliente_label, ranking_recomendacion),
    x = ~afinidad_predicha,
    text = ~paste0(veces_comprado_producto, " 🛒"),
    showarrow = FALSE,
    xanchor = 'left',
    xshift = 10,
    font = list(size = 12)
  ) %>%
  # Diseño y título del gráfico
  layout(
    title = list(
      text = "Top 10 Clientes Recomendados para el Producto Objetivo",
      font = list(size = 18)
    ),
    xaxis = list(
      title = "Afinidad Predicha",
      range = c(0, max(top10_clientes$afinidad_predicha) * 1.2), # Espacio para etiquetas
      zeroline = TRUE,
      showgrid = TRUE
    ),
    yaxis = list(
      title = "",
      zeroline = FALSE,
      showgrid = FALSE
    ),
    # Añadir notas explicativas
    annotations = list(
      list(
        x = 0.5,
        y = 1.1
        ,
        text = "Mayor afinidad = Cliente más propenso a comprar el producto objetivo",
        showarrow = FALSE,
        xref = "paper",
        yref = "paper",
        font = list(size = 14)
      ),
      list(
        x = 0.5,
        y = 1.05,
        text = "🆕 = Cliente nuevo | 🔄 = Cliente existente | 🛒 = Número de compras previas del producto",
        showarrow = FALSE,
        xref = "paper",
        yref = "paper",
        font = list(size = 14)
      )
    ),
    margin = list(l = 150, r = 50, b = 50, t = 100) # Margen izquierdo para los IDs de clientes
  )

# Mostrar el gráfico
grafico_unico

# Función para guardar el gráfico
guardar_grafico <- function() {
  # Guardar como HTML interactivo
  htmlwidgets::saveWidget(as_widget(grafico_unico), "recomendacion_clientes_ALS.html")
  cat("Gráfico guardado como 'recomendacion_clientes_ALS.html'\n")
}

# Descomentar para guardar el gráfico
# guardar_grafico()

cat("\n=== CÓMO INTERPRETAR ESTE GRÁFICO ===\n")
cat("- Las barras muestran la afinidad predicha para cada cliente (mayor valor = mejor candidato)\n")
cat("- Los clientes están ordenados por ranking de recomendación (de arriba a abajo)\n")
cat("- El ícono al inicio indica si es un cliente nuevo (🆕) o existente (🔄)\n")
cat("- El número al final de cada barra (🛒) muestra cuántas veces el cliente ya compró el producto\n")
cat("- Al pasar el cursor sobre cada barra se muestra información detallada del cliente\n")
