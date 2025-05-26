set.seed(7)
# Librerías necesarias
library(data.table)
library(lubridate)
library(rsparse)
library(Matrix)
library(recommenderlab)

# Cargar datos
tickets <- readRDS("DATOS/tickets_enc (1).RDS")
objetivos <- readRDS("DATOS/objetivos (1).RDS")
maestroestr <- readRDS("DATOS/Datos Originales/maestroestr.RDS")

# Asegurar formatos
setDT(tickets)
setDT(maestroestr)
tickets[, dia := ymd(dia)]

# Obtener los clientes olvidadizos (objetivo 4)
clientes_olvidadizos <- objetivos$objetivo4$obj

# Obtener la última cesta de cada cliente olvidadizo
ultimos_tickets <- tickets[id_cliente_enc %in% clientes_olvidadizos,
                           .(ultima_fecha = max(dia),
                             ultimo_ticket = num_ticket[which.max(dia)]),
                           by = id_cliente_enc]

# Filtramos los tickets para no incluir la última cesta en el entrenamiento
tickets_filtrados <- tickets[!paste(id_cliente_enc, num_ticket) %in%
                               paste(ultimos_tickets$id_cliente_enc, ultimos_tickets$ultimo_ticket)]

# Productos más comunes (para evitar recomendaciones genéricas)
popularidad <- tickets[, .N, by = cod_est]
setorder(popularidad, -N)
productos_comunes <- popularidad[1:5, cod_est]

# Crear la matriz de interacciones cliente-producto
interacciones <- tickets_filtrados[, .(frecuencia = .N), by = .(id_cliente_enc, cod_est)]
usuarios <- unique(interacciones$id_cliente_enc)
productos <- unique(interacciones$cod_est)

matriz <- sparseMatrix(
  i = match(interacciones$id_cliente_enc, usuarios),
  j = match(interacciones$cod_est, productos),
  x = interacciones$frecuencia,
  dimnames = list(usuarios, productos)
)

# CAMBIO PRINCIPAL: Convertir a realRatingMatrix y luego binarizar
ratingMatrix <- as(matriz, "realRatingMatrix")
matriz_binaria <- binarize(ratingMatrix, minRating = 1)
# Convertir de vuelta a sparseMatrix para usar con rsparse
matriz <- as(matriz_binaria, "sparseMatrix")

# Entrenar modelo ALS (implicito)
modelo <- WRMF$new(feedback = "implicit", rank = 30, lambda = 0.1, iterations = 15)
modelo$fit_transform(matriz)

# Preparar tabla de resultados
resultados <- data.table(id_cliente_enc = character(),
                         recomendacion = character(),
                         descripcion = character())

# Generar recomendaciones por cliente
for (cliente in clientes_olvidadizos) {
  if (!(cliente %in% rownames(matriz))) next  # Cliente no está en matriz, saltar

  # Productos comunes a excluir
  indices_excluir <- which(colnames(matriz) %in% productos_comunes)
  cliente_idx <- which(rownames(matriz) == cliente)

  # Obtener top-N productos con scores y excluir comunes
  scores <- tryCatch({
    modelo$predict(cliente_idx, k = 100, not_recommend = indices_excluir, ret_scores = TRUE)
  }, error = function(e) NULL)

  if (!is.null(scores) && length(scores$indices) > 0) {
    productos_ordenados <- data.table(
      cod_est = colnames(matriz)[scores$indices],
      score = scores$scores
    )
    productos_ordenados <- productos_ordenados[!cod_est %in% productos_comunes]

    if (nrow(productos_ordenados) > 0) {
      producto_rec <- productos_ordenados[1, cod_est]
    } else {
      producto_rec <- NA
    }
  } else {
    producto_rec <- NA
  }

  # Si no se encuentra nada, elegimos aleatoriamente entre productos menos comunes
  if (is.na(producto_rec)) {
    productos_fallback <- popularidad[!cod_est %in% productos_comunes]
    if (nrow(productos_fallback) > 0) {
      producto_rec <- sample(productos_fallback$cod_est, 1)
    } else {
      next
    }
  }

  # Obtener descripción
  descripcion <- maestroestr[cod_est == producto_rec, descripcion]
  if (length(descripcion) == 0) descripcion <- "Sin descripción"

  # Agregar a resultados
  resultados <- rbind(resultados, data.table(
    id_cliente_enc = cliente,
    recomendacion = producto_rec,
    descripcion = descripcion
  ))
}

# Mostrar y guardar resultados
print(resultados)
saveRDS(resultados, "DATOS/Datos Shiny/recomendaciones_als_final.RDS")
write.csv(resultados, "DATOS/Datos Shiny/recomendaciones_als_final.csv", row.names = FALSE)

# Cargar librerías necesarias para gráfico
library(ggplot2)
library(plotly)
library(dplyr)
library(stringr)

# Crear dataframe con los primeros 10 resultados para el gráfico
recomendaciones_viz <- resultados[1:min(10, nrow(resultados))]
recomendaciones_viz[, id := 1:.N]

# Extraer categorías de los productos (primera palabra)
recomendaciones_viz[, categoria := str_extract(descripcion, "^\\w+")]

# Crear el gráfico base con ggplot2
p <- ggplot(recomendaciones_viz, aes(x = factor(id), y = 1, text = paste0(
  "ID Cliente: ", id, "<br>",
  "Código: ", recomendacion, "<br>",
  "Producto: ", descripcion
))) +
  geom_tile(aes(fill = categoria), color = "white", width = 0.9, height = 0.9) +
  geom_text(aes(label = recomendacion), color = "white", fontface = "bold") +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Recomendaciones de Productos por Cliente (Matriz Binaria)",
    x = "ID Cliente",
    fill = "Categoría"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom"
  )

# Convertir a gráfico interactivo con plotly
pp <- ggplotly(p, tooltip = "text") %>%
  layout(margin = list(l = 50, r = 50, b = 50, t = 100))

# Mostrar el gráfico
pp
