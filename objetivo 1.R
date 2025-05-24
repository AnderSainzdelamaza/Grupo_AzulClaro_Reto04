# Cargar datos
set.seed(7)

tickets <- readRDS("DATOS/tickets_enc (1).RDS")
objetivos <- readRDS("DATOS/objetivos (1).RDS")
maestroestr <- readRDS("DATOS/Datos Originales/maestroestr.RDS")

# Cargar librerías necesarias
library(data.table)
library(rsparse)
library(Matrix)

# Convertir a data.table
setDT(tickets)
setDT(maestroestr)

# Obtener el producto objetivo
producto_objetivo <- objetivos$objetivo1$obj
descripcion_producto <- maestroestr[cod_est == producto_objetivo, descripcion]
cat("Producto a promocionar:", producto_objetivo, "-", descripcion_producto, "\n")

# Preparar datos para ALS
interacciones <- tickets[, .(id_cliente_enc, cod_est)]
interacciones_conteo <- interacciones[, .(cantidad = .N), by = .(id_cliente_enc, cod_est)]

# Mapear IDs a índices
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

# Asignar índices
interacciones_indices <- merge(interacciones_conteo, cliente_indice, by = "id_cliente_enc")
interacciones_indices <- merge(interacciones_indices, producto_indice, by = "cod_est")

# Crear matriz dispersa original
# Crear matriz dispersa original
matriz_dispersa <- sparseMatrix(
  i = interacciones_indices$cliente_idx,
  j = interacciones_indices$producto_idx,
  x = interacciones_indices$cantidad,
  dims = c(length(clientes_unicos), length(productos_unicos))
)

# Binarizar correctamente la matriz dispersa
matriz_dispersa@x[matriz_dispersa@x > 0] <- 1


# Entrenar modelo ALS
cat("Entrenando modelo ALS...\n")
modelo_als <- WRMF$new(
  rank = 20,
  lambda = 0.1,
  max_iter = 10,
  nthread = parallel::detectCores() - 1,
  use_float = FALSE,
  non_negative = TRUE
)

factores_usuario <- modelo_als$fit_transform(matriz_dispersa)
factores_item <- modelo_als$components

# Similitud entre productos
cat("Calculando similitudes entre productos...\n")
factores_item_normalizados <- t(factores_item)
factores_item_norm <- factores_item_normalizados / sqrt(rowSums(factores_item_normalizados^2))

indice_producto_objetivo <- producto_indice[cod_est == producto_objetivo, producto_idx]

if (length(indice_producto_objetivo) == 0) {
  stop("El producto objetivo no se encuentra en los datos de entrenamiento.")
}

similitud_producto <- factores_item_norm %*% factores_item_norm[indice_producto_objetivo, ]

similitudes <- data.table(
  producto_idx = 1:length(similitud_producto),
  similitud = as.numeric(similitud_producto)
)

similitudes <- similitudes[producto_idx != indice_producto_objetivo]
similitudes <- merge(similitudes, producto_indice, by = "producto_idx")
similitudes <- merge(similitudes, maestroestr[, .(cod_est, descripcion)], by = "cod_est", all.x = TRUE)

setorder(similitudes, -similitud)
top20_productos_similares <- similitudes[1:20]
cat("\nLos 20 productos más similares al producto objetivo son:\n")
print(top20_productos_similares[, .(cod_est, descripcion, similitud)])

top4_productos_similares <- top20_productos_similares[1:4]

# Generar puntuaciones
cat("\nGenerando recomendaciones...\n")
factores_item <- modelo_als$components
puntuaciones <- as.matrix(factores_usuario %*% factores_item)

puntuaciones_producto_objetivo <- puntuaciones[, indice_producto_objetivo]

resultado <- data.table(
  indice_interno = 1:length(puntuaciones_producto_objetivo),
  afinidad_predicha = puntuaciones_producto_objetivo
)
resultado <- merge(resultado, cliente_indice, by.x = "indice_interno", by.y = "cliente_idx")

setorder(resultado, -afinidad_predicha)
top10_clientes <- resultado[1:10]

compras_previas <- tickets[
  id_cliente_enc %in% top10_clientes$id_cliente_enc & cod_est == producto_objetivo,
  .(veces_comprado_producto = .N),
  by = id_cliente_enc
]

top10_clientes <- merge(top10_clientes, compras_previas, by = "id_cliente_enc", all.x = TRUE)
top10_clientes[is.na(veces_comprado_producto), veces_comprado_producto := 0]

productos_similares_codigos <- top4_productos_similares$cod_est

compras_similares <- tickets[
  id_cliente_enc %in% top10_clientes$id_cliente_enc &
    cod_est %in% productos_similares_codigos,
  .(
    cantidad_compras = .N,
    producto = cod_est
  ),
  by = id_cliente_enc
]

compras_productos_similares <- dcast(
  compras_similares,
  id_cliente_enc ~ producto,
  value.var = "cantidad_compras",
  fill = 0,
  fun.aggregate = sum
)
setDT(compras_productos_similares)
compras_productos_similares[, total_productos_similares := rowSums(.SD), .SDcols = productos_similares_codigos]

top10_clientes <- merge(top10_clientes, compras_productos_similares,
                        by = "id_cliente_enc", all.x = TRUE)

for (col in c(productos_similares_codigos, "total_productos_similares")) {
  if (col %in% names(top10_clientes)) {
    top10_clientes[is.na(get(col)), (col) := 0]
  } else {
    top10_clientes[, (col) := 0]
  }
}

top10_clientes[, ranking := rank(-afinidad_predicha, ties.method = "first")]

# DF 1: Tabla principal de clientes recomendados
df_clientes_recomendados <- top10_clientes[, .(
  ranking,
  id_cliente = id_cliente_enc,
  afinidad = afinidad_predicha,
  compras_producto = veces_comprado_producto,
  productos_similares_comprados = total_productos_similares
)]
setorder(df_clientes_recomendados, ranking)

write.csv(df_clientes_recomendados, "DATOS/Datos Shiny/df_clientes_recomendados.csv", row.names = T)
# DF 2: Productos similares
df_productos_similares <- top20_productos_similares[, .(
  ranking = 1:.N,
  codigo_producto = cod_est,
  nombre_producto = descripcion,
  similitud
)]
df_productos_similares <- df_productos_similares[-11, ]
write.csv(df_productos_similares, "DATOS/Datos Shiny/df_productos_similares.csv", row.names = T)

# Mostrar resultados
cat("\n\n=== TABLA 1: CLIENTES RECOMENDADOS ===\n")
print(df_clientes_recomendados)

cat("\n\n=== TABLA 2: PRODUCTOS SIMILARES (TOP 20) ===\n")
print(df_productos_similares)












library(data.table)
library(reshape2)
library(plotly)

# Asegurarse que 'datos_largos' es data.table
setDT(datos_largos)

# Crear tabla con códigos y descripciones de productos similares
tabla_productos_similares <- maestroestr[cod_est %in% productos_similares_codigos,
                                         .(cod_est, descripcion)]

# Cambiar nombre columna para hacer merge con datos_largos
setnames(tabla_productos_similares, "cod_est", "producto_similar")

# Convertir producto_similar a character para merge
datos_largos[, producto_similar := as.character(producto_similar)]

# Unir nombres descriptivos de productos similares
datos_largos <- merge(datos_largos, tabla_productos_similares,
                      by = "producto_similar", all.x = TRUE)

# Crear factor para clientes según ranking top 10 para ordenarlos en el gráfico
datos_largos[, cliente := factor(id_cliente_enc, levels = df_top10$id_cliente)]

# Para que se vea mejor, usar descripción como etiqueta de color (nombre del producto)
# Asegurarse que no hay NA en descripcion, si los hay poner "Desconocido"
datos_largos[is.na(descripcion), descripcion := "Desconocido"]

# Graficar
fig2 <- plot_ly(
  datos_largos,
  x = ~cliente,
  y = ~cantidad_comprada,
  color = ~descripcion,
  type = 'bar'
) %>%
  layout(
    barmode = 'stack',
    title = 'Compras de Productos Similares por Top 10 Clientes Recomendados',
    xaxis = list(title = 'ID Cliente', tickangle = -45, tickfont = list(size = 10)),
    yaxis = list(title = 'Cantidad Comprada'),
    legend = list(title = list(text = '<b>Productos Similares</b>')),
    margin = list(b = 100)
  )

fig2
