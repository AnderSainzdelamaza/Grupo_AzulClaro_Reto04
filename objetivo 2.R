# Cargar todas las librerías necesarias
library(dplyr)
library(data.table)
library(tidyr)
library(ggplot2)
library(lubridate)

# Cargar datos
tickets <- readRDS("DATOS/tickets_enc (1).RDS")
objetivos <- readRDS("DATOS/objetivos (1).RDS")
productos <- readRDS("DATOS/Datos Originales/maestroestr.RDS")

# Obtener los 10 clientes objetivo
clientes_objetivo2 <- objetivos$objetivo2$obj
print("Clientes objetivo:")
print(clientes_objetivo2)

# Convertir a character para consistencia
tickets <- tickets %>%
  mutate(id_cliente_enc = as.character(id_cliente_enc))

# PASO 1: Identificar la ÚLTIMA COMPRA de cada cliente objetivo
cat("\n=== PASO 1: IDENTIFICANDO ÚLTIMAS COMPRAS ===\n")

ultimas_compras <- tickets %>%
  filter(id_cliente_enc %in% clientes_objetivo2) %>%
  group_by(id_cliente_enc) %>%
  filter(dia == max(dia)) %>%
  ungroup()

productos_ultima_compra <- ultimas_compras %>%
  group_by(id_cliente_enc) %>%
  summarise(
    productos_en_cesta = list(unique(cod_est)),
    num_productos = n_distinct(cod_est),
    dia_compra = first(dia),
    .groups = "drop"
  )

# PASO 2: Crear MATRIZ BINARIA
cat("\n=== PASO 2: CREANDO MATRIZ BINARIA ===\n")

matriz_binaria <- tickets %>%
  filter(id_cliente_enc %in% clientes_objetivo2) %>%
  distinct(id_cliente_enc, cod_est) %>%
  mutate(compro = 1) %>%
  complete(id_cliente_enc, cod_est = unique(tickets$cod_est), fill = list(compro = 0)) %>%
  pivot_wider(names_from = cod_est, values_from = compro, values_fill = 0)

# PASO 3: Generar RECOMENDACIONES
cat("\n=== PASO 3: GENERANDO RECOMENDACIONES ===\n")

recomendaciones_finales <- data.frame()

for(cliente in clientes_objetivo2) {
  productos_cesta <- productos_ultima_compra %>%
    filter(id_cliente_enc == cliente) %>%
    pull(productos_en_cesta) %>%
    .[[1]]

  if(length(productos_cesta) == 0) next

  productos_historicos <- tickets %>%
    filter(id_cliente_enc == cliente) %>%
    count(cod_est, sort = TRUE) %>%
    filter(!(cod_est %in% productos_cesta)) %>%
    slice_head(n = 5)

  if(nrow(productos_historicos) > 0) {
    producto_recomendado <- productos_historicos$cod_est[1]
    frecuencia_historica <- productos_historicos$n[1]
    metodo <- "historico_personal"
  } else {
    productos_populares <- tickets %>%
      filter(!(cod_est %in% productos_cesta)) %>%
      count(cod_est, sort = TRUE) %>%
      slice_head(n = 1)

    producto_recomendado <- productos_populares$cod_est[1]
    frecuencia_historica <- 0
    metodo <- "popular_global"
  }

  recomendacion <- data.frame(
    id_cliente_enc = cliente,
    producto_recomendado = producto_recomendado,
    frecuencia_historica = frecuencia_historica,
    metodo_recomendacion = metodo,
    productos_en_ultima_cesta = length(productos_cesta)
  )

  recomendaciones_finales <- rbind(recomendaciones_finales, recomendacion)
}

# PASO 4: Enriquecer con descripciones de productos
cat("\n=== PASO 4: ENRIQUECIENDO RESULTADOS ===\n")

if(is.data.frame(productos) || is.data.table(productos)) {
  recomendaciones_finales <- recomendaciones_finales %>%
    left_join(productos %>% select(cod_est, descripcion),
              by = c("producto_recomendado" = "cod_est"))
} else {
  recomendaciones_finales$descripcion <- paste("Producto", recomendaciones_finales$producto_recomendado)
}

# Agregar fecha de última compra
recomendaciones_finales <- recomendaciones_finales %>%
  left_join(productos_ultima_compra %>% select(id_cliente_enc, dia_compra, num_productos),
            by = "id_cliente_enc")

# PASO 5: Crear tabla final limpia
tabla_final <- recomendaciones_finales %>%
  select(
    Cliente = id_cliente_enc,
    Producto_Recomendado = producto_recomendado,
    Nombre_Producto = descripcion,
    Frecuencia_Historica = frecuencia_historica,
    Productos_en_Cesta = productos_en_ultima_cesta,
    Dia_Ultima_Compra = dia_compra
  ) %>%
  arrange(desc(Frecuencia_Historica))

# ostrar tabla limpia con fecha en formato legible y sin la columna Metodo
tabla_final_limpia <- tabla_final %>%
  mutate(Dia_Ultima_Compra = format(ymd(Dia_Ultima_Compra), "%d/%m/%Y"))

print("TABLA FINAL LIMPIA:")
print(tabla_final_limpia)

# VERIFICACIÓN (opcional)
cat("\n=== VERIFICACIÓN ===\n")
verificacion <- ultimas_compras %>%
  group_by(id_cliente_enc) %>%
  summarise(
    productos_cesta_ejemplo = paste(head(unique(cod_est), 3), collapse = ", "),
    .groups = "drop"
  ) %>%
  inner_join(recomendaciones_finales %>%
               select(id_cliente_enc, producto_recomendado, descripcion),
             by = "id_cliente_enc")

print(verificacion %>%
        select(
          Cliente = id_cliente_enc,
          Productos_en_Cesta = productos_cesta_ejemplo,
          Producto_Recomendado = producto_recomendado,
          Nombre_Recomendado = descripcion
        ))

# ESTADÍSTICAS DEL MODELO
cat("\n=== ESTADÍSTICAS ===\n")
cat("Total clientes procesados:", nrow(recomendaciones_finales), "\n")
cat("Recomendaciones basadas en historial personal:",
    sum(recomendaciones_finales$metodo_recomendacion == "historico_personal"), "\n")
cat("Recomendaciones basadas en popularidad global:",
    sum(recomendaciones_finales$metodo_recomendacion == "popular_global"), "\n")
cat("Frecuencia histórica promedio:",
    round(mean(recomendaciones_finales$frecuencia_historica), 2), "\n")
write.csv(tabla_final_limpia, "DATOS/Datos Shiny/recomendaciones_clientes.csv", row.names = FALSE)
# o en Excel:
library(openxlsx)
write.xlsx(tabla_final_limpia, "DATOS/Datos Shiny/recomendaciones_clientes.xlsx")
