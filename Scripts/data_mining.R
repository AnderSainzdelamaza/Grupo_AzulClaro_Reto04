# Librerías
library(cluster)
library(factoextra)
library(dplyr)
library(plotly)
library(RColorBrewer)
library(tidyr)

# Cargar datos para clustering
datos_clientes <- readRDS("Datos/datos_para_clustering.RDS")

# Cargar funciones
source("Scripts/funciones.R")

# ---------------------------------
# DETECCIÓN Y ELIMINACIÓN DE OUTLIERS
# ---------------------------------

# Seleccionar las columnas numéricas para la detección de outliers
columnas_numericas <- names(datos_clientes)[sapply(datos_clientes, is.numeric)]
columnas_numericas <- setdiff(columnas_numericas, "id_cliente_enc")  # Excluir ID

# Detectar outliers
outliers_idx <- detect_outliers(datos_clientes, columnas_numericas)
cat("Número de outliers detectados:", length(outliers_idx), "\n")
cat("Porcentaje de outliers:", round(length(outliers_idx)/nrow(datos_clientes)*100, 2), "%\n")

# Crear un dataframe sin outliers
datos_sin_outliers <- datos_clientes[-outliers_idx, ]
cat("Dimensiones originales:", dim(datos_clientes), "\n")
cat("Dimensiones después de eliminar outliers:", dim(datos_sin_outliers), "\n")

# Usar los datos sin outliers para el proceso de clustering
datos_clientes <- datos_sin_outliers

# Guardar los datos sin outliers (opcional)
saveRDS(datos_sin_outliers, "Datos/datos_sin_outliers.RDS")

# AHORA CONTINUAMOS CON EL CLUSTERING COMO EN TU CÓDIGO ORIGINAL
# Escalado
datos_scaled <- scale(datos_clientes[,-1])
rownames(datos_scaled) <- datos_clientes$id_cliente_enc
# -------------------------------
# MÉTODO DEL CODO PARA K-MEANS
# -------------------------------
set.seed(123)
# Calculamos la suma de cuadrados intra-cluster para diferentes valores de k
wss <- sapply(1:10, function(k) {
  kmeans(datos_scaled, centers = k, nstart = 25)$tot.withinss
})

# Gráfico del método del codo con plotly
elbow_plot <- plot_ly(x = 1:10, y = wss, type = "scatter", mode = "lines+markers") %>%
  layout(title = "Método del Codo para K-Means",
         xaxis = list(title = "Número de Clusters"),
         yaxis = list(title = "Suma de Cuadrados Intra-Cluster"),
         annotations = list(
           x = 3,
           y = wss[3],
           text = "Punto de inflexión",
           showarrow = TRUE,
           arrowhead = 1
         ))

# Guardamos el gráfico
htmlwidgets::saveWidget(elbow_plot, "Resultados/metodo_codo_kmeans.html")

# -------------------------------
# K-MEANS CON K=3
# -------------------------------
set.seed(123)

kmeans_res <- kmeans(datos_scaled, centers = 3)
sil_kmeans <- silhouette(kmeans_res$cluster, dist(datos_scaled))
media_sil_kmeans <- mean(sil_kmeans[, 3])

# -------------------------------
# CLUSTERING JERÁRQUICO
# -------------------------------
d <- dist(datos_scaled)
hc <- hclust(d, method = "ward.D2")
cutree_hc <- cutree(hc, k = 3)
sil_hc <- silhouette(cutree_hc, d)
media_sil_hc <- mean(sil_hc[, 3])

# -------------------------------
# VISUALIZACIÓN DE CLUSTERS CON PLOTLY
# -------------------------------
# Reducción de dimensionalidad para visualización
pca_res <- prcomp(datos_scaled)
pca_data <- as.data.frame(pca_res$x[,1:2])
pca_data$kmeans_cluster <- as.factor(kmeans_res$cluster)
pca_data$hc_cluster <- as.factor(cutree_hc)

# Gráfico de dispersión para K-Means
kmeans_plot <- plot_ly(pca_data, x = ~PC1, y = ~PC2, color = ~kmeans_cluster,
                       type = "scatter", mode = "markers",
                       marker = list(size = 8, opacity = 0.7),
                       colors = c("red", "blue", "green")) %>%
  layout(title = "Clustering K-Means (k=3)",
         xaxis = list(title = "Componente Principal 1"),
         yaxis = list(title = "Componente Principal 2"),
         legend = list(title = list(text = "Cluster")))

# Gráfico de dispersión para Clustering Jerárquico
hc_plot <- plot_ly(pca_data, x = ~PC1, y = ~PC2, color = ~hc_cluster,
                   type = "scatter", mode = "markers",
                   marker = list(size = 8, opacity = 0.7),
                   colors = c("red", "blue", "green")) %>%
  layout(title = "Clustering Jerárquico (k=3)",
         xaxis = list(title = "Componente Principal 1"),
         yaxis = list(title = "Componente Principal 2"),
         legend = list(title = list(text = "Cluster")))

# Guardamos los gráficos
htmlwidgets::saveWidget(kmeans_plot, "Resultados/kmeans_clusters_plot.html")
htmlwidgets::saveWidget(hc_plot, "Resultados/hc_clusters_plot.html")

# -------------------------------
# DISTRIBUCIONES DE VARIABLES POR CLUSTER
# -------------------------------
# Añadimos los clusters a los datos originales
datos_completos <- datos_clientes
datos_completos$kmeans_cluster <- kmeans_res$cluster
datos_completos$hc_cluster <- cutree_hc
write.csv(datos_completos, "DATOS/DatosShiny.csv")

# Seleccionamos algunas variables importantes para visualizar
variables_importantes <- names(datos_clientes)[2:5]  # Ajustar según las variables específicas
if (!dir.exists("Resultados")) {
  dir.create("Resultados")
}


# Crear boxplots para K-Means
boxplots_kmeans <- crear_boxplots(datos_completos, "kmeans_cluster", "kmeans")

# Crear boxplots para Clustering Jerárquico
boxplots_hc <- crear_boxplots(datos_completos, "hc_cluster", "hc")

# -------------------------------
# COMPARACIÓN DE MÉTODOS
# -------------------------------
resultados <- data.frame(
  Metodo = c("K-Means",  "Jerárquico"),
  Silueta_Media = c(media_sil_kmeans, media_sil_hc)
)

# Visualizar comparación de siluetas
silueta_plot <- plot_ly(resultados, x = ~Metodo, y = ~Silueta_Media, type = "bar",
                        marker = list(color = c("rgba(50, 171, 96, 0.7)", "rgba(219, 64, 82, 0.7)"))) %>%
  layout(title = "Comparación de Coeficientes de Silueta",
         xaxis = list(title = ""),
         yaxis = list(title = "Coeficiente de Silueta Media"))

# Guardar el gráfico
htmlwidgets::saveWidget(silueta_plot, "Resultados/comparacion_siluetas.html")

write.csv(resultados, "Resultados/comparativa_silueta.csv", row.names = FALSE)
print(resultados)
# -------------------------------
# ----------------------------------------
# Cargar librerías necesarias
# ----------------------------------------
library(dplyr)
library(plotly)
library(RColorBrewer)
library(htmlwidgets)

# ----------------------------------------
# GRÁFICO RADAR PARA K-MEANS
# ----------------------------------------

# Paso 1: Aplicar K-means (asegúrate de haber definido datos_clientes antes)
set.seed(123)  # para reproducibilidad
kmeans_result <- kmeans(datos_clientes[,-1], centers = 3)

# Paso 2: Guardar los clusters
clustering_kmeans <- kmeans_result$cluster

# Paso 3: Calcular los centroides de cada cluster
centroides_kmeans <- aggregate(datos_clientes[,-1], by = list(cluster = clustering_kmeans), FUN = mean)

# Paso 4: (Opcional) Eliminar la columna del número de cluster para graficar
centroides_kmeans_sin_cluster <- centroides_kmeans[,-1]

# Preparar datos para el gráfico de radar de K-means
radar_data_kmeans <- as.data.frame(t(centroides_kmeans))
radar_data_kmeans$Variable <- rownames(radar_data_kmeans)


# Crear y guardar el gráfico de radar para K-means
radar_plot_kmeans <- create_radar_chart(radar_data_kmeans, "Perfiles de Cluster (Clustering K-means)")
htmlwidgets::saveWidget(radar_plot_kmeans, "Resultados/radar_centroides_kmeans.html")

# ----------------------------------------
# GRÁFICO RADAR PARA CLUSTERING JERÁRQUICO
# ----------------------------------------
# Definimos variables para clustering jerárquico
clustering_jerarquico <- cutree_hc  # Asegúrate que esta variable exista con las asignaciones de cluster jerárquico

# Calculamos centroides para clustering jerárquico (con los mismos datos usados para K-means)
centroides_jerarquico <- aggregate(datos_clientes[,-1], by = list(cluster = clustering_jerarquico), mean)
centroides_jerarquico <- centroides_jerarquico[,-1]

# Preparar datos para el gráfico de radar jerárquico
radar_data_jerarquico <- as.data.frame(t(centroides_jerarquico))
radar_data_jerarquico$Variable <- rownames(radar_data_jerarquico)

# Crear y guardar el gráfico de radar para jerárquico
radar_plot_jerarquico <- create_radar_chart(radar_data_jerarquico, "Perfiles de Cluster (Clustering Jerárquico)")
htmlwidgets::saveWidget(radar_plot_jerarquico, "Resultados/radar_centroides_jerarquico.html")

# Opcional: Mostrar los gráficos en RStudio
cat("Mostrando gráfico de radar para K-means\n")
radar_plot_kmeans

cat("Mostrando gráfico de radar para Clustering Jerárquico\n")
radar_plot_jerarquico

write.csv(
  datos_completos[, c("id_cliente_enc", "kmeans_cluster")],
  file = "clientes_con_cluster.csv",
  row.names = FALSE
)

