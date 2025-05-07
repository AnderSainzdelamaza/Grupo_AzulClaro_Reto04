library(dplyr)
library(ggplot2)
library(cluster)
library(FactoMineR)
library(factoextra)
library(tidyr)

# Cargar datos (Asegúrate de tener los archivos CSV o conexión a la base de datos)
tickets_enc <- tickets_enc %>%
  mutate(
    num_ticket = paste(num_ticket, id_cliente_enc))
datos_ventas <- tickets_enc
datos_productos <- maestrostr

# Exploración de datos
head(datos_ventas)
summary(datos_ventas)

# Número de compras por cliente
compras_por_cliente <- datos_ventas %>%
  group_by(id_cliente_enc) %>%
  summarise(NumCompras = n())

# Número de clientes por producto
clientes_por_producto <- datos_ventas %>%
  group_by(cod_est) %>%
  summarise(NumClientes = n_distinct(id_cliente_enc))

# Clustering de clientes basándose en la frecuencia de compra
matriz_clientes_productos <- datos_ventas %>%
  group_by(id_cliente_enc, cod_est) %>%
  summarise(Frecuencia = n()) %>%
  pivot_wider(names_from = cod_est, values_from = Frecuencia, values_fill = list(Frecuencia = 0))

# Convertir a matriz para clustering
matriz <- as.matrix(matriz_clientes_productos[, -1])
rownames(matriz) <- matriz_clientes_productos$ClienteID

# Aplicar K-Means con 4 clústeres (puedes ajustar el número de clusters)
kmeans_result <- kmeans(matriz, centers = 4)
matriz_clientes_productos$Cluster <- kmeans_result$cluster

# Reducción de dimensionalidad con PCA
pca_result <- PCA(matriz, scale.unit = TRUE, ncp = 2)

# Filtrar clientes y productos con poca actividad
clientes_filtrados <- compras_por_cliente %>% filter(NumCompras > quantile(NumCompras, 0.5))
productos_filtrados <- clientes_por_producto %>% filter(NumClientes > quantile(NumClientes, 0.5))

# Construcción de la matriz reducida
matriz_reducida <- matriz[rownames(matriz) %in% clientes_filtrados$ClienteID,
                          colnames(matriz) %in% productos_filtrados$ProductoID]

# Visualizar clustering
fviz_cluster(list(data = matriz, cluster = kmeans_result$cluster))

# Guardar resultados
write.csv(matriz_reducida, "matriz_reducida.csv")

# Crear un nuevo dataframe agrupado por día con cantidad de clientes y tickets únicos
df_diario <- datos_ventas %>%
  group_by(FechaCompra) %>%
  summarise(NumClientesUnicos = n_distinct(ClienteID),
            NumTicketsUnicos = n_distinct(TicketID))

# Ver los primeros registros
head(df_diario)
