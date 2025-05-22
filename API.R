### Lectura de librerias
library(plumber)
library(rsparse)
library(Matrix)
library(stringr)
library(dplyr)

### Preparacion de los archivos
set.seed(123)

maestroestr <- readRDS("DATOS/Datos Originales/maestroestr.RDS")
maestroestr$cod_est <- str_c("X", maestroestr$cod_est)

Matriz<- read.csv("matriz_reducida.csv", row.names = 1)
Matriz[is.na(Matriz)] <- 0
Matriz <- ifelse(Matriz > 0, 1, 0)
Matriz_sparse <- as(as.matrix(Matriz), "dgCMatrix")

clientes_cluster <- read.csv("clientes_con_cluster.csv")

datos_cluster_1 <- df <- data.frame(
  Cluster = c(1, 2, 3),
  "Promedio de productos comprados" = c(27.6, 60.2, 150.0),
  "Variedad de productos comprados" = c(26.7, 43.6, 89.0),
  "Promedio de dias en los que compra" = c("Bajo (estimado)", "Medio", "Alto"),
  "Compras por semana" = c("Moderada", "Alta", "Muy alta"),
  "Compras entre semana" = c("Moderadas", "Alta", "Muy Alta"),
  "Compras fin de semana" = c("Moderadas", "Alta", "Alta"))

### Preparacion del modelo
modelo <- WRMF$new(rank = 10, lambda = 0.01, feedback = "implicit")
modelo$fit_transform(Matriz_sparse)

### Creacion API
#* @post /Recomendador
#* @param IDCliente ID del cliente al que se quiere analizar
#* @param NumeroRecomendaciones Numero de recomendacione a hacer

function(IDCliente = "0000595d5253705a2bf0c6f05ccbb800",
         NumeroRecomendaciones = 3)  {

  if (!(IDCliente %in% rownames(Matriz))) {
    return(list(error = "Cliente no encontrado"))
  }

  cliente <- which(rownames(Matriz) == IDCliente)
  productos_comprados <- which(Matriz[cliente, ] > 0)

  predicciones <- modelo$predict(Matriz_sparse[cliente, , drop = FALSE], k = ncol(Matriz))
  puntuaciones <- as.numeric(predicciones[1, ])
  productos_ordenados <- order(puntuaciones, decreasing = TRUE)
  productos_recomendables <- setdiff(productos_ordenados, productos_comprados)
  recomendaciones <- head(productos_recomendables, as.numeric(NumeroRecomendaciones))
  nombres_productos <- colnames(Matriz)[recomendaciones]

  nombres_productos <- as.character(nombres_productos)
  productos_necesarios <- maestroestr %>% filter( cod_est %in% nombres_productos) %>% select(descripcion)
  productos_necesarios <- as.vector(productos_necesarios$descripcion)

  Cluster_designado <- clientes_cluster %>% filter(id_cliente_enc %in% IDCliente) %>% select(kmeans_cluster)
  datos_cluster_1 <- datos_cluster_1 %>% filter(Cluster %in% Cluster_designado)

  return(list(Cliente = IDCliente,
    Recomendaciones = productos_necesarios,
    DatosCluster = datos_cluster_1))
}

