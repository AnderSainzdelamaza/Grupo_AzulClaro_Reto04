#### Lectura archivos y librerias ####

library(ggplot2)
library(cluster)
library(FactoMineR)
library(factoextra)
library(tidyr)
library(dplyr)

tickets_enc <- readRDS("DATOS/Datos Originales/tickets_enc.RDS")

#------------------- TRATAMIENTO DE TICKETS DUPLICADOS ---------------------------
#IDENTIFICACION DE TICKETS DUPLICADOS
df<- tickets_enc %>% group_by(dia, num_ticket, id_cliente_enc) %>%
  summarise(productos = list(table(cod_est)), .groups = "drop")
# hay tickets que se repiten esto esta mal ya que los tickets son unicos por compra
rm(df)

tickets_enc <- tickets_enc %>%
  mutate(
    num_ticket = paste(num_ticket, id_cliente_enc))

#comprobacion de que realmente no se repiten los numeros de ticket
df_ticket <- tickets_enc %>%
  group_by(num_ticket) %>%
  summarise(NumClientesUnicos = n_distinct(id_cliente_enc))
max(df_ticket$NumClientesUnicos)  # es 1 por lo que no se repiten
rm(df_ticket)

#### Analisis de datos y creacion de data.frames necesarios ####

tickets_enc <- tickets_enc %>%
  mutate(num_ticket = paste(num_ticket, id_cliente_enc))
head(tickets_enc)
summary(tickets_enc)

CompraCliente <- tickets_enc %>%
  group_by(id_cliente_enc) %>%
  summarise(NumCompras = n())
head(CompraCliente)

ClienteProducto <- tickets_enc %>%
  group_by(cod_est) %>%
  summarise(NumClientes = n_distinct(id_cliente_enc))
head(ClienteProducto)

#### Matriz de datos ####

Matriz_datos <- tickets_enc %>%
  group_by(id_cliente_enc, cod_est) %>%
  summarise(Frecuencia = n()) %>%
  pivot_wider(names_from = cod_est, values_from = Frecuencia)

matriz <- as.matrix(Matriz_datos[, -1])
rownames(matriz) <- Matriz_datos$id_cliente_enc

#### Matriz de datos reducida ####

CompraClienteFiltrados <- CompraCliente %>% filter(NumCompras > quantile(NumCompras, 0.5))
ClienteProductoFiltrados <- ClienteProducto %>% filter(NumClientes > quantile(NumClientes, 0.5))

matriz_reducida <- matriz[rownames(matriz) %in% CompraClienteFiltrados$id_cliente_enc,
                          colnames(matriz) %in% ClienteProductoFiltrados$cod_est]

write.csv(matriz_reducida, "matriz_reducida.csv")

# # Crear un nuevo dataframe agrupado por día con cantidad de clientes y tickets únicos
# tickets_enc$dia<-lubridate::ymd(tickets_enc$dia)
#
# df_diario <- tickets_enc %>%
#   group_by(dia) %>%
#   summarise(NumClientesUnicos = n_distinct(id_cliente_enc),
#             NumTicketsUnicos = n_distinct(cod_est))
#
# # Ver los primeros registros
# head(df_diario)



