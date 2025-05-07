library(lubridate)
library(dplyr)
library(naniar)
library(tidyr)

maestrostr<-readRDS("DATOS/Datos Originales/maestroestr.RDS")
objetivos<-readRDS("DATOS/Datos Originales/objetivos.RDS")
tickets_enc<-readRDS("DATOS/Datos Originales/tickets_enc.RDS")
str(tickets_enc)
tickets_enc$num_ticket<- as.character(tickets_enc$num_ticket)
tickets_enc$dia<- ymd(tickets_enc$dia)

str(tickets_enc)
summary(tickets_enc)

vis_miss(tickets_enc, warn_large_data = FALSE) # No se encuentran NA's

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

#------------------------------- CLUSTERING ------------------------------------
#IDENTIFICACION DE DIA DE LA SEMANA DE CADA COMPRA
tickets_enc <- tickets_enc %>%
  mutate(DiaSemana = wday(dia, week_start=1))

#CARACTERIZACION DE LOS CLIENTES - COLUMNAS PARA CLUSTERING
datos_clientes <- tickets_enc %>%
  group_by(id_cliente_enc) %>%
  summarise(
    total_productos = n(),  # Número total de compras (filas en la base de datos)
    productos_distintos = n_distinct(cod_est),  # Número de productos únicos comprados
    dias_activos = as.numeric(max(dia) - min(dia)),
    compras_por_semana = ifelse(dias_activos > 0, n() / (dias_activos / 7), n()), # Evitar división por 0
    compras_entre_semana = sum(DiaSemana %in% 1:5),  # Compras de lunes a viernes
    compras_fin_de_semana = sum(DiaSemana %in% 6:7)   # Compras en sábado o domingo
  ) %>%
  ungroup()

saveRDS(datos_clientes, "Datos/datos_para_clustering.RDS")
library(lubridate)
