
tickets<-readRDS("C:/Users/Ander/OneDrive - Mondragon Unibertsitatea/Archivos de Erik Arbaiza Astorquiza - reto 4 1/DATOS/tickets_enc (1).RDS")
objetivos<-readRDS("C:/Users/Ander/OneDrive - Mondragon Unibertsitatea/Archivos de Erik Arbaiza Astorquiza - reto 4 1/DATOS/objetivos (1).RDS")
productos<-readRDS("C:/Users/Ander/Downloads/maestroestr (2).RDS")


clientes_objetivo2 <- objetivos$objetivo2$obj
print(clientes_objetivo2)


# Filtrar los tickets correspondientes a esos 10 clientes
library(dplyr)

tickets <- tickets %>%
  mutate(id_cliente_enc = as.character(id_cliente_enc))

tickets_filtrados <- tickets %>%
  filter(id_cliente_enc %in% clientes_objetivo2)



# Calcular una recomendación relevante para los 10 clientes, el producto más comprado por cliente
library(dplyr)

recomendaciones <- tickets_filtrados %>%
  group_by(id_cliente_enc, cod_est) %>%
  summarise(frecuencia = n(), .groups = "drop") %>%
  group_by(id_cliente_enc) %>%
  slice_max(order_by = frecuencia, n = 1, with_ties = FALSE)%>%
  arrange(desc(frecuencia))

recomendaciones
