library(lubridate)
library(dplyr)
library(naniar)
library(tidyr)
library(ggplot2)
library(forcats)
library(plotly)

# Cargar datasets
maestrostr<-readRDS("DATOS/Datos Originales/maestroestr.RDS")
str(maestrostr)
tickets_enc<-readRDS("DATOS/Datos Originales/tickets_enc.RDS")
str(tickets_enc)
tickets_enc$num_ticket<- as.character(tickets_enc$num_ticket)
tickets_enc$dia<- ymd(tickets_enc$dia)

# Hacer left join para conseguir tener toda la información un un único df
data_completa <- tickets_enc %>%
  left_join(maestrostr, by = "cod_est")
str(data_completa)

# Establecemos la paleta
eroski_rojo <- "#F20505"
eroski_azul <- "#0367A6"
eroski_gris <- "#666666"
eroski_fondo <- "#F2F2F2"

# Creamos un tema para aplicarlo a los graficos
tema_eroski <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.background = element_rect(fill = eroski_fondo, color = NA),
      plot.background = element_rect(fill = eroski_fondo, color = NA),
      legend.background = element_rect(fill = eroski_fondo),
      panel.grid.major = element_line(color = "#DADADA"),
      panel.grid.minor = element_blank()
    )+ggplotly()
}

# Top 20 productos
top_productos <- data_completa %>%
  count(descripcion, sort = TRUE) %>%
  slice_max(n, n = 20)

ggplot(top_productos, aes(x = reorder(descripcion, n), y = n)) +
  geom_col(fill = eroski_azul) +
  coord_flip() +
  labs(title = "Top 20 productos más vendidos", x = "Producto", y = "Cantidad vendida")+
  tema_eroski()

# Evolución compras en el periodo seleccionado
compras_dia <- data_completa %>%
  count(dia)

ggplot(compras_dia, aes(x = dia, y = n)) +
  geom_line(color = eroski_rojo) +
  labs(title = "Compras totales por día", x = "Fecha", y = "Número de productos")+
  tema_eroski()

# Segmentación por número de compras
segmentos <- data_completa %>%
  group_by(id_cliente_enc) %>%
  summarise(total = n()) %>%
  mutate(segmento = case_when(
    total <= 10 ~ "Bajo",
    total <= 100 & total>10 ~ "Medio",
    TRUE ~ "Alto"
  ))

ggplot(segmentos, aes(x = segmento)) +
  geom_bar(fill = eroski_rojo) +
  labs(title = "Segmentación de clientes por número de compras", x = "Segmento", y = "Clientes")+
  tema_eroski()

# diversidad <- data_completa %>%
#   group_by(id_cliente_enc) %>%
#   summarise(productos_distintos = n_distinct(cod_est))
#
# ggplot(diversidad, aes(x = productos_distintos)) +
#   geom_histogram(bins)+tema_eroski()

# ------------------------------
#   HÁBITOS POR CADA CLIENTE
# ------------------------------
habitos_cliente <- data_completa %>%
  group_by(id_cliente_enc) %>%
  summarise(
    total_productos = n(),
    tickets_distintos = n_distinct(num_ticket),
    dias_compra = n_distinct(dia),
    productos_distintos = n_distinct(descripcion)
  )
str(habitos_cliente)

# Densidad de productos comprados por cliente
ggplot(habitos_cliente, aes(x = total_productos)) +
  geom_density(fill = eroski_rojo, color=eroski_rojo) +
  labs(
    title = "Distribución de productos comprados por cliente",
    x = "Total productos",
    y = "Densidad"
  ) +
  theme(panel.background = element_rect(fill = eroski_fondo))+
  scale_x_continuous(lim=c(0,400))+
  tema_eroski()

# Boxplot de diversidad de productos
ggplot(habitos_cliente, aes(y = productos_distintos)) +
  geom_boxplot(fill = eroski_rojo, outlier.color = eroski_gris) +
  labs(
    title = "Variedad de productos distintos por cliente",
    y = "Productos distintos"
  ) +
  theme(panel.background = element_rect(fill = eroski_fondo))+
  tema_eroski()

#### LO MISMO PERO EN BARRAS
ggplot(habitos_cliente, aes(x = productos_distintos)) +
  geom_histogram(bins = 30, fill = eroski_rojo, color = "white") +
  labs(
    title = "Distribución de productos distintos por cliente",
    x = "Productos distintos",
    y = "Número de clientes"
  ) +
  scale_x_continuous(lim=c(0,200))+
  tema_eroski()

# Violín días de compra
ggplot(habitos_cliente, aes(x = "Clientes", y = dias_compra)) +
  geom_violin(fill = eroski_rojo, color=eroski_rojo) +
  labs(
    title = "Frecuencia de compra por cliente",
    x = "",
    y = "Días únicos"
  ) +
  theme(
    panel.background = element_rect(fill = eroski_fondo),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )+
  scale_y_continuous(lim=c(0,16)) + tema_eroski()

# Barras por rango de tickets
habitos_cliente %>%
  mutate(grupo_tickets = cut(tickets_distintos,
                             breaks = c(0, 5, 10, 20, 50, 100, Inf),
                             labels = c("1-5", "6-10", "11-20", "21-50", "51-100", "100+"))) %>%
  ggplot(aes(x = grupo_tickets)) +
  geom_bar(fill = eroski_rojo) +
  labs(
    title = "Clientes por rango de tickets",
    x = "Rango de tickets",
    y = "Número de clientes"
  )  + tema_eroski()

# Dispersión de tickets vs. productos
ggplot(habitos_cliente, aes(x = tickets_distintos, y = total_productos)) +
  geom_point(alpha = 0.3, color = eroski_rojo) +
  geom_smooth(method = "lm", color = eroski_azul) +
  labs(
    title = "Relación entre tickets y productos",
    x = "Tickets distintos",
    y = "Productos comprados"
  )  + tema_eroski()

# Compras por dia de la semana
tickets_por_dia_semana <- data_completa %>%
  mutate(dia_semana = wday(dia, label = TRUE, abbr = FALSE, week_start = 1)) %>%
  group_by(dia_semana, dia) %>%
  summarise(tickets = n_distinct(num_ticket), .groups = "drop") %>%
  group_by(dia_semana) %>%
  summarise(promedio_tickets = mean(tickets))

ggplot(tickets_por_dia_semana, aes(x = dia_semana, y = promedio_tickets, group = 1)) +
  geom_line(color = eroski_azul, size = 1.2) +
  geom_point(color = eroski_rojo, size = 3) +
  labs(
    title = "Promedio diario de tickets por día de la semana",
    x = "Día de la semana",
    y = "Tickets promedio"
  ) +tema_eroski()

# Producto más vendido por día de la semana
productos_por_dia <- data_completa %>%
  mutate(dia_semana = wday(dia, label = TRUE, abbr = FALSE, week_start = 1)) %>%
  group_by(dia_semana, descripcion) %>%
  summarise(cantidad = n(), .groups = "drop")

top_3_productos_por_dia <- productos_por_dia %>%
  group_by(dia_semana) %>%
  slice_max(order_by = cantidad, n = 3, with_ties = FALSE) %>%
  ungroup()

# Generar una paleta con 6 colores para los productos
colores_6 <- c(eroski_rojo, eroski_azul,"#F4D35E","#00A087FF","#2E4057", "#FF9F1C")
# Gráfico de barras apiladas con 3 productos por día
ggplot(top_3_productos_por_dia, aes(x = dia_semana, y = cantidad, fill = descripcion)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Top 3 productos más comprados por día de la semana",
    x = "Día de la semana",
    y = "Unidades vendidas",

  ) +
  scale_fill_manual(values=colores_6)+ tema_eroski()

# Clasificar los días en semana vs finde
clientes_por_periodo <- data_completa %>%
  mutate(dia_semana = wday(dia, week_start = 1),
         periodo = ifelse(dia_semana <= 5, "Entre semana", "Fin de semana")) %>%
  group_by(periodo) %>%
  summarise(clientes_unicos = n_distinct(id_cliente_enc), .groups = "drop")

ggplot(clientes_por_periodo, aes(x = periodo, y = clientes_unicos, fill = periodo)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  scale_fill_manual(values = c("Entre semana" = eroski_rojo, "Fin de semana" = eroski_azul)) +
  labs(
    title = "Clientes únicos: Entre semana vs Fin de semana",
    x = "",
    y = "Número de clientes únicos"
  ) +
  theme(
    panel.background = element_rect(fill = eroski_fondo),
    text = element_text(size = 13)
  ) + tema_eroski()

# ######### EL SIGUIENTE YA ESTÁ HECHO EN VIOLÍN
# habitos_cliente <- data_completa %>%
#   group_by(id_cliente_enc) %>%
#   summarise(dias_compra = n_distinct(dia))
#
# ggplot(habitos_cliente, aes(x = dias_compra)) +
#   geom_histogram(bins = 30, fill = "#E6001E", color = "white") +
#   labs(title = "Días únicos de compra por cliente", x = "Días de compra", y = "Cantidad de clientes") +
#   theme_minimal()+
#   scale_x_continuous(lim=c(0,16))
#
# table(habitos_cliente$dias_compra)

# Evolución semanal de compras
data_completa %>%
  mutate(semana = floor_date(dia, "week")) %>%
  count(semana) %>%
  ggplot(aes(x = semana, y = n)) +
  geom_line(color = eroski_azul, size = 1.2) +
  geom_point(color = eroski_rojo, size = 3) +
  labs(title = "Compras semanales", x = "Semana", y = "Total de compras") +
  theme_minimal() + tema_eroski()

# Top productos por día de la semana
top_por_dia <- data_completa %>%
  mutate(dia_semana = wday(dia, label = T, week_start = 1, abbr=F)) %>%
  count(dia_semana, descripcion) %>%
  group_by(dia_semana) %>%
  slice_max(order_by = n, n = 5)

ggplot(top_por_dia, aes(x = dia_semana, y = fct_reorder(descripcion, n))) +
  geom_tile(aes(fill = n), color = "white") +
  scale_fill_gradient(low = eroski_azul, high = eroski_rojo) +
  labs(title = "Top productos por día de la semana", x = "Día", y = "Producto")+ tema_eroski()

# Histograma de los intervalos
intervalos_dias <- data_completa %>%
  arrange(id_cliente_enc, dia) %>%
  group_by(id_cliente_enc) %>%
  mutate(dias_entre_compras = as.numeric(difftime(dia, lag(dia), units = "days"))) %>%
  ungroup() %>%
  filter(!is.na(dias_entre_compras) & dias_entre_compras > 0)

ggplot(intervalos_dias, aes(x = dias_entre_compras)) +
  geom_histogram(bins = 30, fill = eroski_azul, color = "white") +
  labs(
    title = "Distribución de días entre compras",
    x = "Días entre compras",
    y = "Número de casos"
  ) +
  tema_eroski() +
  scale_x_continuous(limits = c(0,30))

# Curvas de densidad de comportamiento de clientes
habitos_cliente_long <- habitos_cliente %>%
  pivot_longer(cols = c(total_productos, tickets_distintos, productos_distintos),
               names_to = "variable",
               values_to = "valor")

ggplot(habitos_cliente_long, aes(x = valor, fill = variable, color = variable)) +
  geom_density(alpha = 0.3) +
  scale_fill_manual(values = c(eroski_rojo, eroski_azul, "#F4D35E")) +
  scale_color_manual(values = c(eroski_rojo, eroski_azul, "#F4D35E")) +
  labs(
    title = "Curvas de densidad de comportamiento de clientes",
    x = "Cantidad",
    y = "Densidad",
    fill = "Variable",
    color = "Variable"
  ) +
  tema_eroski() +
  xlim(0, 200)
