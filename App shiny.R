library(tibble)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)
library(lubridate)
library(tidyr)
library(forcats)
library(DT)

# Cargar datos
## Datos analisis exploratorio
maestrostr <- readRDS("DATOS/Datos Originales/maestroestr.RDS")
tickets_enc <- readRDS("DATOS/Datos Originales/tickets_enc.RDS")
tickets_enc$num_ticket <- as.character(tickets_enc$num_ticket)
tickets_enc$dia <- ymd(tickets_enc$dia)

data_completa <- tickets_enc %>%
  left_join(maestrostr, by = "cod_est")

str(data_completa)
## Datos clusters
datos_completos <- read.csv("DATOS/DatosShiny.csv")
str(datos_completos)
## Datos modelos
comp_bi_nor <- read.csv("DATOS/Datos Shiny/resultados_topn_comparativa.csv")
str(comp_bi_nor)
comp_rat <- read.csv("DATOS/Datos Shiny/resultados_ratings_comparativa.csv")
str(comp_rat)

## Datos objetivos
df_clientes_recomendados <- read.csv("DATOS/Datos Shiny/df_clientes_recomendados.csv")
df_productos_similares <- read.csv("DATOS/Datos Shiny/df_productos_similares.csv")
prods <- read.csv("DATOS/Datos Shiny/prods.csv")
prods <- prods[complete.cases(prods), ]
prods2 <- read.csv("DATOS/Datos Shiny/prods2.csv")
prods2 <- prods2[complete.cases(prods2), ]
comparacion <- read.csv("DATOS/Datos Shiny/comparacion.csv")
comparacion <- comparacion[complete.cases(comparacion), ]
tabla_final_limpia <- read.csv("DATOS/Datos Shiny/recomendaciones_clientes.csv")
resultados <- readRDS("DATOS/Datos Shiny/recomendaciones_als_final.RDS")

# Paleta y tema
eroski_rojo <- "#F20505"
eroski_azul <- "#0367A6"
eroski_gris <- "#666666"
eroski_fondo <- "#F2F2F2"

# Cargar funciones
source("funciones.R")

# UI
ui <- navbarPage("Reto 4: Eroski",

                 tabPanel("1 - Análisis Exploratorio",
                          fluidPage(
                            h4("Selecciona un gráfico para visualizar"),
                            selectInput("grafico_seleccionado", "Gráfico:", choices = list(
                              "Top 20 productos más vendidos" = "top",
                              "Compras totales por día" = "dia",
                              "Evolución semanal compras" = "semana",
                              "Intervalos de días entre compras" = "intervalos",
                              "Top productos: Recurrentes vs Ocasionales" = "cliente"
                            )),
                            conditionalPanel("input.grafico_seleccionado == 'top'", plotlyOutput("top_productos")),
                            conditionalPanel("input.grafico_seleccionado == 'dia'", plotlyOutput("compras_dia")),
                            conditionalPanel("input.grafico_seleccionado == 'semana'",
                                             dateRangeInput("rango_fechas", "Selecciona rango de fechas:",
                                                            start = min(data_completa$dia),
                                                            end = max(data_completa$dia),
                                                            min = min(data_completa$dia),
                                                            max = max(data_completa$dia),
                                                            language = "es",
                                                            separator = " a "),
                                             plotlyOutput("evol_semanal")),
                            conditionalPanel("input.grafico_seleccionado == 'intervalos'", plotlyOutput("intervalos")),
                            conditionalPanel("input.grafico_seleccionado == 'cliente'", plotlyOutput("prodcliente"))
                          )
                 ),

                 tabPanel("2 - Clusterización",
                          fluidPage(
                            h3("Segmentación de clientes"),
                            tabsetPanel(
                              tabPanel("Gráfico Radar",
                                       fluidPage(
                                         sidebarLayout(
                                           sidebarPanel(
                                             selectInput("metodo_radar", "Método de clustering:",
                                                         choices = c("K-means" = "kmeans_cluster",
                                                                     "Jerárquico" = "hc_cluster"))
                                           ),
                                           mainPanel(
                                             plotlyOutput("radar_plot", height = "600px")
                                           )
                                         )
                                       )
                              ),
                              tabPanel("Boxplots",
                                       fluidPage(
                                         sidebarLayout(
                                           sidebarPanel(
                                             selectInput("metodo_boxplot", "Método de clustering:",
                                                         choices = c("K-means" = "kmeans_cluster",
                                                                     "Jerárquico" = "hc_cluster")),
                                             selectInput("variable_boxplot", "Variable a visualizar:",
                                                         choices = c("Total productos" = "total_productos",
                                                                     "Productos distintos" = "productos_distintos",
                                                                     "Días activos" = "dias_activos",
                                                                     "Compras por semana" = "compras_por_semana",
                                                                     "Compras entre semana" = "compras_entre_semana",
                                                                     "Compras fin de semana" = "compras_fin_de_semana")),
                                             helpText("Visualización para los 3 clusters predefinidos")
                                           ),
                                           mainPanel(
                                             plotlyOutput("boxplot_clusters", height = "500px")
                                           )
                                         )
                                       )
                              ),
                              tabPanel("Distribución Compras",
                                       fluidPage(
                                         sidebarLayout(
                                           sidebarPanel(
                                             selectInput("metodo_distrib", "Método de clustering:",
                                                         choices = c("K-means" = "kmeans_cluster",
                                                                     "Jerárquico" = "hc_cluster")),
                                             selectInput("tipo_grafico_distrib", "Tipo de gráfico:",
                                                         choices = c("Por tipo de día" = "tipo_dia",
                                                                     "Por día de la semana" = "dia_semana"))
                                           ),
                                           mainPanel(
                                             plotlyOutput("distrib_compras", height = "500px")
                                           )
                                         )
                                       )
                              ),
                              tabPanel("Productos por Cluster",
                                       fluidPage(
                                         sidebarLayout(
                                           sidebarPanel(
                                             selectInput("metodo_cluster_productos", "Método de clustering:",
                                                         choices = c("K-means" = "kmeans_cluster",
                                                                     "Jerárquico" = "hc_cluster")),
                                             sliderInput("n_productos", "Número de productos a mostrar:",
                                                         min = 3, max = 10, value = 5),
                                             dateRangeInput("rango_fechas_productos", "Rango de fechas:",
                                                            start = min(data_completa$dia),
                                                            end = max(data_completa$dia)),
                                             actionButton("ejecutar_analisis", "Ejecutar Análisis",
                                                          icon = icon("play"),
                                                          class = "btn-primary")
                                           ),
                                           mainPanel(
                                             plotlyOutput("productos_cluster1", width = "100%"),
                                             plotlyOutput("productos_cluster2", width = "100%"),
                                             plotlyOutput("productos_cluster3", width = "100%")
                                           )
                                         )
                                       )
                              )
                            ),
                          )
                 ),

                 tabPanel("3 - Resultados de Modelos",
                          fluidPage(
                            titlePanel("Comparación de Algoritmos de Recomendación"),
                            tabsetPanel(
                              tabPanel("Comparación General",
                                       fluidRow(
                                         column(6,
                                                h3("Métricas Top-N List"),
                                                plotlyOutput("metricas_topn", height = "500px")
                                         ),
                                         column(6,
                                                h3("Métricas de Ratings"),
                                                plotlyOutput("barras_metricas_ratings", height = "500px")
                                         )
                                       )
                              ),
                              tabPanel("Análisis Detallado",
                                       fluidRow(
                                         column(4,
                                                wellPanel(
                                                  h4("Configuración"),
                                                  selectInput("tipo_metrica", "Tipo de Métrica:",
                                                              choices = c("Top-N List" = "Top-N List",
                                                                          "Ratings" = "Ratings")),
                                                  conditionalPanel(
                                                    "input.tipo_metrica == 'Top-N List'",
                                                    selectInput("metrica_topn", "Métrica a visualizar:",
                                                                choices = c("Precision" = "precision",
                                                                            "Recall" = "recall",
                                                                            "TPR" = "TPR",
                                                                            "FPR" = "FPR"))
                                                  ),
                                                  conditionalPanel(
                                                    "input.tipo_metrica == 'Ratings'",
                                                    selectInput("metrica_ratings", "Métrica a visualizar:",
                                                                choices = c("RMSE" = "RMSE",
                                                                            "MSE" = "MSE",
                                                                            "MAE" = "MAE"))
                                                  )
                                                )
                                         ),
                                         column(8,
                                                plotlyOutput("comparacion_detallada", height = "400px"),
                                                uiOutput("ui_tablas_comparativas")
                                         )
                                       )
                              )
                            )
                          )
                 ),

                 tabPanel("4 - Resultados Objetivos",
                          fluidPage(
                            h3("Resultados por Objetivo"),

                            tabsetPanel(
                              tabPanel("Objetivo 1 - Clientes para Ítem Promocionado",
                                       h4("Top 10 Clientes para Recomendar el Ítem Promocionado"),
                                       dataTableOutput("tabla_clientes_recomendados"),

                                       h4("Productos Similares al Ítem Promocionado"),
                                       dataTableOutput("tabla_productos_similares")
                              ),
                              tabPanel("Objetivo 2 - Recomendación Relevante por Cliente",
                                       h3("Recomendaciones Basadas en Última Compra para 10 Clientes"),
                                       h4("Tabla de Recomendaciones Personalizadas"),
                                       DT::dataTableOutput("tabla_recomendaciones_obj2")
                              ),
                              tabPanel("Objetivo 3 - Ofertas Personalizadas",
                                       fluidPage(
                                         h3("Recomendación de Productos en Oferta para Clientes"),
                                         h4("Modelo con Feedback Implícito (No Binario)"),
                                         dataTableOutput("tabla_recomendaciones_obj3"),
                                         h4("Modelo Binario"),
                                         dataTableOutput("tabla_recomendaciones_obj3_bin"),
                                         h4("Comparación entre Ambos Modelos"),
                                         dataTableOutput("tabla_comparacion_modelos")
                                       )
                              ),
                              tabPanel("Objetivo 4 - Artículo Olvidado en la Última Compra",
                                       h3("Recomendación del Ítem Faltante para Clientes Olvidadizos"),
                                       plotlyOutput("grafico_obj4"),
                                       br(),
                                       h4("Tabla de Recomendaciones por Cliente"),
                                       DT::dataTableOutput("tabla_obj4")
                              )
                            )
                          )
                 )
)


# Server
server <- function(input, output) {
  output$top_productos <- renderPlotly({
    top_productos <- data_completa %>%
      count(descripcion, sort = TRUE) %>%
      slice_max(n, n = 20)

    p <- ggplot(top_productos, aes(x = reorder(descripcion, n), y = n, text = paste("Producto:", descripcion, "<br>Cantidad:", n))) +
      geom_col(fill = eroski_azul) +
      coord_flip() +
      labs(x = "Producto", y = "Cantidad vendida", title="Top 20 productos más vendidos") +
      tema_eroski()

    ggplotly(p, tooltip = "text")
  })

  output$compras_dia <- renderPlotly({
    tickets_por_dia_semana <- data_completa %>%
      mutate(dia_semana = wday(dia, label = TRUE, abbr = FALSE, week_start = 1)) %>%
      group_by(dia_semana, dia) %>%
      summarise(tickets = n_distinct(num_ticket), .groups = "drop") %>%
      group_by(dia_semana) %>%
      summarise(promedio_tickets = mean(tickets))

    p <- ggplot(tickets_por_dia_semana, aes(x = dia_semana, y = promedio_tickets, group = 1,
                                            text = paste("Día:", dia_semana, "<br>Tickets promedio:", round(promedio_tickets, 1)))) +
      geom_line(color = eroski_azul, size = 1.2) +
      geom_point(color = eroski_rojo, size = 3) +
      labs(
        title = "Promedio diario de tickets por día de la semana",
        x = "Día de la semana",
        y = "Tickets promedio"
      ) + tema_eroski()

    ggplotly(p, tooltip = "text")
  })

  output$prodcliente <- renderPlotly({
    req(data_completa)

    tryCatch({
      productos_cliente <- data_completa %>%
        group_by(id_cliente_enc, descripcion) %>%
        summarise(veces_comprado = n(), .groups = 'drop')

      productos_recurrentes <- productos_cliente %>%
        filter(veces_comprado > 1) %>%
        distinct(descripcion) %>%
        mutate(tipo = "Recurrente")

      clasificacion_final <- productos_cliente %>%
        group_by(descripcion) %>%
        summarise(
          total_compras = sum(veces_comprado),
          clientes_unicos = n_distinct(id_cliente_enc),
          clientes_recurrentes = sum(veces_comprado > 1),
          .groups = 'drop'
        ) %>%
        left_join(productos_recurrentes, by = "descripcion") %>%
        mutate(
          tipo = ifelse(is.na(tipo), "Ocasional", tipo),
          ratio_recompra = clientes_recurrentes / clientes_unicos
        )

      top_recurrentes <- clasificacion_final %>%
        filter(tipo == "Recurrente") %>%
        slice_max(order_by = clientes_recurrentes, n = 10)

      top_ocasionales <- clasificacion_final %>%
        filter(tipo == "Ocasional") %>%
        slice_max(order_by = clientes_unicos, n = 10) %>%
        anti_join(top_recurrentes, by = "descripcion")

      datos_visualizacion <- bind_rows(
        top_recurrentes %>% mutate(categoria = "Recurrentes (recomprados)"),
        top_ocasionales %>% mutate(categoria = "Ocasionales (no recomprados)")
      ) %>%
        mutate(descripcion = fct_reorder(descripcion, clientes_unicos))

      p <- ggplot(datos_visualizacion,
                  aes(x = descripcion,
                      y = ifelse(categoria == "Recurrentes (recomprados)",
                                 clientes_recurrentes,
                                 clientes_unicos),
                      fill = categoria,
                      text = paste(
                        "Producto:", descripcion,
                        "<br>Categoría:", categoria,
                        "<br>Total compras:", total_compras,
                        "<br>Clientes únicos:", clientes_unicos,
                        ifelse(categoria == "Recurrentes (recomprados)",
                               paste("<br>Clientes que recompraban:", clientes_recurrentes),
                               "")
                      ))) +
        geom_col() +
        coord_flip() +
        facet_wrap(~categoria, scales = "free", ncol = 1) +
        scale_fill_manual(values = c("Recurrentes (recomprados)" = eroski_rojo,
                                     "Ocasionales (no recomprados)" = eroski_azul)) +
        labs(title = "Top productos por patrón de compra",
             subtitle = "Productos recurrentes vs ocasionales",
             x = "",
             y = "Número de clientes") +
        tema_eroski()

      ggplotly(p, tooltip = "text") %>%
        layout(margin = list(l = 150, r = 50),
               hoverlabel = list(bgcolor = "white"))

    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
      return(NULL)
    })
  })

  output$evol_semanal <- renderPlotly({
    req(input$rango_fechas)

    dfsemanas <- data_completa %>%
      filter(dia >= input$rango_fechas[1] & dia <= input$rango_fechas[2]) %>%
      mutate(semana = floor_date(dia, "week")) %>%
      count(semana)

    validate(
      need(nrow(dfsemanas) > 0, "No hay datos disponibles para el rango de fechas seleccionado")
    )

    p <- plot_ly(
      data = dfsemanas,
      x = ~semana,
      y = ~n,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(color = eroski_rojo, width = 2),
      marker = list(color = eroski_azul, size = 8)
    ) %>%
      layout(
        title = list(
          text = "Compras semanales",
          x = 0.15,
          font = list(
            color = "black",
            size=20
          )
        ),
        xaxis = list(
          title = "Semana",
          gridcolor = "#DADADA",
          zeroline = FALSE
        ),
        yaxis = list(
          title = "Total de compras",
          gridcolor = "#DADADA",
          zeroline = FALSE
        ),
        plot_bgcolor = eroski_fondo,
        paper_bgcolor = eroski_fondo
      )
    p
  })

  output$intervalos <- renderPlotly({
    intervalos_dias <- data_completa %>%
      arrange(id_cliente_enc, dia) %>%
      group_by(id_cliente_enc) %>%
      mutate(dias_entre_compras = as.numeric(difftime(dia, lag(dia), units = "days"))) %>%
      ungroup() %>%
      filter(!is.na(dias_entre_compras) & dias_entre_compras > 0) %>%
      mutate(intervalo = cut(dias_entre_compras, breaks = seq(0, 30, by = 1), right = FALSE)) %>%
      count(intervalo)

    p <- ggplot(intervalos_dias, aes(x = intervalo, y = n,
                                     text = paste("Intervalo:", intervalo, "<br>Número de casos:", n))) +
      geom_bar(stat = "identity", fill = eroski_azul, color = "white") +
      labs(
        title = "Distribución de días entre compras",
        x = "Días entre compras",
        y = "Número de casos"
      ) +
      tema_eroski()

    ggplotly(p, tooltip = "text") %>%
      layout(xaxis = list(tickangle = -45))
  })

  output$radar_plot <- renderPlotly({
    req(datos_completos)

    centroides <- datos_completos %>%
      group_by(cluster = .data[[input$metodo_radar]]) %>%
      summarise(across(c(total_productos, productos_distintos, dias_activos,
                         compras_por_semana, compras_entre_semana,
                         compras_fin_de_semana), mean)) %>%
      select(-cluster)

    variables <- colnames(centroides)
    n_vars <- length(variables)

    fig <- plot_ly(type = 'scatterpolar',
                   fill = 'toself') %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = TRUE,
            range = c(0, max(centroides)*1.1)
          )
        ),
        title = list(
          text = paste("Perfil de los 3 Clusters -",
                       ifelse(input$metodo_radar == "kmeans_cluster", "K-means", "Jerárquico")),
          font = list(color = "black", size = 18)
        ),
        showlegend = TRUE,
        plot_bgcolor = eroski_fondo,
        paper_bgcolor = eroski_fondo,
        font = list(color = "black")
      )

    colors <- c(eroski_azul, eroski_rojo, "#04B2D9")

    for(i in 1:nrow(centroides)) {
      fig <- fig %>%
        add_trace(
          r = as.numeric(centroides[i, ]),
          theta = variables,
          name = paste("Cluster", i),
          fillcolor = alpha(colors[i], 0.3),
          line = list(color = colors[i], width = 2),
          hoverinfo = "text",
          text = paste0(variables, ": ", round(as.numeric(centroides[i, ]), 2))
        )
    }

    fig
  })

  output$boxplot_clusters <- renderPlotly({
    req(datos_completos)

    p <- ggplot(datos_completos, aes(x = factor(.data[[input$metodo_boxplot]]),
                                     y = .data[[input$variable_boxplot]],
                                     fill = factor(.data[[input$metodo_boxplot]]))) +
      geom_boxplot() +
      scale_fill_manual(values = c(eroski_azul, eroski_rojo, "#04B2D9")) +
      labs(title = paste("Distribución de", gsub("_", " ", input$variable_boxplot)),
           subtitle = paste("Método:", ifelse(input$metodo_boxplot == "kmeans_cluster", "K-means", "Jerárquico")),
           x = "Cluster",
           y = NULL,
           fill="Cluster") +
      theme_minimal() +
      theme(legend.position = "none",
            plot.title = element_text(size = 16, face = "bold"),
            plot.subtitle = element_text(size = 12),
            axis.text = element_text(size = 11),
            axis.title = element_text(size = 12)) +
      tema_eroski()
    ggplotly(p, tooltip = "text")
  })

  output$distrib_compras <- renderPlotly({
    req(datos_completos, input$metodo_distrib)

    if(input$tipo_grafico_distrib == "tipo_dia") {
      datos_distrib <- datos_completos %>%
        mutate(cluster = factor(.data[[input$metodo_distrib]])) %>%
        group_by(cluster) %>%
        summarise(
          Entre_Semana = mean(compras_entre_semana),
          Fin_de_Semana = mean(compras_fin_de_semana)
        ) %>%
        pivot_longer(cols = c(Entre_Semana, Fin_de_Semana),
                     names_to = "Tipo", values_to = "Porcentaje")

      p <- ggplot(datos_distrib, aes(x = cluster, y = Porcentaje, fill = Tipo,
                                     text = paste("Cluster:", cluster, "<br>Tipo:", Tipo,
                                                  "<br>Porcentaje:", round(Porcentaje, 2)))) +
        geom_bar(stat = "identity", position = "stack") +
        scale_fill_manual(values = c(eroski_azul, eroski_rojo)) +
        labs(title = "Distribución de compras por tipo de día",
             x = "Cluster",
             y = "Proporción de compras") +
        tema_eroski()

    } else if(input$tipo_grafico_distrib == "dia_semana")  {
      datos_dias <- data_completa %>%
        left_join(
          datos_completos %>%
            select(id_cliente_enc, cluster = .data[[input$metodo_distrib]]),
          by = "id_cliente_enc"
        ) %>%
        mutate(dia_semana = lubridate::wday(dia, label = TRUE, week_start = 1)) %>%
        filter(!is.na(dia_semana), !is.na(cluster)) %>%
        group_by(cluster, dia_semana) %>%
        summarise(n = n(), .groups = "drop") %>%
        group_by(cluster) %>%
        mutate(porcentaje = n / sum(n))

      if(nrow(datos_dias) == 0) {
        return(plotly_empty(type = "scatter", mode = "markers") %>%
                 layout(title = "No hay datos disponibles para este filtro"))
      }

      p <- ggplot(datos_dias, aes(x = dia_semana, y = porcentaje, fill = factor(cluster),
                                  text = paste("Cluster:", cluster,
                                               "<br>Día:", dia_semana,
                                               "<br>Porcentaje:", scales::percent(porcentaje)))) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_fill_manual(values = c(eroski_azul, eroski_rojo, "#04B2D9")) +
        scale_y_continuous(labels = scales::percent) +
        labs(title = "Distribución de compras por día de la semana",
             x = "Día de la semana",
             y = "Porcentaje de compras",
             fill = "Cluster") +
        tema_eroski()
    }

    ggplotly(p, tooltip = "text")
  })

  parametros_analisis <- eventReactive(input$ejecutar_analisis, {
    list(
      fechas = input$rango_fechas_productos,
      metodo = input$metodo_cluster_productos,
      n_top = input$n_productos
    )
  })

  datos_filtrados_react <- reactive({
    req(parametros_analisis())

    data_completa %>%
      filter(
        dia >= parametros_analisis()$fechas[1],
        dia <= parametros_analisis()$fechas[2]
      ) %>%
      left_join(
        datos_completos %>%
          select(id_cliente_enc, cluster = .data[[parametros_analisis()$metodo]]),
        by = "id_cliente_enc"
      )
  })

  productos_comunes_react <- reactive({
    identificar_productos_comunes(datos_filtrados_react())
  })

  output$productos_cluster1 <- renderPlotly({
    req(parametros_analisis(), datos_filtrados_react(), productos_comunes_react())

    datos_cluster1 <- datos_filtrados_react() %>%
      filter(cluster == 1, !descripcion %in% productos_comunes_react())

    top_productos <- datos_cluster1 %>%
      count(descripcion, name = "total_compras") %>%
      slice_max(order_by = total_compras, n = parametros_analisis()$n_top) %>%
      mutate(descripcion = fct_reorder(descripcion, total_compras))

    p <- ggplot(top_productos, aes(x = descripcion, y = total_compras,
                                   text = paste("Producto:", descripcion,
                                                "<br>Total compras:", total_compras,
                                                "<br>Cluster: 1"))) +
      geom_col(fill = eroski_azul) +
      coord_flip() +
      labs(title = "Cluster 1: Productos más comprados (exclusivos)",
           x = "", y = "Total compras") +
      tema_eroski()

    ggplotly(p, tooltip = "text")
  })

  output$productos_cluster2 <- renderPlotly({
    req(parametros_analisis(), datos_filtrados_react(), productos_comunes_react())

    datos_cluster2 <- datos_filtrados_react() %>%
      filter(cluster == 2, !descripcion %in% productos_comunes_react())

    top_productos <- datos_cluster2 %>%
      count(descripcion, name = "total_compras") %>%
      slice_max(order_by = total_compras, n = parametros_analisis()$n_top) %>%
      mutate(descripcion = fct_reorder(descripcion, total_compras))

    p <- ggplot(top_productos, aes(x = descripcion, y = total_compras,
                                   text = paste("Producto:", descripcion,
                                                "<br>Total compras:", total_compras,
                                                "<br>Cluster: 2"))) +
      geom_col(fill = eroski_rojo) +
      coord_flip() +
      labs(title = "Cluster 2: Productos más comprados (exclusivos)",
           x = "", y = "Total compras") +
      tema_eroski()

    ggplotly(p, tooltip = "text")
  })

  output$productos_cluster3 <- renderPlotly({
    req(parametros_analisis(), datos_filtrados_react(), productos_comunes_react())

    datos_cluster3 <- datos_filtrados_react() %>%
      filter(cluster == 3, !descripcion %in% productos_comunes_react())

    top_productos <- datos_cluster3 %>%
      count(descripcion, name = "total_compras") %>%
      slice_max(order_by = total_compras, n = parametros_analisis()$n_top) %>%
      mutate(descripcion = fct_reorder(descripcion, total_compras))

    p <- ggplot(top_productos, aes(x = descripcion, y = total_compras,
                                   text = paste("Producto:", descripcion,
                                                "<br>Total compras:", total_compras,
                                                "<br>Cluster: 3"))) +
      geom_col(fill = "#04B2D9") +
      coord_flip() +
      labs(title = "Cluster 3: Productos más comprados (exclusivos)",
           x = "", y = "Total compras") +
      tema_eroski()

    ggplotly(p, tooltip = "text")
  })

  output$metricas_topn <- renderPlotly({
    metricas_topn <- comp_bi_nor %>%
      as.data.frame() %>%
      mutate(
        Precision = TP / (TP + FP),
        Recall = TP / (TP + FN),
        F1 = 2 * (Precision * Recall) / (Precision + Recall)
      ) %>%
      select(X, Precision, Recall, F1)

    colores_algoritmos <- c(
      eroski_azul,
      eroski_rojo,
      "#F2B705",
      "#8CBF40",
      "#A6528A",
      "#04B2D9"
    )

    fig <- plot_ly(type = 'scatterpolar', fill = 'toself')

    for (i in 1:nrow(metricas_topn)) {
      fig <- fig %>%
        add_trace(
          r = as.numeric(metricas_topn[i, c("Precision", "Recall", "F1")]),
          theta = c("Precision", "Recall", "F1"),
          name = metricas_topn$X[i],
          fillcolor = alpha(colores_algoritmos[i], 0.2),
          line = list(color = colores_algoritmos[i], width = 2),
          hoverinfo = "text",
          text = paste0("<b>", metricas_topn$X[i], "</b><br>",
                        "Precisión: ", round(metricas_topn$Precision[i], 3), "<br>",
                        "Recall: ", round(metricas_topn$Recall[i], 3), "<br>",
                        "F1: ", round(metricas_topn$F1[i], 3))
        )
    }

    fig <- fig %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = TRUE,
            range = c(0, max(metricas_topn[, -1], na.rm = TRUE) * 1.1),
            gridcolor = '#DADADA',
            tickfont = list(color = 'black')
          ),
          angularaxis = list(
            gridcolor = '#DADADA',
            linecolor = '#DADADA',
            tickfont = list(color = 'black')
          )
        ),
        title = list(
          text = "Comparación de Métricas Top-N por Algoritmo",
          font = list(size = 18, color = 'black'),
          x = 0.1
        ),
        showlegend = TRUE,
        legend = list(
          orientation = "h",
          y = -0.2,
          font = list(color = 'black')
        ),
        plot_bgcolor = eroski_fondo,
        paper_bgcolor = eroski_fondo,
        margin = list(t = 80, b = 120)
      )

    fig
  })

  output$barras_metricas_ratings <- renderPlotly({
    datos <- comp_rat %>%
      as.data.frame() %>%
      rownames_to_column("Algoritmo") %>%
      mutate(Algoritmo = factor(Algoritmo, levels = unique(Algoritmo)))

    plot_ly(datos, x = ~X, y = ~RMSE, type = 'bar', name = 'RMSE',
            marker = list(color = eroski_azul),
            text = ~paste("Algoritmo:", X, "<br>RMSE:", round(RMSE, 4)),
            hoverinfo = 'text') %>%
      add_trace(y = ~MAE, name = 'MAE',
                marker = list(color = eroski_rojo),
                text = ~paste("Algoritmo:", X, "<br>MAE:", round(MAE, 4)),
                hoverinfo = 'text') %>%
      layout(
        title = list(
          text = "Comparación de Métricas de Ratings",
          font = list(color = "black", size = 18, x=0.1)
        ),
        yaxis = list(
          title = 'Valor de Métrica',
          gridcolor = "#DADADA"
        ),
        xaxis = list(
          title = 'Algoritmo',
          tickangle = -45,
          gridcolor = "#DADADA"
        ),
        barmode = 'group',
        hoverlabel = list(bgcolor = "white"),
        plot_bgcolor = eroski_fondo,
        paper_bgcolor = eroski_fondo,
        font = list(color = "black")
      )
  })

  output$tabla_topn <- renderDT({
    datos_topn <- comp_bi_nor %>%
      as.data.frame() %>%
      mutate(
        Precision = round(TP/(TP+FP), 3),
        Recall = round(TP/(TP+FN), 3),
        F1 = round(2*(Precision*Recall)/(Precision+Recall), 3)
      ) %>%
      select(X, TP, FP, FN, TN, Precision, Recall, F1)

    datatable(
      datos_topn,
      extensions = 'Buttons',
      options = list(
        dom = 'Blfrtip',
        buttons = c('copy', 'csv', 'excel'),
        pageLength = 10,
        scrollX = TRUE,
        search = list(regex = FALSE, caseInsensitive = TRUE),
        columnDefs = list(
          list(targets = '_all', searchable = TRUE)
        )
      ),
      rownames = FALSE,
      filter = 'none'
    ) %>%
      formatStyle(
        'F1',
        backgroundColor = styleInterval(
          c(0.4, 0.7),
          c('#FF9999', '#FFFF99', '#99FF99')
        ),
        fontWeight = 'bold'
      ) %>%
      formatStyle(
        'X',
        fontWeight = 'bold',
        color = 'white',
        backgroundColor = eroski_azul
      )
  })

  output$tabla_ratings <- renderDT({
    datos_ratings <- comp_rat %>%
      as.data.frame() %>%
      mutate(
        RMSE = round(RMSE, 4),
        MSE = round(MSE, 4),
        MAE = round(MAE, 4)
      ) %>%
      select(X, RMSE, MSE, MAE)

    datatable(
      datos_ratings,
      extensions = 'Buttons',
      options = list(
        dom = 'Blfrtip',
        buttons = c('copy', 'csv', 'excel'),
        pageLength = 10,
        scrollX = TRUE,
        search = list(regex = FALSE, caseInsensitive = TRUE),
        columnDefs = list(
          list(targets = '_all', searchable = TRUE)
        )
      ),
      rownames = FALSE,
      filter = 'none'
    ) %>%
      formatStyle(
        'RMSE',
        backgroundColor = styleInterval(
          c(1.0, 1.5, 2.0),
          c('#99FF99', '#CCFFCC', '#FFFF99', '#FF9999')
        ),
        fontWeight = 'bold'
      ) %>%
      formatStyle(
        'MAE',
        backgroundColor = styleInterval(
          c(0.8, 1.2, 1.8),
          c('#99FF99', '#CCFFCC', '#FFFF99', '#FF9999')
        )
      ) %>%
      formatStyle(
        'X',
        fontWeight = 'bold',
        color = 'white',
        backgroundColor = eroski_rojo
      )
  })

  output$ui_tablas_comparativas <- renderUI({
    if(input$tipo_metrica == "Top-N List") {
      tagList(
        h3("Métricas de Recomendación Top-N"),
        DTOutput("tabla_topn"),
        br(),
        p("Nota: Las métricas Top-N evalúan la capacidad del modelo para recomendar ítems relevantes."),
        p("F1 es la media armónica entre Precisión y Recall."),
        p("TPR (True Positive Rate) equivale al Recall, FPR (False Positive Rate) mide los falsos positivos.")
      )
    } else {
      tagList(
        h3("Métricas de Predicción de Ratings"),
        DTOutput("tabla_ratings"),
        br(),
        p("Nota: Las métricas de Rating evalúan la precisión de las predicciones numéricas."),
        p("RMSE (Root Mean Squared Error): Valores más bajos indican mejor rendimiento."),
        p("MAE (Mean Absolute Error): Error absoluto promedio. Más robusto a outliers que RMSE."),
        p("MSE (Mean Squared Error): Similar a RMSE pero sin la raíz cuadrada. Más sensible a grandes errores.")
      )
    }
  })

  output$comparacion_detallada <- renderPlotly({
    req(comp_bi_nor, comp_rat)

    if(input$tipo_metrica == "Top-N List") {
      datos <- comp_bi_nor %>%
        as.data.frame() %>%
        select(Algoritmo = X,
               Metric = input$metrica_topn) %>%
        mutate(Algoritmo = factor(Algoritmo, levels = unique(Algoritmo)))

      plot_ly(datos,
              x = ~Algoritmo,
              y = ~Metric,
              type = 'bar',
              marker = list(color = eroski_azul),
              text = ~paste("Algoritmo:", Algoritmo,
                            "<br>", input$metrica_topn, ":",
                            round(Metric, 4)),
              hoverinfo = 'text') %>%
        layout(
          title = list(
            text = paste("Comparación de", input$metrica_topn, "en modelos Top-N"),
            font = list(color = "black", size = 18)
          ),
          yaxis = list(
            title = input$metrica_topn,
            gridcolor = "#DADADA"
          ),
          xaxis = list(
            title = "",
            tickangle = -45,
            gridcolor = "#DADADA"
          ),
          hoverlabel = list(bgcolor = "white"),
          plot_bgcolor = eroski_fondo,
          paper_bgcolor = eroski_fondo,
          font = list(color = "black")
        )

    } else {
      datos <- comp_rat %>%
        as.data.frame() %>%
        select(Algoritmo = X,
               Metric = input$metrica_ratings) %>%
        mutate(Algoritmo = factor(Algoritmo, levels = unique(Algoritmo)))

      plot_ly(datos,
              x = ~Algoritmo,
              y = ~Metric,
              type = 'bar',
              marker = list(color = eroski_rojo),
              text = ~paste("Algoritmo:", Algoritmo,
                            "<br>", input$metrica_ratings, ":",
                            round(Metric, 4)),
              hoverinfo = 'text') %>%
        layout(
          title = list(
            text = paste("Comparación de", input$metrica_ratings, "en modelos de Rating"),
            font = list(color = "black", size = 18)
          ),
          yaxis = list(
            title = input$metrica_ratings,
            gridcolor = "#DADADA"
          ),
          xaxis = list(
            title = "",
            tickangle = -45,
            gridcolor = "#DADADA"
          ),
          hoverlabel = list(bgcolor = "white"),
          plot_bgcolor = eroski_fondo,
          paper_bgcolor = eroski_fondo,
          font = list(color = "black")
        )
    }
  })
  output$tabla_clientes_recomendados <- renderDataTable({
    datatable(
      df_clientes_recomendados,
      options = list(pageLength = 10, autoWidth = TRUE),
      rownames = FALSE
    )
  })
  output$tabla_productos_similares <- renderDataTable({
    datatable(
      df_productos_similares,
      options = list(pageLength = 10, autoWidth = TRUE),
      rownames = FALSE
    )
  })
  output$tabla_recomendaciones_obj3 <- renderDataTable({
    datatable(
      prods,
      options = list(pageLength = 10, autoWidth = TRUE),
      rownames = FALSE
    )
  })


  output$tabla_recomendaciones_obj3_bin <- renderDataTable({
    datatable(
      prods2,
      options = list(pageLength = 10, autoWidth = TRUE),
      rownames = FALSE
    )
  })
  output$tabla_comparacion_modelos <- renderDataTable({
    datatable(
      comparacion,
      options = list(pageLength = 10, autoWidth = TRUE),
      rownames = FALSE
    )
  })
  output$tabla_recomendaciones_obj2 <- DT::renderDataTable({
    DT::datatable(
      tabla_final_limpia,
      options = list(pageLength = 10, autoWidth = TRUE),
      rownames = FALSE
    )
  })
  output$grafico_obj4 <- renderPlotly({
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
    pp
  })

  output$tabla_obj4 <- DT::renderDataTable({
    DT::datatable(
      resultados,
      options = list(pageLength = 10, autoWidth = TRUE),
      rownames = FALSE
    )
  })
}


shinyApp(ui, server)


