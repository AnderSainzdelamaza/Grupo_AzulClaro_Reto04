
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)
library(lubridate)
library(tidyr)
library(forcats)

# Cargar datos
maestrostr <- readRDS("DATOS/Datos Originales/maestroestr.RDS")
tickets_enc <- readRDS("DATOS/Datos Originales/tickets_enc.RDS")
tickets_enc$num_ticket <- as.character(tickets_enc$num_ticket)
tickets_enc$dia <- ymd(tickets_enc$dia)
data_completa <- tickets_enc %>%
  left_join(maestrostr, by = "cod_est")
datos_completos <- read.csv("DATOS/DatosShiny.csv")

# Paleta y tema
eroski_rojo <- "#F20505"
eroski_azul <- "#0367A6"
eroski_gris <- "#666666"
eroski_fondo <- "#F2F2F2"
tema_eroski <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(panel.background = element_rect(fill = eroski_fondo, color = NA),
          plot.background = element_rect(fill = eroski_fondo, color = NA),
          legend.background = element_rect(fill = eroski_fondo),
          panel.grid.major = element_line(color = "#DADADA"),
          panel.grid.minor = element_blank())
}

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
                                                                     "Jerárquico" = "hc_cluster"))
                                           ),
                                           mainPanel(
                                             plotlyOutput("distrib_compras", height = "500px")
                                           )
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
                                                       min = 3, max = 10, value = 5)
                                         ),
                                         mainPanel(
                                           plotlyOutput("productos_por_cluster", height = "800px")
                                         )
                                       )
                                     )
                            )
                          )
                 ),

                 tabPanel("3 - Resultados de Modelos",
                          fluidPage(
                            h3("Modelo predictivo"),
                            verbatimTextOutput("modelo_output")
                          )
                 ),

                 tabPanel("4 - Resultados Objetivos",
                          fluidPage(
                            h3("Indicadores clave"),
                            tableOutput("kpi_table")
                          )
                 )
)

# Server
server <- function(input, output) {
  output$top_productos <-  renderPlotly({
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
    # Verificar que los datos estén cargados correctamente
    req(data_completa)

    tryCatch({
      # Paso 1: Calcular frecuencia de compra por cliente y producto
      productos_cliente <- data_completa %>%
        group_by(id_cliente_enc, descripcion) %>%
        summarise(veces_comprado = n(), .groups = 'drop')

      # Paso 2: Identificar productos con al menos un cliente que los recompró
      productos_recurrentes <- productos_cliente %>%
        filter(veces_comprado > 1) %>%
        distinct(descripcion) %>%
        mutate(tipo = "Recurrente")

      # Paso 3: Clasificación definitiva de productos
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

      # Paso 4: Seleccionar tops excluyendo duplicados
      top_recurrentes <- clasificacion_final %>%
        filter(tipo == "Recurrente") %>%
        slice_max(order_by = clientes_recurrentes, n = 10)

      top_ocasionales <- clasificacion_final %>%
        filter(tipo == "Ocasional") %>%
        slice_max(order_by = clientes_unicos, n = 10) %>%
        anti_join(top_recurrentes, by = "descripcion")

      # Paso 5: Preparar datos para visualización
      datos_visualizacion <- bind_rows(
        top_recurrentes %>% mutate(categoria = "Recurrentes (recomprados)"),
        top_ocasionales %>% mutate(categoria = "Ocasionales (no recomprados)")
      ) %>%
        mutate(descripcion = fct_reorder(descripcion, clientes_unicos))

      # Paso 6: Crear el gráfico
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
        facet_wrap(~categoria, scales = "free_y", ncol = 1) +
        scale_fill_manual(values = c("Recurrentes (recomprados)" = "#F20505",
                                     "Ocasionales (no recomprados)" = "#0367A6")) +
        labs(title = "Top productos por patrón de compra",
             subtitle = "Productos recurrentes vs ocasionales",
             x = "",
             y = "Número de clientes") +
        theme_minimal() +
        theme(legend.position = "none",
              strip.text = element_text(size = 12, face = "bold"),
              plot.title = element_text(size = 16, face = "bold"),
              plot.subtitle = element_text(size = 12),
              axis.text.y = element_text(size = 10))

      ggplotly(p, tooltip = "text") %>%
        layout(margin = list(l = 150, r = 50),
               hoverlabel = list(bgcolor = "white"))

    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
      return(NULL)
    })
  })
  output$evol_semanal <- renderPlotly({
    req(input$rango_fechas)  # Asegurarse que hay fechas seleccionadas

    dfsemanas <- data_completa %>%
      filter(dia >= input$rango_fechas[1] & dia <= input$rango_fechas[2]) %>%
      mutate(semana = floor_date(dia, "week")) %>%
      count(semana)

    # Verificar que hay datos después del filtrado
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
      # Crear intervalos/bins manualmente para el bar plot
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
      layout(xaxis = list(tickangle = -45)) # Ajustar ángulo en Plotly
  })
  # Gráfico Radar
  output$radar_plot <- renderPlotly({
    req(datos_completos)

    # Calcular centroides para los 3 clusters
    centroides <- datos_completos %>%
      group_by(cluster = .data[[input$metodo_radar]]) %>%
      summarise(across(c(total_productos, productos_distintos, dias_activos,
                         compras_por_semana, compras_entre_semana,
                         compras_fin_de_semana), mean)) %>%
      select(-cluster)

    # Preparar datos para radar plot
    variables <- colnames(centroides)
    n_vars <- length(variables)

    # Crear el gráfico radar interactivo
    fig <- plot_ly(type = 'scatterpolar',
                   fill = 'toself') %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = TRUE,
            range = c(0, max(centroides)*1.1)
          )
        ),
        title = paste("Perfil de los 3 Clusters -",
                      ifelse(input$metodo_radar == "kmeans_cluster", "K-means", "Jerárquico")),
        showlegend = TRUE
      )

    # Añadir cada cluster al gráfico
    colors <- c("#0367A6", "#F20505", "#04B2D9")  # Azul Eroski, Rojo Eroski, Azul claro

    for(i in 1:nrow(centroides)) {
      fig <- fig %>%
        add_trace(
          r = as.numeric(centroides[i, ]),
          theta = variables,
          name = paste("Cluster", i),
          fillcolor = alpha(colors[i], 0.3),
          line = list(color = colors[i], width = 2),
          hoverinfo = "text",
          text = paste0(variables, ": ", round(as.numeric(centroides[i, ]), 2)
          ))
    }

    fig
  })
  # Boxplots
  output$boxplot_clusters <- renderPlotly({
    req(datos_completos)

    # Crear boxplot para los 3 clusters
    p <- ggplot(datos_completos, aes(x = factor(.data[[input$metodo_boxplot]]),
                                     y = .data[[input$variable_boxplot]],
                                     fill = factor(.data[[input$metodo_boxplot]]))) +
      geom_boxplot() +
      scale_fill_manual(values = c("#0367A6", "#F20505", "#04B2D9")) +
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
            axis.title = element_text(size = 12))+
      tema_eroski()
    ggplotly(p, tooltip = "text")
  })
  output$distrib_compras <- renderPlotly({
    req(datos_completos)

    # Preparar datos para el gráfico de barras apiladas
    datos_distrib <- datos_completos %>%
      mutate(cluster = factor(.data[[input$metodo_distrib]])) %>%
      group_by(cluster) %>%
      summarise(
        Entre_Semana = mean(compras_entre_semana),
        Fin_de_Semana = mean(compras_fin_de_semana)
      ) %>%
      pivot_longer(cols = c(Entre_Semana, Fin_de_Semana),
                   names_to = "Tipo", values_to = "Porcentaje")

    # Crear gráfico de barras apiladas
    p <- ggplot(datos_distrib, aes(x = cluster, y = Porcentaje, fill = Tipo,
                                   text = paste("Cluster:", cluster, "<br>Tipo:", Tipo,
                                                "<br>Porcentaje:", round(Porcentaje, 2)))) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = c(eroski_azul, eroski_rojo)) +
      labs(title = "Distribución de compras por tipo de día",
           x = "Cluster",
           y = "Proporción de compras") +
      tema_eroski()

    ggplotly(p, tooltip = "text")
  })
  output$productos_por_cluster <- renderPlotly({
    req(datos_completos, data_completa)

    tryCatch({
      # 1. Unir datos de clusters con transacciones
      datos_con_cluster <- data_completa %>%
        left_join(datos_completos %>%
                    select(id_cliente_enc, cluster = .data[[input$metodo_cluster_productos]]),
                  by = "id_cliente_enc") %>%
        filter(!is.na(cluster))

      # 2. Calcular top productos por cluster
      top_productos_cluster <- datos_con_cluster %>%
        group_by(cluster, descripcion) %>%
        summarise(
          total_compras = n(),
          clientes_unicos = n_distinct(id_cliente_enc),
          .groups = 'drop'
        ) %>%
        group_by(cluster) %>%
        slice_max(order_by = total_compras, n = 5) %>%
        ungroup() %>%
        mutate(cluster = paste("Cluster", cluster),
               descripcion = fct_reorder(descripcion, total_compras))

      # 3. Visualización
      p <- ggplot(top_productos_cluster,
                  aes(x = descripcion, y = total_compras, fill = cluster,
                      text = paste("Cluster:", cluster,
                                   "<br>Producto:", descripcion,
                                   "<br>Total compras:", total_compras,
                                   "<br>Clientes únicos:", clientes_unicos))) +
        geom_col(show.legend = FALSE) +
        coord_flip() +
        facet_wrap(~cluster, scales = "free_y", ncol = 1) +
        scale_fill_manual(values = c("#0367A6", "#F20505", "#04B2D9")) +
        labs(title = "Top 5 productos más comprados por cluster",
             x = "",
             y = "Total de compras") +
        theme_minimal() +
        theme(strip.text = element_text(face = "bold", size = 12),
              plot.title = element_text(size = 16, face = "bold"),
              axis.text.y = element_text(size = 10))

      ggplotly(p, tooltip = "text") %>%
        layout(margin = list(l = 150, r = 50),
               hoverlabel = list(bgcolor = "white"))

    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
      return(NULL)
    })
  })

}


shinyApp(ui, server)


