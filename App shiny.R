
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
             "Intervalos de días entre compras" = "intervalos"
           )),
           conditionalPanel("input.grafico_seleccionado == 'top'", plotlyOutput("top_productos")),
           conditionalPanel("input.grafico_seleccionado == 'dia'", plotlyOutput("compras_dia")),
           conditionalPanel("input.grafico_seleccionado == 'semana'", plotlyOutput("evol_semanal")),
           conditionalPanel("input.grafico_seleccionado == 'intervalos'", plotlyOutput("intervalos"))
         )
),

                 tabPanel("2 - Clusterización",
                          fluidPage(
                            h3("Segmentación de clientes (k-means)"),
                            sliderInput("clusters", "Número de clusters", min = 2, max = 6, value = 3),
                            plotOutput("kmeans_plot")
                          )
                 ),

                 tabPanel("3 - Resultados de Modelos",
                          fluidPage(
                            h3("Modelo predictivo (Ejemplo)"),
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

  output$evol_semanal <- renderPlotly({
    dfsemanas <- data_completa %>%
      mutate(semana = floor_date(dia, "week")) %>%
      count(semana)
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
        title = list(text = "Compras semanales"),
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
        paper_bgcolor = eroski_fondo,
        font = list(color = eroski_gris)
      )
    p
  })
?lag()
  output$intervalos <- renderPlotly({
      intervalos_dias <- data_completa %>%
        arrange(id_cliente_enc, dia) %>%
        group_by(id_cliente_enc) %>%
        mutate(dias_entre_compras = as.numeric(difftime(dia, lag(dia), units = "days"))) %>%
        ungroup() %>%
        filter(!is.na(dias_entre_compras) & dias_entre_compras > 0)

      p <- ggplot(intervalos_dias, aes(x = dias_entre_compras,
                                       text = paste("Días entre compras:", round(dias_entre_compras, 1)))) +
        geom_histogram(bins = 30, fill = eroski_azul, color = "white") +
        labs(
          title = "Distribución de días entre compras",
          x = "Días entre compras",
          y = "Número de casos"
        ) +
        tema_eroski() +
        scale_x_continuous(limits = c(0,30))

      ggplotly(p, tooltip = "text")
    })

  output$kmeans_plot <- renderPlot({
    habitos_cliente <- data_completa %>%
      group_by(id_cliente_enc) %>%
      summarise(
        total_productos = n(),
        tickets_distintos = n_distinct(num_ticket),
        dias_compra = n_distinct(dia),
        productos_distintos = n_distinct(descripcion)
      )
    habitos_scaled <- scale(habitos_cliente[,-1])
    set.seed(123)
    km <- kmeans(habitos_scaled, centers = input$clusters)
    habitos_cliente$cluster <- factor(km$cluster)
    ggplot(habitos_cliente, aes(x = total_productos, y = productos_distintos, color = cluster)) +
      geom_point() +
      labs(x = "Total productos", y = "Productos distintos") +
      tema_eroski()
  })

  output$modelo_output <- renderPrint({
    # Ejemplo simple: regresión lineal de productos vs tickets
    habitos_cliente <- data_completa %>%
      group_by(id_cliente_enc) %>%
      summarise(
        total_productos = n(),
        tickets_distintos = n_distinct(num_ticket)
      )
    modelo <- lm(total_productos ~ tickets_distintos, data = habitos_cliente)
    summary(modelo)
  })

  output$kpi_table <- renderTable({
    tibble::tibble(
      "Total de clientes" = n_distinct(data_completa$id_cliente_enc),
      "Total de productos vendidos" = nrow(data_completa),
      "Total de tickets" = n_distinct(data_completa$num_ticket)
    )
  })
}

shinyApp(ui, server)
