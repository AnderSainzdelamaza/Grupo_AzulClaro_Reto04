# Cargar funciones
## Analisis graficos
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

guardar_grafico <- function(nombre_archivo, plot_expr) {
  pdf(file = paste0("graficos/", nombre_archivo, ".pdf"), width = 10, height = 6)
  print(eval(plot_expr))
  dev.off()
}

## Shiny
identificar_productos_comunes <- function(datos_filtrados, n_comunes = 10) {
  clusters <- unique(na.omit(datos_filtrados$cluster))

  top_por_cluster <- lapply(clusters, function(cl) {
    datos_filtrados %>%
      filter(cluster == cl) %>%
      count(descripcion, sort = TRUE) %>%
      slice_head(n = n_comunes) %>%
      pull(descripcion)
  })

  all_productos <- unlist(top_por_cluster)
  productos_duplicados <- names(table(all_productos)[table(all_productos) > 1])

  return(productos_duplicados)
}

## Data mining
detect_outliers <- function(data, columnas, coef = 1.5) {
  outliers_indexes <- c()

  for (col in columnas) {
    q1 <- quantile(data[[col]], 0.25, na.rm = TRUE)
    q3 <- quantile(data[[col]], 0.75, na.rm = TRUE)
    iqr <- q3 - q1

    lower_bound <- q1 - coef * iqr
    upper_bound <- q3 + coef * iqr

    outliers_col <- which(data[[col]] < lower_bound | data[[col]] > upper_bound)
    outliers_indexes <- union(outliers_indexes, outliers_col)
  }

  return(outliers_indexes)
}

crear_boxplots <- function(datos, var_cluster, titulo) {
  plots_list <- list()

  for (var in variables_importantes) {
    # Preparar datos para el boxplot
    data_plot <- datos %>%
      select(!!sym(var), !!sym(var_cluster)) %>%
      rename(cluster = !!sym(var_cluster))

    # Crear boxplot
    p <- plot_ly(data_plot, x = ~cluster, y = as.formula(paste0("~", var)),
                 type = "box", color = ~cluster,
                 colors = c("red", "blue", "green")) %>%
      layout(title = paste("Distribución de", var, "por Cluster"),
             xaxis = list(title = "Cluster"),
             yaxis = list(title = var))

    plots_list[[var]] <- p

    # Guardar gráfico
    file_name <- paste0("Resultados/", titulo, "_", var, ".html")
    htmlwidgets::saveWidget(p, file_name)
  }

  return(plots_list)
}

create_radar_chart <- function(radar_data, titulo) {
  # Definir los ejes (variables)
  variables <- radar_data$Variable

  # Crear un dataframe en formato adecuado para plotly
  plot_data <- radar_data %>% select(-Variable)

  # Crear trazas para cada cluster
  fig <- plot_ly()

  # Usamos una paleta de colores adecuada para el número de clusters
  n_clusters <- ncol(plot_data)
  cluster_colors <- brewer.pal(max(3, n_clusters), "Set1")[1:n_clusters]

  for (i in 1:ncol(plot_data)) {
    cluster_values <- c(plot_data[,i], plot_data[1,i])  # Cerrar el polígono

    fig <- fig %>% add_trace(
      type = 'scatterpolar',
      r = cluster_values,
      theta = c(variables, variables[1]),  # Repetir el primer punto para cerrar el polígono
      name = paste('Cluster', i),
      line = list(color = cluster_colors[i], width = 2),
      fill = 'toself',
      fillcolor = paste0(cluster_colors[i], "50")  # Color semi-transparente
    )
  }

  # Diseño del gráfico
  fig <- fig %>% layout(
    polar = list(
      radialaxis = list(
        visible = TRUE,
        range = c(min(radar_data[,-ncol(radar_data)]) - 0.5,
                  max(radar_data[,-ncol(radar_data)]) + 0.5)
      )
    ),
    title = titulo,
    showlegend = TRUE
  )

  return(fig)
}

## Implementacion WRMF
wrmf_simple <- function(R, alpha = 40, lambda = 0.1, factors = 2, iterations = 5) {
  n_users <- nrow(R)
  n_items <- ncol(R)

  # Preferencias binarias: 1 si hay interacción, 0 si no
  P <- (R > 0) * 1

  # Matriz de confianza
  C <- 1 + alpha * R

  # Inicialización aleatoria de factores
  X <- matrix(runif(n_users * factors), nrow = n_users)  # Usuarios
  Y <- matrix(runif(n_items * factors), nrow = n_items)  # Ítems

  I_f <- diag(factors)  # Identidad para regularización

  for (iter in 1:iterations) {
    cat("Iteración", iter, "\n")

    # Actualizamos los usuarios
    for (u in 1:n_users) {
      Cu <- diag(C[u, ])
      Yt_Cu_Y <- t(Y) %*% Cu %*% Y
      Yt_Cu_pu <- t(Y) %*% Cu %*% P[u, ]
      X[u, ] <- solve(Yt_Cu_Y + lambda * I_f, Yt_Cu_pu)
    }

    # Actualizamos los ítems
    for (i in 1:n_items) {
      Ci <- diag(C[, i])
      Xt_Ci_X <- t(X) %*% Ci %*% X
      Xt_Ci_pi <- t(X) %*% Ci %*% P[, i]
      Y[i, ] <- solve(Xt_Ci_X + lambda * I_f, Xt_Ci_pi)
    }
  }

  # Devolvemos la matriz de predicciones
  return(X %*% t(Y))
}

## Objetivo 3
encontrar_usuarios_similares <- function(usuario_id, su_matriz, top_n = 5) {
  # Obtener similaridades del usuario (excluyendo él mismo)
  similaridades <- su_matriz[usuario_id, ]
  similaridades <- similaridades[similaridades < 1 & similaridades > 0]

  # Ordenar de mayor a menor similaridad (menor distancia = mayor similaridad)
  usuarios_similares <- sort(similaridades, decreasing = T)[1:top_n]

  return(usuarios_similares)
}






