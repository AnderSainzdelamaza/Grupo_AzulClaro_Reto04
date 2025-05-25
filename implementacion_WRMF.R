# WRMF simplificado en R usando ALS

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

# Pequeña matriz de ejemplo: 3 usuarios x 4 ítems
R <- matrix(c(1, 0, 1, 0,
              0, 1, 0, 0,
              1, 1, 0, 0), nrow = 3, byrow = TRUE)

# Ejecutamos WRMF
predicciones <- wrmf_simple(R)

# Mostramos las predicciones
print(round(predicciones, 2))
