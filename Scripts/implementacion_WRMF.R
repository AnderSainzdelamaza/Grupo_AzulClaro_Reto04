source("Scripts/funciones.R")

# WRMF simplificado en R usando ALS

# Pequeña matriz de ejemplo: 3 usuarios x 4 ítems
R <- matrix(c(1, 0, 1, 0,
              0, 1, 0, 0,
              1, 1, 0, 0), nrow = 3, byrow = TRUE)

# Ejecutamos WRMF
predicciones <- wrmf_simple(R)

# Mostramos las predicciones
print(round(predicciones, 2))
