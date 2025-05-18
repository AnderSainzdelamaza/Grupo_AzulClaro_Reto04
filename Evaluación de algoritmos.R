#### Carga de archivos ####
library(recommenderlab)
library(stringr)

Objetivos <- readRDS("DATOS/Datos Originales/objetivos.RDS")

Matriz<- read.csv("matriz_reducida.csv", row.names = 1)
Matriz <- as(Matriz,"matrix")
str(Matriz)
dim(Matriz)
#### Reduccion de la matriz momentanea ####

Clientes_objetivos <- c(Objetivos$objetivo2$obj,Objetivos$objetivo4$obj)
Productos_objetivos <- c(Objetivos$objetivo3$obj,Objetivos$objetivo1$obj)
Productos_objetivos <- str_c("X", Productos_objetivos)

Otros_productos <- setdiff(colnames(Matriz),Productos_objetivos)
muestra_aleatoria <- sample(Otros_productos, 500)
Columnas_finales <- c(muestra_aleatoria,Productos_objetivos)

Otros_clientes <- setdiff(rownames(Matriz),Clientes_objetivos)
muestra_aleatoria_2 <- sample(Otros_clientes, 2000)
Filas_finales <- c(muestra_aleatoria_2,Clientes_objetivos)
Matriz <- Matriz[Filas_finales,Columnas_finales]


#### Evaluacion de distintos algoritmos ####

# Asegúrate de que Matriz esté definida antes de esta línea
# Matriz <- ... # Aquí deberías cargar o crear tu matriz de datos

Matriz<-as(Matriz,"realRatingMatrix")
matriz_binarizada<-binarize(Matriz,minRating=1)

a<-rowCounts(matriz_binarizada)
mean(a)

# Configurar el esquema de evaluación
# Decide si goodRating debe ser 2 o 3 y úsalo consistentemente.
# Mantendremos 2 por ahora, pero recuerda la inconsistencia si evalúas Top-N con 3.
esquema<-evaluationScheme(Matriz,"split",train= 0.8, given=30,goodRating= 2)
esquema2<-evaluationScheme(matriz_binarizada,"split",train= 0.8, given=-1)

traindata<-getData(esquema,"train")

#Crear modelos
rec_model_RANDOM<-Recommender(traindata,method="RANDOM")
rec_model_IBCF<-Recommender(traindata,method="IBCF",param=list( method="Euclidean"))
rec_model_UBCF<-Recommender(traindata,method="UBCF",param=list(nn=30))
rec_model_POPULAR<-Recommender(traindata,method="POPULAR")
rec_model_svdf<-Recommender(traindata,method="SVDF")

#Realizar predicciones
### RATINGS ###
predicciones_random_r<-predict(rec_model_RANDOM,getData(esquema,"known"),type="ratings")
predicciones_ibcf_r<-predict(rec_model_IBCF,getData(esquema,"known"), type="ratings")
predicciones_ubcf_r<-predict(rec_model_UBCF,getData(esquema,"known"),type="ratings")
predicciones_popular_r<-predict(rec_model_POPULAR,getData(esquema,"known"),type="ratings")
predicciones_svdf_r<-predict(rec_model_svdf,getData(esquema,"known"),type="ratings")

### TOPNLIST ###
predicciones_ibcf_t<-predict(rec_model_IBCF,getData(esquema,"known"),type="topNList", n=5)
predicciones_ubcf_t<-predict(rec_model_UBCF,getData(esquema,"known"),type="topNList", n=5)
predicciones_random_t<-predict(rec_model_RANDOM,getData(esquema,"known"),type="topNList",n=5)
predicciones_popular_t<-predict(rec_model_POPULAR,getData(esquema,"known"),type="topNList",n=5)
predicciones_svdf_t<-predict(rec_model_svdf,getData(esquema,"known"),type="topNList",n=5)


# ratings (Esta sección está correcta)
rat1<-calcPredictionAccuracy(predicciones_random_r,getData(esquema,"unknown"))
rat2<-calcPredictionAccuracy(predicciones_ibcf_r,getData(esquema,"unknown"))
rat3<-calcPredictionAccuracy(predicciones_ubcf_r,getData(esquema,"unknown"))
rat4<-calcPredictionAccuracy(predicciones_svdf_r,getData(esquema,"unknown"))
rat5<-calcPredictionAccuracy(predicciones_popular_r,getData(esquema,"unknown"))

# topnlist
# Usando goodRating=3 y given=15 como en tu código original, pero corrigiendo las variables
topn1<-calcPredictionAccuracy(predicciones_random_t,getData(esquema,"unknown"),goodRating=2,given=15)
topn2<-calcPredictionAccuracy(predicciones_ibcf_t,getData(esquema,"unknown"),goodRating=2,given=15)
topn3<-calcPredictionAccuracy(predicciones_ubcf_t,getData(esquema,"unknown"),goodRating=2,given=15)
topn4<-calcPredictionAccuracy(predicciones_svdf_t,getData(esquema,"unknown"),goodRating=2,given=15)
topn5<-calcPredictionAccuracy(predicciones_popular_t,getData(esquema,"unknown"),goodRating=2,given=15)

# Combinar resultados
comp_topNList<-rbind(topn1,topn2,topn3,topn4,topn5)
comp_topNList
comp_rat<-rbind(rat1,rat2,rat3,rat4,rat5)
comp_rat



matriz_binarizada

a<-rowCounts(matriz_binarizada)
mean(a)


traindata<-getData(esquema2,"train")

#Crear modelos
rec_model_RANDOM2<-Recommender(traindata,method="RANDOM")
rec_model_IBCF2<-Recommender(traindata,method="IBCF")
rec_model_UBCF2<-Recommender(traindata,method="UBCF")
rec_model_POPULAR2<-Recommender(traindata,method="POPULAR")

#Realizar predicciones
### RATINGS ###
predicciones_random_r2<-predict(rec_model_RANDOM2,getData(esquema2,"known"),type="ratings")
predicciones_ibcf_r2<-predict(rec_model_IBCF2,getData(esquema2,"known"), type="ratings")
predicciones_ubcf_r2<-predict(rec_model_UBCF2,getData(esquema2,"known"),type="ratings")
predicciones_popular_r2<-predict(rec_model_POPULAR2,getData(esquema2,"known"),type="ratings")

### TOPNLIST ###
predicciones_ibcf_t2<-predict(rec_model_IBCF2,getData(esquema2,"known"),type="topNList", n=5)
predicciones_ubcf_t2<-predict(rec_model_UBCF2,getData(esquema2,"known"),type="topNList", n=5)
predicciones_random_t2<-predict(rec_model_RANDOM2,getData(esquema2,"known"),type="topNList",n=5)
predicciones_popular_t2<-predict(rec_model_POPULAR2,getData(esquema2,"known"),type="topNList",n=5)


# topnlist
# Usando goodRating=3 y given=15 como en tu código original, pero corrigiendo las variables
topn12<-calcPredictionAccuracy(predicciones_random_t2,getData(esquema2,"unknown"),goodRating=2,given=15)
topn22<-calcPredictionAccuracy(predicciones_ibcf_t2,getData(esquema2,"unknown"),goodRating=2,given=15)
topn32<-calcPredictionAccuracy(predicciones_ubcf_t2,getData(esquema2,"unknown"),goodRating=2,given=15)
topn52<-calcPredictionAccuracy(predicciones_popular_t2,getData(esquema2,"unknown"),goodRating=2,given=15)

comp_topNList2<-rbind(topn12,topn22,topn32,topn52)
comp_topNList2
comp_bi_nor<-rbind(comp_topNList2,comp_topNList)
comp_bi_nor
# Añadir nombres a comp_topNList
rownames(comp_topNList) <- c("RANDOM_NOR", "IBCF_NOR", "UBCF_NOR", "SVDF_NOR", "POPULAR_NOR")

# Añadir nombres a comp_topNList2 (binarizada)
rownames(comp_topNList2) <- c("RANDOM_BIN", "IBCF_BIN", "UBCF_BIN", "POPULAR_BIN")

# Combinar los resultados con nombres distintos
comp_bi_nor <- rbind(comp_topNList2, comp_topNList)
comp_bi_nor

# Añadir nombres a comp_rat
rownames(comp_rat) <- c("RANDOM_NOR", "IBCF_NOR", "UBCF_NOR", "SVDF_NOR", "POPULAR_NOR")
r<-round(comp_bi_nor,4)

# Guardar datos para App de Shiny

write.csv(comp_bi_nor, "DATOS/Datos Shiny/resultados_topn_comparativa.csv", row.names = TRUE)
write.csv(comp_rat, "DATOS/Datos Shiny/resultados_ratings_comparativa.csv", row.names = TRUE)

write.csv(comp_topNList, "DATOS/Datos Shiny/resultados_topn_individuales.csv", row.names = TRUE)
write.csv(comp_topNList2, "DATOS/Datos Shiny/resultados_topn_binarizados.csv", row.names = TRUE)

# Crear directorio si no existe
if(!dir.exists("modelos")) dir.create("modelos")

# Guardar todos los modelos
saveRDS(rec_model_RANDOM, "modelos/rec_model_RANDOM.rds")
saveRDS(rec_model_IBCF, "modelos/rec_model_IBCF.rds")
saveRDS(rec_model_UBCF, "modelos/rec_model_UBCF.rds")
saveRDS(rec_model_POPULAR, "modelos/rec_model_POPULAR.rds")
saveRDS(rec_model_svdf, "modelos/rec_model_SVDF.rds")

# también los modelos binarizados:
saveRDS(rec_model_RANDOM2, "modelos/rec_model_RANDOM2.rds")
saveRDS(rec_model_IBCF2, "modelos/rec_model_IBCF2.rds")
saveRDS(rec_model_UBCF2, "modelos/rec_model_UBCF2.rds")
saveRDS(rec_model_POPULAR2, "modelos/rec_model_POPULAR2.rds")

library(shiny)
library(DT)

#UI
ui <- fluidPage(
  titlePanel("Tabla Comparativa Compacta"),

  # Estilo para tabla más pequeña
  tags$style(HTML("
    table.dataTable td {
      padding: 4px !important;
      font-size: 10px;
    }
    table.dataTable th {
      padding: 4px !important;
      font-size: 10px;
    }
  ")),

  DTOutput("tabla_colores")
)
#Server
server <- function(input, output) {
  output$tabla_colores <- renderDT({
    datatable(
      r,
      rownames = T
    ) %>%
      formatStyle('TP', backgroundColor = 'lightgreen') %>%
      formatStyle('TN', backgroundColor = 'lightgreen') %>%
      formatStyle('FP', backgroundColor = 'lightcoral') %>%
      formatStyle('FN', backgroundColor = 'lightcoral')
  })
}

shinyApp(ui = ui, server = server)


