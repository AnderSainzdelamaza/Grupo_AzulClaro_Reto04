#### Carga de archivos ####
library(recommenderlab)
library(stringr)

Objetivos <- readRDS("DATOS/Datos Originales/objetivos.RDS")

Matriz<- read.csv("matriz_reducida.csv", row.names = 1)
Matriz <- as(Matriz,"matrix")
str(Matriz)
#### Reduccion de la matriz momentanea ####
set.seed(7)
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
dim(Matriz)
Matriz<-as(Matriz,"realRatingMatrix")
dim(Matriz)
matriz_binarizada<-binarize(Matriz,minRating=1)
dim(matriz_binarizada)

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
rec_model_ALS_NOR <- Recommender(getData(esquema, "train"), method = "ALS",
                                 parameter = list(normalize = "center", lambda = 0.1, n_factors = 10, n_iter = 10))


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
predicciones_als_t <- predict(rec_model_ALS_NOR, getData(esquema, "known"), type = "topNList", n = 5)


# ratings
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
topnALS <- calcPredictionAccuracy(predicciones_als_t, getData(esquema, "unknown"), goodRating = 2, given = 15)

# Combinar resultados
comp_topNList<-rbind(topn1,topn2,topn3,topn4,topn5)
comp_topNList
comp_rat<-rbind(rat1,rat2,rat3,rat4,rat5)
comp_rat
comp_topNList <- rbind(comp_topNList, ALS_NOR = topnALS)

# Reasignar nombres para asegurarte que están correctos (ahora con 6 modelos)
rownames(comp_topNList) <- c("RANDOM_NOR", "IBCF_NOR", "UBCF_NOR", "SVDF_NOR", "POPULAR_NOR", "ALS_NOR")


comp_bi_nor <- rbind(comp_topNList2, comp_topNList)

round(comp_bi_nor, 4)



matriz_binarizada

a<-rowCounts(matriz_binarizada)
mean(a)


traindata<-getData(esquema2,"train")

#Crear modelos
rec_model_RANDOM2<-Recommender(traindata,method="RANDOM")
rec_model_IBCF2<-Recommender(traindata,method="IBCF")
rec_model_UBCF2<-Recommender(traindata,method="UBCF")
rec_model_POPULAR2<-Recommender(traindata,method="POPULAR")
rec_model_ALS_BIN <- Recommender(getData(esquema2, "train"), method = "ALS",
                                 parameter = list(normalize = "center", lambda = 0.1, n_factors = 10, n_iter = 10))

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
predicciones_als_t2 <- predict(rec_model_ALS_BIN, getData(esquema2, "known"), type = "topNList", n = 5)


# topnlist
# Usando goodRating=3 y given=15 como en tu código original, pero corrigiendo las variables
topn12<-calcPredictionAccuracy(predicciones_random_t2,getData(esquema2,"unknown"),goodRating=2,given=15)
topn22<-calcPredictionAccuracy(predicciones_ibcf_t2,getData(esquema2,"unknown"),goodRating=2,given=15)
topn32<-calcPredictionAccuracy(predicciones_ubcf_t2,getData(esquema2,"unknown"),goodRating=2,given=15)
topn52<-calcPredictionAccuracy(predicciones_popular_t2,getData(esquema2,"unknown"),goodRating=2,given=15)
topnALS2 <- calcPredictionAccuracy(predicciones_als_t2, getData(esquema2, "unknown"), goodRating = 2, given = 15)

comp_topNList2<-rbind(topn12,topn22,topn32,topn52)
comp_topNList2
comp_bi_nor<-rbind(comp_topNList2,comp_topNList)
comp_bi_nor
# Añadir nombres a comp_topNList
rownames(comp_topNList) <- c("RANDOM_NOR", "IBCF_NOR", "UBCF_NOR", "SVDF_NOR", "POPULAR_NOR")


rownames(comp_topNList2) <- c("RANDOM_BIN", "IBCF_BIN", "UBCF_BIN", "POPULAR_BIN")

comp_bi_nor <- rbind(comp_topNList2, comp_topNList)
comp_bi_nor

rownames(comp_rat) <- c("RANDOM_NOR", "IBCF_NOR", "UBCF_NOR", "SVDF_NOR", "POPULAR_NOR")
r<-round(comp_bi_nor,4)

comp_topNList2 <- rbind(comp_topNList2, ALS_BIN = topnALS2)


rownames(comp_topNList2) <- c("RANDOM_BIN", "IBCF_BIN", "UBCF_BIN", "POPULAR_BIN", "ALS_BIN")

comp_bi_nor <- rbind(comp_topNList2, comp_topNList)

# Ver tabla final redondeada
round(comp_bi_nor, 4)

# Coverage
# Novelty

