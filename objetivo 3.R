library(recommenderlab)
library(stringr)
library(rsparse)
productos<-readRDS("DATOS/Datos Originales/maestroestr.RDS")
Matriz<- read.csv("matriz_reducida.csv", row.names = 1)
Matriz<-as(Matriz,"matrix")
Matriz2<-as(Matriz,"realRatingMatrix")
matriz_binarizada<-binarize(Matriz2,minRating=1)

Objetivos <- readRDS("DATOS/Datos Originales/objetivos.RDS")
obj3<-str_c("X",Objetivos$objetivo3$obj)
matriz<-Matriz[,obj3]
mat<-replace(matriz, is.na(matriz), 0)
mat_rs<-as(mat,"sparseMatrix")
mat_rs
model<-WRMF$new(rank=10,lambda=0.05,feedback="implicit",dynamic_lambda = T)
user_emb <- model$fit_transform(mat) # matriz de factores de los usuarios quita
dim(user_emb)
item_emb <- model$components # matriz de factores de los items
preds <- model$predict(mat_rs, k = 1,
                       not_recommend = mat_rs,
                       n_iter=10000L,output_margin=T, exclude=T)
a<-attr(preds,"scores")
b<-as.data.frame(attr(preds,"ids"))
b$V1<-str_remove_all(b$V1,"X")
library(dplyr)
colnames(b)<-"cod_est"
b$user_id <- rownames(b)
prods <- b %>%
  inner_join(productos, by = "cod_est")
rownames(prods) <- prods$user_id
prods$user_id <- NULL
unique(prods$descripcion)
#Hacer similarity para justificar
####

obj3<-str_c("X",Objetivos$objetivo3$obj)
bin_rs<-as(matriz_binarizada,"dgCMatrix")
matriz2<-matriz_binarizada[,obj3]
mat_rs2<-as(matriz2,"sparseMatrix")
dim(mat_rs2)
model2<-WRMF$new(rank=10,lambda=0.05,feedback="implicit",dynamic_lambda = T)
user_emb2 <- model2$fit_transform(mat_rs2) # matriz de factores de los usuarios quita
dim(user_emb2)
item_emb2 <- model2$components # matriz de factores de los items
dim(item_emb2)
preds2 <- model2$predict(mat_rs2, k = 1,
                       not_recommend = mat_rs2,
                       n_iter=10000L,output_margin=T, exclude=T)
a2<-attr(preds2,"scores")
b2<-as.data.frame(attr(preds2,"ids"))
b2$V1 <-str_remove_all(b2$V1,"X")
library(dplyr)
colnames(b2)<-"cod_est"
b2$user_id <- rownames(b2)
prods2 <- b2 %>%
  inner_join(productos, by = "cod_est")
rownames(prods2) <- prods2$user_id
prods2$user_id <- NULL
unique(prods2$descripcion)
comparacion<-cbind(prods,prods2)
#Similarity
su<-similarity(as(user_emb2,"realRatingMatrix"),which="users",method="Euclidean")
su_matriz<-as.matrix(su)
su_data<-as.data.frame(su)
su_matriz1<-sort(su_matriz[su_matriz<1],decreasing = T)
colnames(su_matriz1)
si<-similarity(as(item_emb2,"realRatingMatrix"),which="users",method="Euclidean")
# 2. Convertir a matriz y dataframe
su_matriz <- as.matrix(su)
su_data <- as.data.frame(su_matriz)

# 3. Función para encontrar usuarios más similares a un usuario específico
encontrar_usuarios_similares <- function(usuario_id, su_matriz, top_n = 5) {
  # Obtener similaridades del usuario (excluyendo él mismo)
  similaridades <- su_matriz[usuario_id, ]
  similaridades <- similaridades[similaridades < 1 & similaridades > 0]

  # Ordenar de mayor a menor similaridad (menor distancia = mayor similaridad)
  usuarios_similares <- sort(similaridades, decreasing = T)[1:top_n]

  return(usuarios_similares)
}
encontrar_usuarios_similares(1,su_matriz,5)
