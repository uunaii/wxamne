####################################### MODELADO ########################################
# 1- TRANSFORMACI?N DEL DATAFRAME -----------------------------------------
dfm <- read.csv("./DatosTransformados/df.csv", sep=",", header = T)
dfm$X <- NULL ; dfm$product_category_1 <- NULL
dfm$product_category_2 <- NULL ; dfm$product_category_3 <- NULL

dim(dfm)
colnames(dfm)
str(dfm)
dfm$transaction_timestamp <- ymd_hms(dfm$transaction_timestamp)
dfm_filtrado <- dfm[dfm$membership_id != "no_socio",]


df_model <- dfm_filtrado
df_model$store <- NULL ; df_model$transaction_id <- NULL ; df_model$transaction_timestamp <- NULL
df_model$product_category_2_name <- NULL ; df_model$product_category_1_name <- NULL
df_model$Qty <- NULL ; df_model$precio_unidad <- NULL

df_modelo <- df_model %>%
  group_by(membership_id, product_category_3_name) %>%
  summarise(sales = sum(sales)
            )
df_rec_ <- spread(data = df_modelo, key = product_category_3_name, value = sales)
df_rec_ <- column_to_rownames(df_rec_, var = "membership_id")
df_rec_ <- df_rec_[,-1]
#
n_miss(df_rec_)
mean(is.na(df_rec_))
#
df_rec_ # esta seria la matriz de recomendacion que se emplearia para el modelado tras unos filtrados
#

# 2- FILTRADO DE LOS CLIENTES CON UN SOLO PRODUCTO COMPRADO ---------------

# Quitamos los clientes que solo han comprado un producto y una sola compra,
# ya que no sirven para la matriz de recomendacion

# muchos clientes (aunque no la mayoria) han comprado pocos productos; eliminamos esos registros

df_rec_<- as.data.frame(cbind(df_rec_, total_na = apply(df_rec_, 1, n_miss)))


# Se quitan los clientes que solo han comprado 6 producto
ncol(df_rec_)-7 # 22 productos --> 209 missings si ha comprado solo 1, es decir, el minimo
quantile(df_rec_$total_na)
quantile(df_rec_$total_na, 0.9) # mas del 10% de los clientes solo han comprado un producto
df_rec <- df_rec_ %>% filter(total_na <=204)
df_rec$total_na <- NULL
# El dataframe se recorta en 10000 instancias, mas o menos ese 18% mencionado
#write.csv(df_rec, "./DatosTransformados/DatosRecomen1.csv")
# 3- CONVERSION A MATRIZ DE PUNTUACIONES -------------------------------------
rm(df_model)
rm(df_modelo)
rm(df_rec_)
rm(dfm)
rm(dfm_filtrado)
rm(df)
#vamos a probar ha hacer todo con 100 filas
matriz_rec <- as.matrix(df_rec)
matriz_rec <- as(matriz_rec,"realRatingMatrix")
matriz_rec
as(matriz_rec, "matrix") #puedo volver a convertirlo a formato matriz
getRatingMatrix(matriz_rec)
# Tambi?n ofrece la misma informacion como una lista de usuarios con sus calificaciones para una inspecci?n m?s detallada
as(matriz_rec, "list")
# ?Cu?ntos tipos de recomendadores hay? Todos estos. Iremos probando algunos
recommenderRegistry$get_entries(dataType = "realRatingMatrix")
#$POPULAR_realRatingMatrix --> item popularity  
#$RE-RECOMMENDER_realRatingMatrix --> Re-recomienda productos
#$UBCF_realRatingMatrix --> user-based collaborative filtering
#$IBCF_realRatingMatrix --> item-based collaborative filtering
#$SVDF_realRatingMatrix --> Singular Value Decomposition con Gradient Descent

# ##### 4.1-ITEM POPULARITY (recomienda los productos mas vendidos) -----------
recomendador_top <- recommenderlab::Recommender(matriz_rec, 
                                                method = "POPULAR")
names(getModel(recomendador_top)) # opciones derivadas del recomendador "popular"
as(getModel(recomendador_top)$topN,"matrix") #mirar la lista puede albergar 100 recomendaciones para 1 usuario


# Pedimos que nos ofrezca 5 art?culos para 2 usuarios nuevos (del dataset original que hemos dejado reservado)
recomienda5 <- predict(recomendador_top, matriz_rec[c(51:52),], n=5)#n numero de items sugeridos
recomienda5
# El resultado contiene dos listas de recomendaciones ordenadas de la parte superior de la lista N, una para cada usuario. 
# Los art?culos recomendados pueden ser inspeccionados como una lista.
as(recomienda5, "list")  

# Ya que las listas est?n ordenadas, podemos extraer sublistas de los mejores art?culos
# Por ejemplo, podemos obtener las 3 mejores recomendaciones para cada lista usando bestN().
recomienda3 <- bestN(recomienda5, n = 3)
recomienda3
as(recomienda3, "list")

# Adem?s, se pueden predecir puntuaciones que dos futuros clientes les dar?an a esos art?culos
recomienda_rating <- predict(recomendador_top, 
                             matriz_rec[c(51:52),], 
                             type="ratings")
recomienda_rating
as(recomienda_rating, "matrix")[,1:10] # pedimos que nos ejemplifique los 10 primeros productos
#No nos es muy util

# Tambi?n podemos solicitar la matriz de clasificaci?n completa que incluye las clasificaciones 
# originales del usuario y las nuevas predicciones.
recomienda_rating_orig <- predict(recomendador_top, 
                                  matriz_rec[c(51:52),], 
                                  type="ratingMatrix")
recomienda_rating_orig
as(recomienda_rating_orig, "matrix")[,1:10]


#  4.2 RE-RECOMENDADOR --------------------------------------------------------
set.seed(666)
particion <- evaluationScheme(matriz_rec, method="split", train=0.8, given=4)
re_recomendador <- recommenderlab::Recommender(getData(particion, "train"),
                                               method = "RERECOMMEND")
re_recomienda <- predict(re_recomendador, 
                         getData(particion, "known"), 
                         n=5)
re_recomienda
as(re_recomienda, "list")
as(re_recomienda, "list")[1]


re_recomienda_rating <- predict(re_recomendador, 
                                getData(particion, "known"), 
                                type="ratings")
re_recomienda_rating
as(re_recomienda_rating, "matrix")[1:5,1:20]

# 4.3 FILTRADO COLABORATIVO USER-BASED ----------------------------------------
  
# Usaremos el m?todo split
# Con una proporci?n del 80% para el train set
# Tendremos en cuenta 15 items para nuestra predicci?n
# Establecemos que >0 es el umbral para determinar que algo gusta

# evaluationScheme() crea 3 conjuntos de datos. 
# Divide los datos en train y conjunto de pruebas, pero dentro del conjunto 
# de pruebas crea adem?s un conjunto de datos conocidos y otro desconocido, 
# que se usar?n para validar las predicciones hechas usando los datos conocidos.

# ########### FILTRADO COLABORATIVO

# Sin normalizar, coseno como ?ndice de similitud
particion <- evaluationScheme(matriz_rec, method="split", train=0.8, given=4)
particion  
User_based_no_norm <- Recommender(getData(particion, "train"), 
                                  "UBCF", 
                                  param=list(normalize = "center", method="Euclidean"))

names(getModel(User_based_no_norm))
getModel(User_based_no_norm)$nn # n?mero de usuarios que evalua (se puede cambiar en param, nn=x)

# Con escalado, coseno como ?ndice de similitud
User_based_norm <- Recommender(getData(particion, "train"),
                               "UBCF",
                               param=list(normalize = "Z-score", method="Euclidean")) #"pearson"/"Euclidean"

# Predecimos con los dos modelos
predicciones1 <- predict(User_based_no_norm, getData(particion, "known"), type="ratingMatrix")
predicciones1
as(predicciones1, "matrix")[,1:10]

predicciones2 <- predict(User_based_norm, getData(particion, "known"), type="ratingMatrix")
predicciones2
as(predicciones2, "matrix")[,1:10]

error1 <- rbind(
  UB = calcPredictionAccuracy(predicciones1, getData(particion, "unknown")),
  UB_Norm = calcPredictionAccuracy(predicciones2, getData(particion, "unknown"))
)
error1

test_error <- calcPredictionAccuracy(predicciones1, getData(particion, "unknown"), byUser = TRUE)
test_error


# FILTRADO COLABORATIVO ITEM-BASED ----------------------------------------
Item_based_no_norm <- Recommender(getData(particion, "train"), 
                                  "IBCF",
                                  param=list(normalize = NULL, method="Cosine"))

Item_based_norm <- Recommender(getData(particion, "train"), 
                               "IBCF",
                               param=list(normalize = "Z-score", method="Cosine"))

names(getModel(Item_based_norm))
getModel(Item_based_norm)$k # n?mero de items que evalua (se puede cambiar en param, k=x)

predicciones4A <- predict(Item_based_no_norm, getData(particion, "known"), type="ratingMatrix") 
predicciones4A
as(predicciones4A, "matrix")[,1:10]

predicciones4B <- predict(Item_based_norm, getData(particion, "known"), type="ratingMatrix") 
predicciones4B
as(predicciones4A, "matrix")[,1:10]

error2 <- rbind(
  IB = calcPredictionAccuracy(predicciones4A, getData(particion, "unknown")),
  IB_Norm = calcPredictionAccuracy(predicciones4B, getData(particion, "unknown"))
)
error2

# MODELO QUE COMETE MENOS ERROS -------------------------------------------
# Miramos a ver con qu? modelo cometemos menos error
library(dplyr)
library(ggplot2)
rbind(error1,error2) %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column(var = "method") %>% 
  ggplot(aes(x = method, y = MAE)) +
  geom_col() +
  theme_minimal()


# #MODELO HYBRIDO ---------------------------------------------------------
Hybrid_recom <- HybridRecommender(Recommender(getData(particion, "train"), "POPULAR"),
                                  Recommender(getData(particion, "train"),"UBCF", param=list(normalize = "Z-score",method="Cosine")),
                                  Recommender(getData(particion, "train"),"IBCF", param=list(normalize = "Z-score",method="Cosine")),
                                  weights = c(.6, .2, .2))

predicciones5 <- predict(Hybrid_recom, getData(particion, "known"), n=5)
predicciones5[c(51:52),]
as(predicciones5, "list")

HB_Nor <- calcPredictionAccuracy(predicciones5, getData(particion, "unknown"), goodRating=0, given=15)
HB_Nor

# COMPARACION DE RECOMENDADORES I -----------------------------------------
recomendacion_top2 <- evaluationScheme(matriz_rec, method="cross", k=10, given=5, goodRating=5)
recomendacion_top2

# Evaluamos las listas de recomendaci?n top-1, top-3 y top-5
resultados <- evaluate(recomendacion_top2, 
                       method="POPULAR", 
                       type = "topNList", # se puede sustituir por ratings quitando el siguiente argumento
                       n=c(1,3,5))
resultados
getConfusionMatrix(resultados)[[1]] # 1er k-fold
avg(resultados) # promedio de los 10k-folds
plot(resultados, annotate=TRUE) # Curva ROC por defecto


# COMPARACION DE RECOMENDADORES II -----------------------------------------

recomendacion_top3 <- evaluationScheme(matriz_rec, method="cross", k=5, given=4, goodRating=5)
recomendacion_top3

algoritmos_variados <- list(
  "aleatorios" = list(name="RANDOM", param=NULL),
  "populares" = list(name="POPULAR", param=NULL),
  "user-based" = list(name="UBCF", param=list(normalize = "Z-score",method="Cosine",nn = 50)),
  "item-based" = list(name="IBCF", param=list(normalize = "Z-score",method="Cosine",k = 50)),
  "SVD-GD" = list(name="SVD", param=list(normalize = "Z-score",k = 5)))

recomendacion_variada <- evaluate(recomendacion_top3,
                                  algoritmos_variados, 
                                  type = "topNList", 
                                  n=c(1, 3, 5, 10, 15, 20))
recomendacion_variada

names(recomendacion_variada)
recomendacion_variada[["user-based"]]
recomendacion_variada[[3]]
plot(recomendacion_variada, annotate=c(1:20), legend="center")
plot(recomendacion_variada, annotate=c(1:20), legend="bottom")

predicciones6 <- evaluate(recomendacion_top3, algoritmos_variados, type = "ratings")
avg(predicciones6)




# Creacion de las variables  -------------------------------------------------------------------
options(scipen=999)
df<-df %>% mutate(fecha=as.Date(df$transaction_timestamp,format="%d-%b-%y"))
cdias<-function(from,to){
  d<-c("lunes","martes","mi?rcoles","jueves","viernes","s?bado","domingo")
  to<-as.Date(to)
  from<-as.Date(from)
  dif<-to-from
  dias<-round((dif)/7)
  drest<-dif-dias*7+1
  w<-which(d==weekdays(from))
  r<-rep(dias,7)
  for(i in 1:drest){
    if(w<=7){
      r[w]<-r[w]+1
      w<-w+1
    }else{
      w<-1
      r[w]<-r[w]+1
      w<-w+1
    }
  }
  r<-as.data.frame(cbind(dias=d,n=r))
  return(r)
}
cdias(min(df$fecha),max(df$fecha))
df<-df %>% mutate(dias=weekdays(df$fecha, abbreviate = FALSE))
table(df$dias)

# grafico de ventas en junio  ---------------------------------------------
names(df)
#Obtenemos la media de goles por partido para cada Copa del Mundo con aggregate()
evol_prod<-aggregate(qty ~ transaction_timestamp, df, FUN=sum)
#Obtenemos el promedio de goles por partido para las dos etapas descritas anteriormente (1958)
goles_x_etapa <- round(aggregate(qty ~ transaction_timestamp, evol_prod, FUN=sum)[,2],2)
evol_prod$transaction_timestamp
mean(evol_prod$qty)
#VISUALIZACI?N FINAL:
names(evol_prod)
ggplot(evol_prod, aes(x=transaction_timestamp, y=qty)) + #Datos: evol_goles, con el a?o en el eje X y los promedios en el Y
  geom_point(size=2) + geom_line() + #Comandos para poner los puntos y la l?nea
  stat_smooth(method="lm", se=F, formula= y ~ 1, aes(col=transaction_timestamp))+#A?adimos una l?nea que indique la media de goles en las dos etapas
  theme_fivethirtyeight(base_size = 14)+ #Usamos el tema de FiveThirtyEight para el gr?fico
  scale_colour_manual(values = fivethirtyeight_pal()(2))+#Se?alamos los colores que queremos para indicar las medias por etapa
  labs(title = "Evoluci?n del promedio de productos en  junio 2019 ",
       caption = "socios y no socios")+ #Por ?ltimo, t?tulos y subt?tulos del gr?fico+
  annotate("text", x=(mean(df$transaction_timestamp)-10), y=c(60000), #Con annotate() introduciremos el texto para indicar el promedio num?rico para cada etapa
           label= paste("Media = 58745  productos"), col=fivethirtyeight_pal()(1)) 
evol_prod$transaction_timestamp


# OTROS -------------------------------------------------------------------
df %>% 
  ggplot() +
  geom_bar(aes(x = wday(fecha, label = TRUE)),fill="cornflowerblue")+
  labs(title="Productos vendidos en  junio 2019",
       subtitle="Muestra tomada 8 al 14 de Agosto",
       x="Dia de la semana",
       y="Cantidad vendida",
       caption = "Fuente: Twitter")+
  theme_minimal()
cdias(min(df$fecha),max(df$fecha))
table(df$dias)

#
#plumber_API_subsectores.R

#* @get /barras_subsector
#* @serializer png

function(usuario="") {
  usuario= userTimeline("", n = 3200)
  print(usuario[1])
}

